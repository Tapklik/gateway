-module(rmq).

-behaviour(gen_server).

-include("global.hrl").
-include("rmq.hrl").
-include("lager.hrl").
-include_lib("../lib/amqp_client/include/amqp_client.hrl").

-export([start_link/0, start_link/1, start_subscriber/1, start_publisher/1]).
-export([publish/2, publish/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {
	channel,
	subscriber,
	publisher,
	logging
}).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	start_link([]).
start_link(Args) ->
	gen_server:start_link(?MODULE, [Args], []).

start_subscriber(#subscriber{pool_size = PoolSize} = Subscriber) ->
	[supervisor:start_child(rmq_sup, [Subscriber]) || _X <- lists:seq(1, PoolSize)].

start_publisher(#publisher{name = Name, pool_size = PoolSize} = Publisher) ->
	case try_ets_lookup(rmq_publishers, Name) of
		not_found ->
			ets:insert(rmq_publishers, {Name, ok}),
			RmqPubPooler = [
				{name, Name},
				{max_count, PoolSize}, {init_count, PoolSize},
				{start_mfa, {rmq, start_link, [Publisher]}}
			],
			pooler:new_pool(RmqPubPooler);
		_ ->
			?WARN("RMQ: Publisher ~p already started. ", [Name])
	end.

publish(PublisherName, Msg) ->
	publish(PublisherName, default, Msg).
publish(PublisherName, Topic, Msg) ->
	case try_ets_lookup(rmq_publishers, PublisherName) of
		not_found ->
			?ERROR("RMQ: Error in publishing to RMQ server. Publisher pool ( ~p ) not found! ~n (Msg: ~p)",
				[PublisherName, shorten_log_output(Msg)]);
		_ ->
			try_publish(PublisherName, Topic, Msg)
	end.

%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init([Args]) ->
	process_flag(trap_exit, true),
	{ok, Connection} =
		amqp_connection:start(#amqp_params_network{
			host = ?RMQ_HOST,
			port = ?RMQ_PORT,
			username = ?RMQ_USER,
			password = ?RMQ_PASSWORD,
			virtual_host = ?RMQ_VHOST
		}),
	{ok, Channel} = amqp_connection:open_channel(Connection),
	erlang:send_after(100, self(), {init, Args}),
	{ok, #state{channel = Channel}}.

handle_call(check_health, _From, State) ->
	{reply, ok, State}.

handle_cast({publish, Topic0, Payload}, State) ->
	Channel = State#state.channel,
	#publisher{name = Name, exchange = Exchange, topic = TopicFromWorker} = State#state.publisher,
	Topic = case Topic0 of
				default -> TopicFromWorker;
				T -> T
			end,
	case amqp_channel:call(State#state.channel, #'basic.publish'{
		exchange = Exchange,
		routing_key = Topic
	}, #amqp_msg{payload = Payload}) of
		ok ->
			case State#state.logging of
				true ->
					?INFO("RMQ: Published message successfully. ~n (Name: ~p. Channel: ~p. Exchange: ~p. Msg: ~p)",
						[Name, Channel, Exchange, shorten_log_output(Payload)]);
				_ ->
					ok
			end;
		_ ->
			?ERROR("RMQ: Error in publishing to RMQ server! ~n (Name: ~p. Channel: ~p. Exchange: ~p. Msg: ~p)",
				[Name, Channel, Exchange, shorten_log_output(Payload)])
	end,
	pooler:return_member(Name, self(), ok),
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({init, #subscriber{exchange = Exchange, topic = Topic0,
	name = Name, logging = Logging} = Subscriber}, State) ->
	Topic = get_topic(Topic0),
	Channel = State#state.channel,
	Queue = generate_queue_id(Name),
	#'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{exclusive = false,
		queue = Queue, arguments = [{<<"x-message-ttl">>, signedint, ?RMQ_X_MESSAGE_TTL}]}),
	amqp_channel:call(Channel, #'queue.bind'{exchange = Exchange, routing_key = Topic,
		queue = Queue}),
	amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue,
		no_ack = true}, self()),
	?INFO("RMQ: Started subscriber: ~p ~n (Channel: ~p. Exchange: ~p. Topic: ~p)",
		[Name, Channel, Exchange, Topic]),
	{noreply, State#state{subscriber = Subscriber, logging = Logging}};

handle_info({init, #publisher{name= Name, exchange = Exchange, topic = Topic0, logging = Logging} = Publisher}, State) ->
	Topic = get_topic(Topic0),
	?INFO("RMQ: Started publisher: ~p ~n (Channel: ~p. Exchange: ~p. Topic: ~p)",
		[Name, State#state.channel, Exchange, Topic]),
	{noreply, State#state{publisher = Publisher, logging = Logging}};

handle_info(stop, State) ->
	{stop, normal, State};
handle_info({'EXIT', _, _}, State) ->
	{stop, shutdown, State};
handle_info(Msg, State) ->
	case Msg of
		#'basic.consume_ok'{} ->
			ok;
		#'basic.cancel_ok'{} ->
			ok;
		{#'basic.deliver'{}, #amqp_msg{payload = Payload}} ->
			#subscriber{name = Name, exchange = Exchange, func = Fun} = State#state.subscriber,
			Channel = State#state.channel,
			case State#state.logging of
				true ->
					?INFO("RMQ: Received message! ~n (Name: ~p. Channel: ~p. Exchange: ~p. Msg: ~p)",
						[Name, Channel, Exchange, shorten_log_output(Payload)]);
				_ ->
					ok
			end,
			case Fun(Payload) of
				{ok, _} -> ok;
				E ->
					?ERROR("RMQ: Error in completing fun [~p] with error: ~p. ~n (Name: ~p. Channel: ~p. Exchange: ~p. Msg: ~p)",
						[Fun, E, Name, Channel, Exchange, shorten_log_output(Payload)])
			end
	end,
	{noreply, State}.

terminate(_Reason, State) ->
	amqp_connection:close(State#state.channel).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

try_publish(PublisherName, Topic, Msg) ->
	try_publish(PublisherName, Topic, Msg, 1).

try_publish(PublisherName, Topic, Msg, Count) ->
	NewCount = Count + 1,
	timer:sleep(20),
	Worker = pooler:take_member(PublisherName),
	case Worker of
		error_no_members when NewCount > 3 ->
			?ERROR("POOLER (~p): No members available! Increase Pool size!", [PublisherName]),
			?ERROR("RMQ: Msg ~p on publisher ~p was not published!", [shorten_log_output(Msg), PublisherName]);
		error_no_members ->
			?WARN("POOLER (~p): No members available! Retrying [~p/3]... ", [PublisherName, NewCount]),
			try_publish(PublisherName, Topic, Msg, NewCount);
		W ->
			gen_server:cast(W, {publish, Topic, Msg})
	end.

get_topic(Topic0) ->
	BidderId = list_to_binary(?ENV(app_id, "")),
	binary:replace(Topic0, <<"{id}">>, BidderId).

generate_queue_id(Name) ->
	case try_ets_lookup(rmq_queues, Name) of
		not_found ->
			Queue = generate_binray_uid(),
			ets:insert(rmq_queues, {Name, Queue}),
			Queue;
		Val -> Val
	end.

%% @hidden
try_ets_lookup(Table, Key) ->
	try_ets_lookup(Table, Key, not_found).
try_ets_lookup(Table, Key, Default) ->
	case ets:lookup(Table, Key) of
		[{_, Val} | _] -> Val;
		[] -> Default
	end.

generate_binray_uid() ->
	<<A:32, _B:16, _C:16, _D:16, _E:48>> = crypto:strong_rand_bytes(16),
	Str = io_lib:format("~8.16.0b", [A]),
	list_to_binary(Str).

shorten_log_output(Output) ->
	case byte_size(Output) of
		X when X > ?LOGGER_OUTPUT_MAX_LENGTH ->
			Bin = binary:part(Output, 0, ?LOGGER_OUTPUT_MAX_LENGTH),
			<<Bin/binary, "...">>;
		_ ->
			Output
	end.