-module(gateway_bids_handler).

-include("gateway_global.hrl").
-include("lager.hrl").


-export([start_link/0]).
-export([init/1, write_debug/3, system_code_change/4, system_continue/3, system_terminate/4]).

%% TYPES
-type br() :: #{binary() => any()}.

-record(state, {
	from,
	exchange,
	bid_id,
	t1,
	debug,
	timestamp,
	statsderl_prefix
}).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%% Started new by each bid request in.
%%
%% Here we start proc_lib process instead of gen_server to lessen the common bottleneck
%% 	with gen_server.
%%
-spec start_link() -> {ok, Result :: map() | atom()} | {error, atom()}.
start_link() ->
	proc_lib:start_link(?MODULE, init, [self()], 20, [{priority, high}, {fullsweep_after, 20}]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

%% @private
init(Parent) ->
	proc_lib:init_ack(Parent, {ok, self()}),
	Debug = sys:debug_options([]),
	[Node, Host] = binary:split(atom_to_binary(node(), latin1), <<"@">>),
	K = <<"bids.gw.", Node/binary, "__", Host/binary, ".">>,
	loop(Parent, Debug, #state{statsderl_prefix = K}).

%%
%% @private
%% Receives msg from Cowboy HTTP Handler with BR {From, br, BR}. And starts
%% 	immediately a timer BID_TIMEOUT for the maximum amount of time that
%% 	bidders are allowed to take to send back a valid response.
%%
%% After that the process converts BR -> BR+ (parsing, augmenting,
%% 	adding user data, etc...).
%%
%% When BR+ is obtained the server retrieves a list of active bidder workers
%% 	on the same node or other nodes from ETS (bidder_workers), and selects
%% 	randomly to which process to send the bid in the form:
%%
%%		SEND: 	 self()	---- {From, br, BidId, BR} ----> bidder_worker
%%		RECEIVE: self() <--- {From, rsp, BidId, RSP} --- bidder_worker
%%
%% Reply is sent later so the call is not blocking
%%
loop(Parent, Debug, State) ->
	Prefix = State#state.statsderl_prefix,
	receive

		{system, From, Request} ->
			sys:handle_system_msg(
				Request, From, Parent, ?MODULE, Debug, State
			);

	%% Receiving bid request from Cowboy http handler
		{From, br, Exchange, BR} ->
			%% STAT:
			T1 = erlang:monotonic_time(),

			TimeStamp = time_server:get_timestamp(),
			BidId = generate_bidid(TimeStamp),
			erlang:send_after(?BID_TIMEOUT, self(), {self(), rsp, BidId, timeout}),

			%% Set Debug flag for this bid (if DEBUG is enabled through compile)
			DebugBid = case ?ENV(debug_bids, 0.0) of
						   0.0 -> false;
						   X ->
							   X > rand:uniform()
					   end,

			%% Parsing original BR to the internal format
			BRparsed = gateway_parser:parse_br(BR),
			%% Augmenting with additional data: BR -> BR+
			BRplus = gateway_augmentor:augment_br(BRparsed),

			%% Log bid if DebugBid = true (should be compiled with debug enabled)
			log_bid(BidId, [{<<"br">>, BR}, {<<"br_plus">>, BRplus}, {<<"time">>, time_server:get_iso_time()}], DebugBid),

			%% Retrieve list of available bidders and send BR+ to them
			[{_, BidderWorkers}] = ets:lookup(bidder_workers, bidders),
			send_bid_to_bidder(BidId, BRplus, BidderWorkers, TimeStamp, DebugBid),
			loop(Parent, Debug,
				#state{from = From, bid_id = BidId, t1 = T1, debug = DebugBid,
					exchange = Exchange, timestamp = TimeStamp, statsderl_prefix = Prefix});

	%% Receiving response from bidders; positive response is a map always
		{_From, rsp, BidId, RSPmap0} when is_map(RSPmap0) ->
			Exchange = State#state.exchange,
			DebugBid = State#state.debug,
			TimeStamp = State#state.timestamp,
			RSPmap1 = gateway_parser:parse_rsp(Exchange, BidId, RSPmap0, TimeStamp),
			RSPTime = calc_time(State#state.t1),
			%% Log bid if DebugBid = true (should be compiled with debug enabled)
			log_bid(BidId, [{<<"rsp">>, RSPmap1}, {<<"rsp_time">>, RSPTime}], DebugBid),
			statsderl:timing(<<Prefix/binary, "rsp.time.internal">>, RSPTime, ?STATS_P),
			State#state.from ! {rsp, BidId, RSPmap1};

	%% Receiving response from bidders; Other can be: invalid_br, invalid_rsp, timeout
		{_From, rsp, BidId, Other} ->
			Exchange = State#state.exchange,
			DebugBid = State#state.debug,
			RSPTime = calc_time(State#state.t1),
			%% Log bid if DebugBid = true (should be compiled with debug enabled)
			log_bid(BidId, [{<<"rsp">>, atom_to_binary(Other, latin1)}, {<<"rsp_time">>, RSPTime}], DebugBid),
			State#state.from ! {rsp, BidId, Other};

		{'EXIT', _Parent, Reason} ->
			exit(Reason)
	end.

%% @private
write_debug(Dev, Event, Name) ->
	io:format(Dev, "~p event = ~p~n", [Name, Event]).

%% @private
system_continue(Parent, Debug, State) ->
	loop(Parent, Debug, State).

%% @private
system_terminate(Reason, _Parent, _Debug, _State) ->
	exit(Reason).

%% @private
system_code_change(State, _Module, _OldVsn, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%% Send BR+ to bidder workers on other node, or send invalid_br to self()
%%
-spec send_bid_to_bidder(BidId :: binary(), BRplus :: invalid_br | br(), [{any(), pid()}], integer(),true | false) -> ok.
send_bid_to_bidder(BidId, invalid_br, _, _, _) ->
	self() ! {self(), rsp, BidId, invalid_br};
send_bid_to_bidder(_, _, [], _, _) ->
	ok;
send_bid_to_bidder(BidId, BRplus, BidderWorkers, TimeStamp, DebugBid) ->
	Index = rand:uniform(length(BidderWorkers)),
	{_, BidderWorkerPid} = lists:nth(Index, BidderWorkers),
	BidderWorkerPid ! {self(), br, BidId, BRplus, TimeStamp, DebugBid}.

%% @hidden
calc_time(T1) ->
	%% STAT: Calculate bid response time
	T2 = erlang:monotonic_time(),
	erlang:convert_time_unit(T2 - T1, native, milli_seconds).

generate_bidid(T) ->
	<<_A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
	Str = io_lib:format("~p-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b",
		[trunc(T/100000), B, C band 16#0fff, D band 16#3fff bor 16#8000, E]),
	list_to_binary(Str).

log_bid(_, _, false) ->
	ok;
log_bid(BidId, List, true) when is_list(List)->
	Bid1 = #{<<"id">> => BidId},
	Bid2 = lists:foldl(
		fun({K, V}, Acc)->
			Acc#{K => V}
		end
	, Bid1, List),
	rmq:publish(bids_debug, term_to_binary(Bid2)).

