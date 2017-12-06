-module(vm).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([check_health/0]).

-export([s_group_new/2, s_group_delete/1, s_group_add_nodes/2, s_group_remove_nodes/2,
	s_group_info/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).


-define(INTERVAL, 20000).

-ifdef(local).
-define(LOCAL, true).
-else.
-define(LOCAL, false).
-endif.

-record(state, {
	prev_io,
	prev_gc
}).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%% Started by the main sup. Responsible for periodically collecting VM
%% 	metrics and sending them to Statsderl. As well as the periodic heartbeat
%%
start_link() ->
	start_link([]).
start_link(_Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

check_health() ->
	gen_server:call(?MODULE, {check_health}, 500).

s_group_new(SGroup, Nodes) ->
	s_group:new_s_group(SGroup, Nodes).

s_group_delete(SGroup) -> tk_lib:echo1(hey,SGroup),
	s_group:delete_s_group(SGroup).

s_group_add_nodes(SGroup, Nodes) ->
	s_group:add_nodes(SGroup, Nodes).

s_group_remove_nodes(SGroup, Nodes) ->
	s_group:remove_nodes(SGroup, Nodes).

s_group_info() ->
	s_group:sync(),
	SGroupInfo = s_group:info(),
	NoContact = proplists:get_value(no_contact, SGroupInfo),
	Synced = proplists:get_value(synced_nodes, SGroupInfo),
	SyncError = proplists:get_value(sync_error, SGroupInfo),
	ErrorNodes = SyncError ++ NoContact -- Synced,
	OwnGroups = s_group:own_s_groups(),
	lists:map(
		fun({SG, Nodes}) ->
			s_group:remove_nodes(SG, ErrorNodes),
			{SG, Nodes -- ErrorNodes}
		end
		, OwnGroups).

%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

%% @private
init(_Args) ->
	erlang:send_after(?INTERVAL, self(), {interval}),
	{{input, In}, {output, Out}} = erlang:statistics(io),
	State = #state{
		prev_io = {In, Out},
		prev_gc = erlang:statistics(garbage_collection)
	},
	{ok, State}.

%% @private
handle_call({check_health}, _From, State) ->
	Services = [],
	{reply, {ok, <<"running">>, Services}, State};
handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

%% @private
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @private
handle_info({interval}, State) ->
	{_, NewState} = vm_stats(State), %% STAT
	erlang:send_after(?INTERVAL, self(), {interval}),
	{noreply, NewState};
handle_info(stop, State) ->
	{stop, normal, State};
handle_info({'EXIT', _, _}, State) ->
	{stop, shutdown, State};
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%% @hidden
%% sends all VM metrics to statsderl
%%
vm_stats(State) ->
	[Node, Host] = binary:split(atom_to_binary(node(), latin1), <<"@">>),
	K = "erlang." ++ binary_to_list(Node) ++ "__" ++ binary_to_list(binary:replace(Host, <<".">>, <<"_">>)) ++ ".",

	%% Processes
	statsderl:gauge([K, "vm.proc_count"], erlang:system_info(process_count), 1.00),
	statsderl:gauge([K, "vm.proc_limit"], erlang:system_info(process_limit), 1.00),

	%% Messages in queues
	TotalMessages = lists:foldl(
		fun(Pid, Acc) ->
			case process_info(Pid, message_queue_len) of
				undefined -> Acc;
				{message_queue_len, Count} -> Count + Acc
			end
		end,
		0,
		processes()
	),
	statsderl:gauge([K, "vm.messages_in_queues"], TotalMessages, 1.00),

	%% Modules loaded
	statsderl:gauge([K, "vm.modules"], length(code:all_loaded()), 1.00),

	%% Queued up processes (lower is better)
	statsderl:gauge([K, "vm.run_queue"], erlang:statistics(run_queue), 1.00),

	%% Error logger backlog (lower is better)
	{_, MQL} = process_info(whereis(error_logger), message_queue_len),
	statsderl:gauge([K, "vm.error_logger_queue_len"], MQL, 1.00),

	%% Memory usage. There are more options available, but not all were kept.
	%% Memory usage is in bytes.
	K2 = [K, "vm.memory."],
	Mem = erlang:memory(),
	statsderl:gauge([K2, "total"], proplists:get_value(total, Mem), 1.00),
	statsderl:gauge([K2, "procs_used"], proplists:get_value(processes_used, Mem), 1.00),
	statsderl:gauge([K2, "atom_used"], proplists:get_value(atom_used, Mem), 1.00),
	statsderl:gauge([K2, "binary"], proplists:get_value(binary, Mem), 1.00),
	statsderl:gauge([K2, "ets"], proplists:get_value(ets, Mem), 1.00),

	%% Incremental values
	#state{prev_io = {OldIn, OldOut}, prev_gc = {OldGCs, OldWords, _}} = State,
	{{input, In}, {output, Out}} = erlang:statistics(io),
	GC = {GCs, Words, _} = erlang:statistics(garbage_collection),

	statsderl:increment([K, "vm.io.bytes_in"], In - OldIn, 1.00),
	statsderl:increment([K, "vm.io.bytes_out"], Out - OldOut, 1.00),
	statsderl:increment([K, "vm.gc.count"], GCs - OldGCs, 1.00),
	statsderl:increment([K, "vm.gc.words_reclaimed"], Words - OldWords, 1.00),

	%% Reductions across the VM, excluding current time slice, already incremental
	{_, Reds} = erlang:statistics(reductions),
	statsderl:increment([K, "vm.reductions"], Reds, 1.00),
	{ok, State#state{prev_io = {In, Out}, prev_gc = GC}}.
