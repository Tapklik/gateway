-module(gateway_dist).

-behaviour(gen_server).

-include("global.hrl").
-include("lager.hrl").


-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).


-record(state, {
	s_group = {}
}).

-define(TIMEOUT, 5000).
-define(S_GROUP_TIMEOUT, 10000).



%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	gen_server:start_link(?MODULE, [], []).


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	ets:new(bidder_workers, [named_table, set, public, {read_concurrency, true}]),
	ets:insert(bidder_workers, {bidders, []}),
	erlang:send_after(?TIMEOUT, self(), timeout),
	erlang:send_after(?S_GROUP_TIMEOUT, self(), update_s_group),
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.


handle_info(timeout, State) ->
	S_Group = State#state.s_group,
	update_bidders_ets(S_Group),
	erlang:send_after(?TIMEOUT, self(), timeout),
	{noreply, State};
handle_info(update_s_group, State) ->
	State2 = State#state{s_group = find_bidders_s_group()},
	erlang:send_after(?S_GROUP_TIMEOUT, self(), update_s_group),
	{noreply, State2};
handle_info(stop, State) ->
	{stop, normal, State};
handle_info({'EXIT', _, _}, State) ->
	{stop, shutdown, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

update_bidders_ets({}) ->
	?WARN("GATEWAY: Doesn't belong to any S-Group", []);
update_bidders_ets(S_Group) ->
	[{_, BiddersETS}] = ets:lookup(bidder_workers, bidders),
	Bidders = [{Name, s_group:whereis_name(S_Group, {bidder,Name})} ||
		{bidder, Name} <- find_bidder_pids(S_Group)],
	case Bidders == BiddersETS of
		true when Bidders == [] ->
			?WARN("GATEWAY: No bidder workers declared and found", []);
		true->
			ok;
		false->
			ets:insert(bidder_workers, {bidders, Bidders}),
			?INFO("GATEWAY: ETS Table (bidder_workers) is updated with the following list of workers: ~p"
				, [Bidders])
	end.

find_bidder_pids(S_Group) ->
	[{bidder, B} || {_, {bidder, B}} <- s_group:registered_names({s_group, S_Group})].

find_bidders_s_group() ->
	S_Groups = [{bidders, SG} || {{bidders, SG}, _} <- s_group:own_s_groups()],
	case S_Groups of
		[] -> {};
		[H | _] -> H
	end.