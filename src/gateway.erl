-module(gateway).

-behaviour(application).

-include("global.hrl").
-include("lager.hrl").
-include("rmq.hrl").

-export([start/0, start/2, stop/1]).
-export([save_bert_file/1]).
-export([set_start_time/1]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%% Application Gateway starts here...
%%
%% Initiation process includes:
%% - Start supervisor through start/2
%% - Load all dependencies
%% - Start cowboy routes for receiving BR from Ad Exchange
%% - Start initial pool: bids_pooler of gateway_bids_handler
%%
%% Other non-standard running applications:
%% - Bertconf: loads config .bert files stored at ./data/ to ETS tables
%%
start(_Type, _Args) ->
	gateway_sup:start_link().

start() ->
	_ = [application:start(Dep) || Dep <- resolve_deps(gateway),
		not is_otp_base_app(Dep)],
	application:start(cowboy),
	RouteSpecs1 = [{"/", gateway_http_handler, [{exchange, ?ADX03_ID}]}],
	Dispatch1 = cowboy_router:compile([
		{'_', RouteSpecs1}
	]),
	{ok, _} = cowboy:start_clear(http, ?ADX03_GW_ACCEPTORS,
		[{port, ?ADX03_GW_PORT}],
		#{env => #{dispatch => Dispatch1}}
	),
	?INFO("COWBOY: Started ~p acceptors for ADX ~p , on port ~p", [?ADX03_ID, ?ADX03_GW_ACCEPTORS, ?ADX03_GW_PORT]),
	RouteSpecs2 = [{"/api/:service", gateway_api, []}],
	Dispatch2 = cowboy_router:compile([
		{'_', RouteSpecs2}
	]),
	{ok, _} = cowboy:start_clear(rest_api, ?COWBOY_API_GW_ACCEPTORS,
		[{port, ?COWBOY_API_GW_PORT}],
		#{env => #{dispatch => Dispatch2}}
	),
	?INFO("COWBOY: Started API on port ~p", [?COWBOY_API_GW_PORT]),
	BidsPooler = [
		{name, bids_pooler},
		{max_count, ?BIDS_POOLER_MAX_COUNT}, {init_count, ?BIDS_POOLER_INIT_COUNT},
		{start_mfa, {gateway_bids_handler, start_link, []}}
	],
	%% Start RMQ Pub/Sub workers
	[rmq:start_subscriber(Subscriber) || Subscriber <- ?RMQ_SUBSCRIBERS],
	[rmq:start_publisher(Publisher) || Publisher <- ?RMQ_PUBLISHERS],
	?INFO("POOLER: Started ~p pool, with initial count of ~p and max count of ~p",
		[bids_pooler, ?BIDS_POOLER_INIT_COUNT, ?BIDS_POOLER_MAX_COUNT]),
	pooler:new_pool(BidsPooler).

%% todo fix stop
stop(_State) ->
	ok.

set_start_time(<<0>>) ->
	time_server:reset_start_time();
set_start_time(Tbin) ->
	T = msgpack:binary_to_term(Tbin),
	time_server:set_start_time(T).

save_bert_file(FileBin) ->
	[{Filename1, _Content} | _] = binary_to_term(FileBin),
	Filename2 = atom_to_list(Filename1),
	case file:write_file(?DATA_PATH ++ Filename2 ++ ".bert", FileBin) of
		ok ->
			?INFO("RMQ: File ~p is received and saved", [Filename2]),
			{ok, saved};
		{error, E} ->
			?ERROR("RMQ: Error in receiving or saving file ~p. (Error: ~p) ", [Filename2, E]),
			{error, E}
	end.

%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
dep_apps(App) ->
	application:load(App),
	{ok, Apps} = application:get_key(App, applications),
	Apps.

%% @hidden
all_deps(App, Deps) ->
	[[all_deps(Dep, [App | Deps]) || Dep <- dep_apps(App),
		not lists:member(Dep, Deps)], App].
%% @hidden
resolve_deps(App) ->
	DepList = all_deps(App, []),
	{AppOrder, _} = lists:foldl(fun(A, {List, Set}) ->
		case sets:is_element(A, Set) of
			true ->
				{List, Set};
			false ->
				{List ++ [A], sets:add_element(A, Set)}
		end
								end,
		{[], sets:new()},
		lists:flatten(DepList)),
	AppOrder.

%% @hidden
is_otp_base_app(kernel) ->
	true;
is_otp_base_app(stdlib) ->
	true;
is_otp_base_app(_) ->
	false.