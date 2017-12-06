-module(rmq_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	%% start a table to monitor RMQ queues -- will be used by supervisor children
	ets:new(rmq_queues, [named_table, public]),
	%% start a table to for publishers -- will be used by supervisor children
	ets:new(rmq_publishers, [named_table, public]),
	Child = #{
		id => rmq,
		start => {rmq, start_link, []},
		restart => transient,
		shutdown => infinity,
		type => worker,
		modules => [rmq]
	},
	Children = [Child],
	RestartStrategy = {simple_one_for_one, 10, 300},
	{ok, {RestartStrategy, Children}}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
