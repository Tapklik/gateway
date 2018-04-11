-module(gateway_sup).

-behaviour(supervisor).

-include("global.hrl").
-include("lager.hrl").


-export([start_link/0]).
-export([init/1]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%% This is the main supervisor and the entry point
%%
%% Supervisor supervises the following:
%% - Pooler supervisor that supervises in turn gateway_bids_handler pool
%% - RabbitMQ supervisor that supervisor the RMQ subscribers
%% - Gateway_dist (for distributed) gen_server
%% - VM health gen_server
%%
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

%% @private
init([]) ->

	%% Pooler supervisor that supervises in turn gateway_bids_handler pool
	Pooler = #{
		id => pooler_sup,
		start => {pooler_sup, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [pooler_sup]
	},

	%% RabbitMQ supervisor that supervisor the RMQ subscribers
	RmqSup = #{
		id => rmq_sup,
		start => {rmq_sup, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [rmq_sup]
	},

	%% Gateway_dist (for distributed) gen_server
	GatewayDist = #{
		id => gateway_dist,
		start => {gateway_dist, start_link, []},
		restart => permanent,
		shutdown => 2000,
		type => worker,
		modules => [gateway_dist]
	},

	%% VM health gen_server
	VMServer = #{
		id => vm,
		start => {vm, start_link, []},
		restart => permanent,
		shutdown => 2000,
		type => worker,
		modules => [vm]
	},

	%% Time server gen_server
	TimeServer = #{
		id => time_server,
		start => {time_server, start_link, []},
		restart => permanent,
		shutdown => 2000,
		type => worker,
		modules => [time_server]
	},

	Children = [Pooler, RmqSup, VMServer, GatewayDist, TimeServer],
	RestartStrategy = {one_for_one, 10, 300},
	{ok, {RestartStrategy, Children}}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
