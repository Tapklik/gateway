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

	Children = [VMServer, GatewayDist, TimeServer],
	RestartStrategy = {one_for_one, 10, 300},
	{ok, {RestartStrategy, Children}}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
