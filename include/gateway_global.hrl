%%%%%%%%%%%%%%%%%%%%%%
%%%    GLOBAL	   %%%
%%%%%%%%%%%%%%%%%%%%%%

%% SYSTEM SETTINGS
-define(BID_TIMEOUT, 80).
-define(STATS_P, 0.3).
-define(FILTERING, true).
-define(NODE, node()).
-define(DATA_PATH, "./data/"). %% Add the trailing "/"
-define(YEAR, 2017).

-define(NURL_PATH,
	application:get_env(gateway, nurl_host, "http://localhost:2250") ++ "/sad23ref34578hj/wins?"). %% Add the trailing "/"

-define(APPLICATION, gateway).
	-define(ENV(Key), application:get_env(?APPLICATION, Key, [])).
-define(ENV(Key, Default), application:get_env(?APPLICATION, Key, Default)).



%% ADX COWBOY SETTINGS
-define(ADX03_ID, <<"3">>).
-define(ADX03_SEAT, <<"244977050">>).
-define(ADX03_BILLING_ID, <<"8420-5895-2715">>).
-define(ADX03_RSP_HEADERS,
	#{
		<<"content-type">> => <<"application/json">>,
		<<"x-openrtb-version">> => <<"2.5">>,
		<<"charset">> => <<"utf-8">>
	}
).

-define(ADX03_BID_TYPE, <<"variance">>).
-define(ADX03_GW_ACCEPTORS, 2000).
-define(ADX03_GW_PORT, 2234).
-define(ADX03_MAX_KEEPALIVE, 1000).

-define(COWBOY_API_GW_ACCEPTORS, 20).
-define(COWBOY_API_GW_PORT, 2302).



%% tkb config
%% ===========================================
%% POOLER SETTINGS
-define(BIDS_POOLER_INIT_COUNT, 1000).
-define(BIDS_POOLER_MAX_COUNT, 2000).


-ifdef(debug).
-warning("Debug mode is ON!").
-define(DEBUG_BIDS_RATE, 1.0).
-else.
-define(DEBUG_BIDS_RATE, 0.0).
-endif.

