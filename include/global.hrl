%%%%%%%%%%%%%%%%%%%%%%
%%%    GLOBAL	   %%%
%%%%%%%%%%%%%%%%%%%%%%

%% SYSTEM SETTINGS
-define(BID_TIMEOUT, 80).
-define(STATS_P, 0.3).
-define(FILTERING, true).
-define(NODE, node()).
-define(DATA_PATH, "./data/"). %% Add the trailing "/"
-define(YEAR, 2018).

-define(ADSERVER_PATH,
	application:get_env(gateway, adserver, <<"http://localhost:2250/">>)). %% Add the trailing "/"

-define(APPLICATION, gateway).
	-define(ENV(Key), application:get_env(?APPLICATION, Key, [])).
-define(ENV(Key, Default), application:get_env(?APPLICATION, Key, Default)).



%% ADX COWBOY SETTINGS
-define(ADX03_ID, 3).
-define(ADX03_NURL, <<"&wp=${AUCTION_PRICE}">>).
-define(ADX03_SEAT, <<"244977050">>).
-define(ADX03_BILLING_ID, <<"49634885883">>).
-define(ADX03_CLICK_ESC, <<"%%CLICK_URL_ESC%%">>).
-define(ADX03_CLICK_ESC_ESC, <<"%%CLICK_URL_ESC_ESC%%">>).
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

-define(RMQ_HOST, ?ENV(rmq_host, "localhost")).


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


%%%%%%%%%%%%%%%%%%%%%%
%%%    RABBITMQ    %%%
%%%%%%%%%%%%%%%%%%%%%%


-record(subscriber, {
	name,
	exchange,
	type,
	topic,
	func,
	pool_size,
	logging
}).

-record(publisher, {
	name,
	exchange,
	topic,
	pool_size,
	logging
}).


%% RABBITMQ PUBSUB SUBSCRIBERS AND PUBLISHERS
-define(RMQ_SUBSCRIBERS, [
	#subscriber{
		name = config_bert,
		exchange = <<"config">>,
		type = pubsub,
		topic = <<"bert">>,
		logging = true,
		func = fun(P) -> gateway:save_bert_file(P) end,
		pool_size = 5},
	#subscriber{
		name = config_time,
		exchange = <<"config">>,
		type = pubsub,
		topic = <<"time">>,
		logging = true,
		func = fun(P) -> gateway:set_start_time(P) end,
		pool_size = 1}
]).
-define(RMQ_PUBLISHERS, [
	#publisher{
		name = bids_debug,
		exchange = <<"bids">>,
		logging = false,
		topic = <<"bids.debug">>,
		pool_size = 30}
]).


