%%%%%%%%%%%%%%%%%%%%%%
%%%    GLOBAL	   %%%
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

