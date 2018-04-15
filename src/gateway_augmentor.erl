-module(gateway_augmentor).

-include("global.hrl").
-include("lager.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([augment_br/1]).


%% TYPES
-type br() :: #{binary() => any()}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

-spec augment_br(br()) -> br().
augment_br(BR0) ->
	BR0#{
		<<"exchange">> => ?ADX03_ID,
		<<"hourofweek">> => calc_hour_of_week(),
		<<"bid_type">> => ?ADX03_BID_TYPE
	}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    PRIVATE 	   %%%
%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

calc_hour_of_week() ->
	T = time_server:get_timestamp(),
	{Today, {H, _M, _S}} = time_server:timestamp_to_datetime(T),
	24 * (calendar:day_of_the_week(Today) - 1) + H + 1.