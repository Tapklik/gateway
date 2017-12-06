-module(bidrequest_parse_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([
	all/0, groups/0,
	init_per_suite/1, end_per_suite/1,
	init_per_group/2, end_per_group/2
]).
-export([
	eunit_data/1,
	eunit_br/1
]).


-define(BR1, bidrequest_helper:bid_request_1()).


all() -> [
	{group, br1}
].

groups() -> [
	{br1, [], [
		eunit_data,
		eunit_br
	]}
].

init_per_suite(Config) ->
	Pid = spawn(fun ets_owner_proscess/0),
	Tables = [
		{regions, bidrequest_helper:regions()},
		{banner_sizes, bidrequest_helper:banner_sizes()},
		{users, bidrequest_helper:users()}
	],
	lists:foreach(
		fun({Name, Data}) ->
			ets:new(Name, [named_table, set, public, {read_concurrency, true}, {heir, Pid, []}]),
			ets:insert(Name, Data)
		end
		, Tables),
	Config.

end_per_suite(Config) ->
	Config.

init_per_group(br1, Config) ->
	BR = jsx:decode(?BR1, [return_maps]),
	Config ++ [{br, BR}].

end_per_group(br1, Config) ->
	lists:delete([br], Config).


%%%%%%%%%%%%%%%%%%%%%%
%%%    TESTS	   %%%
%%%%%%%%%%%%%%%%%%%%%%

eunit_data(_Config) ->
	?assertEqual(ets:lookup(regions, <<"ARE-FU">>), [{<<"ARE-FU">>, <<"Fujairah">>}]).


eunit_br(Config) ->
	BR = ?config(br, Config),
	Parsed = gateway_parser:parse_br(BR),
	ct:print("TEST: ~p", [Parsed]),
	?assertEqual(maps:get(<<"geo">>, Parsed), {<<"SAU">>, <<"01">>, <<"Riyadh">>, <<"Riyadh">>}),
	?assertEqual(maps:get(<<"id">>, Parsed), <<"BR-73499275800816">>),
	?assertEqual(maps:get(<<"bcat">>, Parsed), [<<"IAB24">>]),
	?assertEqual(maps:get(<<"cat">>, Parsed), [<<"IAB1">>, <<"IAB1-6">>]),
	?assertEqual(maps:get(<<"badv">>, Parsed), [<<"bmw.com">>]),
	?assertEqual(maps:get(<<"ip">>, Parsed), <<"95.246.223.14">>),
	?assertEqual(maps:get(<<"language">>, Parsed), <<"en">>),
	?assertEqual(maps:get(<<"device">>, Parsed), {4, <<"">>, <<"Apple">>, <<"iOS">>, <<"Mozilla/5.0">>}),
	?assertEqual(maps:get(<<"imp">>, Parsed),
		#{
			<<"type">> => <<"banner">>,
			<<"dim">> => [1052, 1053],
			<<"blocked">> => {[1, 3], []},
			<<"pos">> => 1,
			<<"expdir">> => 0,
			<<"bidfloor">> => 2.35
		}),
	?assertEqual(maps:get(<<"user">>, Parsed), 21),
	?assertEqual(maps:get(<<"userid">>, Parsed), <<"USR-79040041481540">>),
	?assertEqual(maps:get(<<"site">>, Parsed),
		#{
			<<"id">> => <<"WW-47262948763574">>,
			<<"domain">> => <<"www.sm3na.com">>,
			<<"cat">> => [<<"IAB1">>, <<"IAB1-6">>]
		}
	).


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL	   %%%
%%%%%%%%%%%%%%%%%%%%%%

ets_owner_proscess() ->
	receive
		stop -> exit(normal);
		_Any -> ets_owner_proscess()
	end.