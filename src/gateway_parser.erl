-module(gateway_parser).

-include("gateway_global.hrl").
-include("lager.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("gateway_parser.hrl").

-export([parse_br/1, parse_rsp/4]).

-export([
	parse_geo/2,
	parse_device/2,
	parse_imp/2,
	parse_user/2,
	parse_cat/2
]).

-export([
	find/3,
	find_either/3
]).

%% TYPES
-type br() :: #{binary() => any()}.

-type rsp() :: #{binary() => any()}.

-type map_path() :: [binary()] | binary().

-type imp() :: #{binary() => any()}.

-export_type([br/0, rsp/0, imp/0, map_path/0]).

%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

-spec parse_br(br()) -> invalid_br | br().
parse_br(BR0) ->
	parse_br(openrtb_parser(), BR0, #{}).
parse_br(P, _, invalid_br) -> tk_lib:echo1(rrrr, P), invalid_br;
parse_br([], _BR, BRmap) -> BRmap;
parse_br([{Parameter, Fun, Args} | T], BR, BRmap) ->
	%% Fun can be in the form of: fun or {fun, default}
	Function = case Fun of
				   {F, Default} -> ?MODULE:F(Args, BR, Default);
				   F -> ?MODULE:F(Args, BR)
			   end,
	BRmap2 = case Function of
				 invalid -> invalid_br;
				 ignore -> BRmap;
				 {kv, Key, Value} -> maps:put(Key, Value, BRmap);
				 Value -> maps:put(Parameter, Value, BRmap)
			 end,
	parse_br(T, BR, BRmap2).

-spec parse_rsp(binary(), binary(), rsp(), integer()) -> rsp().
parse_rsp(Exchange, BidId, RSP0, TimeStamp) -> tk_lib:echo1(rsp_unparsed, RSP0),
	Cmp = tk_maps:get([<<"cid">>], RSP0),
	Crid = tk_maps:get([<<"creative">>, <<"crid">>], RSP0),
	case try_ets_lookup(exchanges, Exchange, not_found) of
		not_found ->
			invalid_rsp;
		{_, Nurl0} ->
			TsBinary = integer_to_binary(TimeStamp),
			TestMode = tk_maps:get([<<"test">>], RSP0),
			BidderAttr = case TestMode of
							 1 ->
								 <<
									 "bidid=", BidId/binary,
									 "&c=", Cmp/binary,
									 "&cr=", Crid/binary,
									 "&ts=", TsBinary/binary,
									 "&x=", Exchange/binary,
									 "&test=1"
								 >>;
							  _->
								 <<
									 "bidid=", BidId/binary,
									 "&c=", Cmp/binary,
									 "&cr=", Crid/binary,
									 "&x=", Exchange/binary,
									 "&ts=", TsBinary/binary
								 >>
						 end,
			NurlPath = list_to_binary(?NURL_PATH),
			Nurl1 = <<NurlPath/binary, BidderAttr/binary>>,
			Nurl2 = binary:replace(Nurl0, <<"{{nurl_path}}">>, Nurl1),
			Adm1 = tk_maps:get([<<"creative">>, <<"adm">>], RSP0), tk_lib:echo1(rsp, RSP0),
			Adm2 = binary:replace(Adm1, <<"{{BIDDER_ATTR}}">>, BidderAttr), tk_lib:echo1(adm, Adm2),
			ImpId = tk_maps:get([<<"creative">>, <<"impid">>], RSP0),

			%% RESPONSE
			#{
				<<"id">> => tk_maps:get([<<"id">>],RSP0),
				<<"bidid">> => BidId,
				<<"seatbid">> => [
					#{
						<<"bid">> => [
							#{
								<<"id">> => BidId,
								<<"impid">> => ImpId,
								<<"price">> => tk_maps:get([<<"price">>], RSP0),
								<<"adid">> => Crid,
								<<"adm">> => Adm2,
								<<"adomain">> => [tk_maps:get([<<"creative">>, <<"adomain">>], RSP0)],
								<<"cid">> => ?ADX03_BILLING_ID,
								<<"crid">> => Crid,
								<<"burl">> => Nurl2
							}
						],
						<<"seat">> => ?ADX03_SEAT
					}
				],
				<<"cur">> => <<"USD">>
			}
	end.

%%%%%%%%%%%%%%%%%%%%%%
%%%    PRIVATE 	   %%%
%%%%%%%%%%%%%%%%%%%%%%

-spec parse_geo(map_path(), br()) -> invalid | {binary(), binary(), binary(), binary()}.
parse_geo(Path, BR) ->
	Geo = tk_maps:get(Path, BR),
	case find([<<"country">>], Geo, invalid) of
		invalid -> invalid;
		Country ->
			Region = find([<<"region">>], Geo, <<"">>),
			RegionName = case Region of
							 <<"">> -> <<"">>;
							 R -> try_ets_lookup(regions, <<Country/binary, "-", R/binary>>)
						 end,
			City = find([<<"city">>], Geo, <<"">>),
			#{
				<<"country">> => Country,
				<<"region">> => Region,
				<<"regionname">> => RegionName,
				<<"city">> => City
			}
	end.

-spec parse_device(map_path(), br()) -> ignore | {binary(), binary(), binary(), binary(), binary()}.
parse_device(Path, BR) ->
	case find(Path, BR, ignore) of
		ignore ->
			#{
				<<"type">> => [2,4,5]
			};
		Device ->
			Type = find([<<"devicetype">>], Device, [2,4,5]),
			Model = find([<<"model">>], Device, <<"">>),
			Make = find([<<"make">>], Device, <<"">>),
			Os0 = find([<<"os">>], Device, <<"">>),
			Ua0 = find([<<"ua">>], Device, <<"">>),
			Ua1 = list_binary_match(Ua0, ?DEVICE_UA, <<"">>),
			Os1 = case lists:member(Os0, ?DEVICE_OS) of
					 true -> Os0;
					 false -> <<"">>
				  end,
			#{
				<<"type">> => Type,
				<<"make">> => Make,
				<<"model">> => Model,
				<<"os">> => Os1,
				<<"ua">> => Ua1
			}
	end.

-spec parse_imp(map_path(), br()) -> invalid | imp().
parse_imp(Path, BR) ->
	[Imp | _] = tk_maps:get(Path, BR),
	{kv, Key, Value} = find_either([
		{banner, [<<"banner">>]}, {video, [<<"video">>]}, {native, [<<"native">>]}
	], Imp, invalid),
	case parse_imp2(Key, Value) of
		invalid -> invalid;
		ParsedImp1 ->
			Bidfloor = find(<<"bidfloor">>, Imp, 0.0),
			Instl = find(<<"instl">>, Value, 0),
			ImpId = find(<<"id">>, Value, <<"1">>),
			ParsedImp1#{
				<<"bidfloor">> => Bidfloor,
				<<"instl">> => Instl,
				<<"impid">> => ImpId
			}
	end.

-spec parse_user(map_path(), br()) -> {binary(), integer()}.
parse_user(Path, BR) ->
	case tk_maps:get(Path, BR, undefined) of
		undefined -> ignore;
		User ->
			Gender = case find([<<"gender">>], User, undefined) of
						 undefined -> [<<"F">>, <<"M">>];
						 <<"O">> -> [<<"F">>, <<"M">>];
						 G -> [G]
					 end,
			Age = ?YEAR - find([<<"yob">>], User, 0),
			UserMap = #{
				<<"age">> => Age,
				<<"gender">> => Gender
			},
			case find_either([
				{<<"id">>, [<<"id">>]}, {<<"buyerid">>, [<<"buyerid">>]}
			], User, ignore) of
				{kv, _, Id2} ->
					UserMap#{
						<<"id">> => Id2
					};
				_ -> ok
			end,
			UserMap
	end.

-spec parse_cat(map_path() | [map_path()], br()) -> list().
parse_cat([H | _] = Path, BR) when is_binary(H) ->
	Cat1 = find(Path, BR, []),
	Cat2 = [get_cat_root(Cat) || Cat <- Cat1],
	lists:usort(lists:merge(Cat1, Cat2));
parse_cat([H | _] = Paths, BR) when is_list(H) ->
	Cat1 = lists:flatten([find(Path, BR, []) || Path <- Paths]),
	Cat2 = [get_cat_root(Cat) || Cat <- Cat1],
	lists:usort(lists:merge(Cat1, Cat2)).

%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

-spec find(map_path(), map(), any()) -> Default :: any() | any().
find(Path, Map, Default) when is_list(Path) == false ->
	find([Path], Map, Default);
find(Path, Map, Default) ->
	case tk_maps:get(Path, Map) of
		{error, _} -> Default;
		Value -> Value
	end.

-spec find_either([{any(), map_path()}], map(), any()) -> any() | {kv, any(), any()}.
find_either([], _Map, Default) ->
	Default;
find_either([{Key, Path} | T], Map, Default) ->
	case tk_maps:get(Path, Map) of
		{error, _} -> find_either(T, Map, Default);
		Value -> {kv, Key, Value}
	end.

%% @hidden
try_ets_lookup(Table, Key) ->
	try_ets_lookup(Table, Key, <<"">>).
try_ets_lookup(Table, Key, Default) ->
	case ets:lookup(Table, Key) of
		[{_, Val} | _] -> Val;
		[] -> Default
	end.

%% @hidden
try_ets_select(Table, Fun) ->
	try_ets_select(Table, Fun, <<"">>).
try_ets_select(Table, Fun, Default) ->
	case ets:select(Table, Fun) of
		[Val | _] -> Val;
		[] -> Default
	end.

%% @hidden
parse_imp2(banner, Value) ->
	W1 = find(<<"w">>, Value, 0),
	H1 = find(<<"h">>, Value, 0),
	Dims1 = set_creative_dimensions(W1, H1),
	Formats = find(<<"format">>, Value, []),
	Dims2 = lists:usort(Dims1 ++ parse_formats(Formats)),
	Btype = find(<<"btype">>, Value, []),
	Battr = find(<<"battr">>, Value, []),
	Pos = find(<<"pos">>, Value, 0),
	Expdir = find(<<"expdir">>, Value, 0),
	#{
		<<"class">> => <<"banner">>,
		<<"dim">> => Dims2,
		<<"battr">> => Battr,
		<<"btype">> => Btype,
		<<"pos">> => Pos,
		<<"expdir">> => Expdir
	};
parse_imp2(_, _) ->
	invalid.

%% @hidden
parse_formats(Formats) ->
	parse_formats(Formats, []).
parse_formats([], Acc) ->
	Acc;
parse_formats([Format | T], Acc) ->
	W2 = find(<<"w">>, Format, 0),
	H2 = find(<<"h">>, Format, 0),
	parse_formats(T, Acc ++ set_creative_dimensions(W2, H2)).


%% @hidden
list_binary_match(Subject, ListOfPatterns, Default) ->
	[Pattern | T] = ListOfPatterns,
	case binary:match(Subject, Pattern) of
		nomatch when T == [] -> Default;
		nomatch -> list_binary_match(Subject, T, Default);
		_ -> Pattern
	end.

%% @hidden
set_creative_dimensions(0, 0) ->
	[];
set_creative_dimensions(W, H) ->
	Wbin = integer_to_binary(W),
	Hbin = integer_to_binary(H),
	DimBin = <<Wbin/binary, "x", Hbin/binary>>,
	[DimBin].

%% @hidden
get_cat_root(Cat) ->
	[C | _] = binary:split(Cat, <<"-">>),
	C.

%% @hidden
openrtb_parser() ->
	[
		{<<"id">>, {find, invalid}, [<<"id">>]},
		{<<"test">>, {find, 0}, [<<"test">>]},
		{<<"geo">>, parse_geo, [<<"device">>, <<"geo">>]},
		{<<"bcat">>, parse_cat, [<<"bcat">>]},
		{<<"cat">>, parse_cat, [[<<"app">>, <<"cat">>], [<<"site">>, <<"cat">>]]},
		{<<"ip">>, {find, <<"">>}, [<<"device">>, <<"ip">>]},
		{<<"badv">>, {find, []}, [<<"badv">>]},
		{<<"bapp">>, {find, []}, [<<"bapp">>]},
		{<<"device">>, parse_device, [<<"device">>]},
		{<<"wlang">>, {find, []}, [<<"wlang">>]},
		{<<"wseat">>, {find, []}, [<<"wseat">>]},
		{<<"bseat">>, {find, []}, [<<"bseat">>]},
		{<<"allimps">>, {find, 0}, [<<"allimps">>]},
		{<<"language">>, {find, <<"">>}, [<<"device">>, <<"language">>]},
		{<<"imp">>, parse_imp, [<<"imp">>]},
		{<<"user">>, parse_user, [<<"user">>]},
		{<<"">>, {find_either, []}, [{<<"app">>, [<<"app">>]}, {<<"site">>, [<<"site">>]}]}
	].
