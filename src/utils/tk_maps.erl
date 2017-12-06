%%%-------------------------------------------------------------------
%%% @author halid
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Jul 2015 4:56 PM
%%%-------------------------------------------------------------------
-module(tk_maps).
-author("halid").

%% API

-export([
    put/3, putf/1,
    get/2, getf/1,
    get/3, getf/2,
    update/3, updatef/1,
    remove/2, removef/1,
    keys/2, keysf/1,
    append/3
]).

get(Path, Map) ->
    GetFun = getf(Path),
    GetFun(Map).

getf(Path) ->
    fun(Map) ->
        getf_internal(Path, Map) end.

getf_internal([Key | _PathRest], Map) when is_list(Key)->
    lists:foldl(
        fun(Map2, Acc) ->
            Acc ++ [getf_internal(Key, Map2)]
        end
        ,[], Map);
getf_internal([Key | PathRest], Map) ->
    try maps:get(Key, Map) of
        Value ->
            getf_internal(PathRest, Value)
    catch
        _:_ ->
            {error, bad_key}
    end;
getf_internal([], Value) ->
    Value.

get(Path, Map, Default) ->
    GetFun = getf(Path, Default),
    GetFun(Map).

getf(Path, Default) ->
    fun(Map) ->
        getf_internal(Path, Map, Default) end.

getf_internal([Key | PathRest], Map, Default) ->
    try maps:get(Key, Map, Default) of
        Value ->
            getf_internal(PathRest, Value, Default)
    catch
        _:_ ->
            {error, bad_key}
    end;
getf_internal([], Value, _) ->
    Value.

update(Path, ValueOrFun, Map) ->
    SetFun = updatef(Path),
    SetFun(ValueOrFun, Map).

updatef(Path) ->
    fun(ValueOrFun, Map) ->
        try updatef_internal(Path, ValueOrFun, Map)
        catch
            error:{error, {no_map, PathRest, Element}} ->
                PathLength = length(Path) - length(PathRest),
                PathToThrow = lists:sublist(Path, PathLength),
                erlang:error({no_map, PathToThrow, Element})
        end
    end.

updatef_internal([Key | PathRest], ValueOrFun, Map) when is_map(Map) ->
    maps:update(Key, updatef_internal(PathRest, ValueOrFun, maps:get(Key, Map)), Map);
updatef_internal([], Fun, OldValue) when is_function(Fun) ->
    Fun(OldValue);
updatef_internal([], Value, _) ->
    Value;
updatef_internal(Path, _, Element) ->
    erlang:error({error, {no_map, Path, Element}}).


put(Path, Value, Map) ->
    PutFun = putf(Path),
    PutFun(Value, Map).

putf(Path) ->
    fun(Value, Map) ->
        putf_internal(Path, Value, Map) end.

putf_internal([Key | PathRest], Value, Map) ->
    SubMap =
        case maps:is_key(Key, Map) andalso is_map(maps:get(Key, Map)) of
            true ->
                maps:get(Key, Map);
            false ->
                #{}
        end,
    maps:put(Key, putf_internal(PathRest, Value, SubMap), Map);
putf_internal([], Value, _) ->
    Value.

remove(Path, Map) ->
    RemoveFun = removef(Path),
    RemoveFun(Map).

removef(Path) ->
    fun(Map) ->
        removef_internal(Path, Map) end.

removef_internal([], _) ->
    throw({bad_path, []});
removef_internal([LastKey], Map) ->
    maps:remove(LastKey, Map);
removef_internal([Key | PathRest], Map) ->
    case maps:is_key(Key, Map) of
        true ->
            maps:put(Key, removef_internal(PathRest, maps:get(Key, Map)), Map);
        false ->
            Map
    end.

keys(Path, Map) ->
    KeysFun = keysf(Path),
    KeysFun(Map).

keysf(Path) ->
    fun(Map) ->
        keysf_internal(Path, Map) end.

keysf_internal([Key | PathRest], Map) ->
    keysf_internal(PathRest, maps:get(Key, Map));
keysf_internal([], Map) ->
    maps:keys(Map).

append(Path, Value, Map) ->
    AppendFun =
        fun(List) when is_list(List) ->
            List ++ [Value];
            (_) ->
                error(no_list)
        end,
    update(Path, AppendFun, Map).