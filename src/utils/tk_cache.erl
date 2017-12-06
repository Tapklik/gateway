%%%-------------------------------------------------------------------
%%% @author halid
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jul 2015 1:21 PM
%%%-------------------------------------------------------------------
-module(tk_cache).
-author("halid").



%% API
-export([create_table/1, create_table/2, delete_table/1]).

-export([insert/3, read/2, update/4, delete/2]).



create_table(Tab) ->
    cache:start_link(Tab, [{n, 3},{ttl,1000}]).

create_table(Tab,Opts) ->
	cache:start_link(Tab, Opts).

delete_table(Tab) ->
	delete_table2(whereis(Tab)).
delete_table2(undefined) ->
	ok;
delete_table2(Tab) ->
	cache:drop(Tab).

insert(Tab, K, V) ->
	cache:put(Tab,K,V).

read(Tab, K) ->
    cache:get(Tab,K).

update(Tab, K, V, add) ->
	V0 = read(Tab, K),
	case is_list(V0) of
		true ->
			V1 = V0 ++ [V],
			update(Tab, K, V0, V1)
	end;
update(Tab, K, V, remove) ->
	V0 = read(Tab, K),
	case is_list(V0) of
		true ->
			V1 = V0 -- [V],
			update(Tab, K, V0, V1)
	end;
update(Tab, K, _V0, V1)->
	delete(Tab,K),
	insert(Tab, K, V1).

delete(Tab, K) ->
    cache:delete(Tab,K).

