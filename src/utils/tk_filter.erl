-module(tk_filter).
-compile([native]).

-export([
	evaluate/2
]).

-define(NULL, undefined).

%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

evaluate({'and', A, B}, Vars) ->
	evaluate(A, Vars) andalso evaluate(B, Vars);
evaluate({'or', A, B}, Vars) ->
	evaluate(A, Vars) orelse evaluate(B, Vars);
evaluate({comp, Comparator, Var, Value}, Vars) ->
	compare(Comparator, lookup(Var, Vars), Value);
evaluate({in, Var, List}, Vars) ->
	lists:member(lookup(Var, Vars), List);
evaluate({notin, Var, List}, Vars) ->
	not lists:member(lookup(Var, Vars), List);
evaluate({in_var, Item, Var}, Vars) ->
	lists:member(Item, lookup(Var, Vars));
evaluate({notin_var, Item, Var}, Vars) ->
	not lists:member(Item, lookup(Var, Vars));
evaluate({null, Var}, Vars) ->
	lookup(Var, Vars) =:= ?NULL;
evaluate({notnull, Var}, Vars) ->
	 lookup(Var, Vars) =/= ?NULL.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
compare(_Comp, undefined, _B) ->
	false;
compare('<', A, B) when is_number(A) and
	is_number(B) ->
	A < B;
compare('<=', A, B) when is_number(A) and
	is_number(B) ->
	A =< B;
compare('=', A, B) when is_number(A) or
	is_binary(A) and
		is_number(B) or
	is_binary(B) ->
	A == B;
compare('>=', A, B) when is_number(A) and
	is_number(B) ->
	A >= B;
compare('>', A, B) when is_number(A) and
	is_number(B) ->
	A > B;
compare('<>', A, B) when is_number(A) or
	is_binary(A) and
		is_number(B) or
	is_binary(B) ->
	A /= B.

lookup(Key, List) ->
	lookup(Key, List, undefined).
lookup(Key, List, Default) ->
	case lists:keyfind(Key, 1, List) of
		false -> Default;
		{_, Value} -> Value
	end.