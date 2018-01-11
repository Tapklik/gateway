%%%-------------------------------------------------------------------
%%% @author halid
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Sep 2015 1:30 PM
%%%-------------------------------------------------------------------
-module(tk_lib).
-author("halid").

%% API
-export([read_file/1]).
-export([uuid4/0]).
-export([rand/1]).
-export([echo1/2, echo2/1]).
-export([writeout/2, writeout/6]).
-export([timestamp/0]).
-export([escape_uri/1]).

-define(VARIANT10, 2#10).
-define(MAX_UNSIGNED_INT_32, 4294967296).
-define(OUTPUT_FILE, "output/file1.csv").

read_file(Fname) ->
	Content1 = case file:open(Fname, [read, raw, binary]) of
				   {ok, _} ->
					   {ok, Content2} = file:read_file(Fname),
					   Content2;
				   {error, Reason} ->
					   {error, Reason}
			   end,
	Content1.

uuid4() ->
	<<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
	Str = io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b",
		[A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E]),
	list_to_binary(Str).

rand(K) ->
	Rand = statsderl_utils:random(?MAX_UNSIGNED_INT_32),
	case Rand =< K * ?MAX_UNSIGNED_INT_32 of
		true ->
			true;
		false ->
			false
	end.



%%------------ECHO---------------------------------------
echo1(NameArg1, Arg1) ->
	io:format("~n =============== ~n ~p : ~p ~n =============== ~n",
		[NameArg1, Arg1]).

echo2(ArgList) ->
	io:format("~n ==============="),
	lists:foreach(
		fun(X) ->
			case is_tuple(X) of
				true ->
					{NameArg, Arg} = X;
				_ ->
					Arg = X, NameArg = var
			end,
			io:format("~n ~p : ~p ~n -----",
				[NameArg, Arg])
		end
		, ArgList),
	io:format("~n =============== ~n ~n").

%%------------WRITE TO FILE------------------------------
writeout(A1, A2) ->
	file:write_file(?OUTPUT_FILE,
		io_lib:fwrite("\n~p;~p;~p", [calendar:local_time(),A1, A2]), [append]).
writeout(A1, A2, A3, A4, A5, A6) ->
	file:write_file(?OUTPUT_FILE,
		io_lib:fwrite("\n~p;~p;~p;~p;~p;~p", [A1, A2, A3, A4, A5, A6]), [append]).

%%------------TIME---------------------------------------
timestamp() ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(erlang:timestamp()),
	lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])).

t() ->
	erlang:monotonic_time().

tdiff(A, B) ->
	B - A.

%%------------EXCAPE URI---------------------------------

escape_uri(<<C:8, Cs/binary>>) when C >= $a, C =< $z ->
	EscUri = escape_uri(Cs),
	<<C, EscUri/binary>>;
escape_uri(<<C:8, Cs/binary>>) when C >= $A, C =< $Z ->
	EscUri = escape_uri(Cs),
	<<C, EscUri/binary>>;
escape_uri(<<C:8, Cs/binary>>) when C >= $0, C =< $9 ->
	EscUri = escape_uri(Cs),
	<<C, EscUri/binary>>;
escape_uri(<<C:8, Cs/binary>>) when C == $. ->
	EscUri = escape_uri(Cs),
	<<C, EscUri/binary>>;
escape_uri(<<C:8, Cs/binary>>) when C == $- ->
	EscUri = escape_uri(Cs),
	<<C, EscUri/binary>>;
escape_uri(<<C:8, Cs/binary>>) when C == $_ ->
	EscUri = escape_uri(Cs),
	<<C, EscUri/binary>>;
escape_uri(<<C:8, Cs/binary>>) ->
	EscByte = escape_byte(C),
	EscUri = escape_uri(Cs),
	<<EscByte/binary, EscUri/binary>>;
escape_uri(<<>>) ->
	<<>>.

escape_byte(C) when C >= 0, C =< 255 ->
	Hex1 = hex_digit(C bsr 4),
	Hex2 = hex_digit(C band 15),
	<<"%", Hex1, Hex2>>.

hex_digit(N) when N >= 0, N =< 9 ->
	N + $0;
hex_digit(N) when N > 9, N =< 15 ->
	N + $a - 10.
