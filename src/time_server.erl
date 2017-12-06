-module(time_server).

-behaviour(gen_server).

-include("lager.hrl").


-export([
	start_link/0,
	get_time/0, get_timestamp/0, get_iso_time/0,
	reset_start_time/0, set_start_time/1,
	datetime_to_timestamp/1, timestamp_to_datetime/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).


-record(state, {
	time,
	starting_time
}).

-define(INTERVAL, 200).
-define(TIME_SCALE, application:get_env(time_scale)).
-define(SEC, 1).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_time() ->
	case try_ets_lookup(time, time_unix) of
		not_found ->
			?ERROR("TIME SERVER: ETS broken!", []);
		T -> T
	end.

get_timestamp() ->
	case try_ets_lookup(time, timestamp) of
		not_found ->
			?ERROR("TIME SERVER: ETS broken!", []);
		T -> T
	end.

get_iso_time() ->
	case try_ets_lookup(time, time_iso) of
		not_found ->
			?ERROR("TIME SERVER: ETS broken!", []);
		T -> T
	end.

reset_start_time() ->
	gen_server:call(?MODULE, {reset_start_time}).

set_start_time(Time) ->
	gen_server:call(?MODULE, {set_start_time, Time}).

datetime_to_timestamp({{Year,Month,Day},{Hours,Minutes,Seconds}}) ->
	(calendar:datetime_to_gregorian_seconds(
		{{Year,Month,Day},{Hours,Minutes,Seconds}}
	) - 62167219200)*?SEC.

timestamp_to_datetime(Timestamp) ->
	calendar:gregorian_seconds_to_datetime(trunc(Timestamp/?SEC) + 62167219200).


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%% @private
%% Starts the gen_server loop by sending an {interval} msg to initiate.
%%
%%
init([]) ->
	process_flag(trap_exit, true),
	ets:new(time, [named_table, set, public, {read_concurrency, true}]),
	%% Init
	erlang:send_after(?INTERVAL, self(), {init}),
	{ok, #state{}}.


handle_call({reset_start_time}, _From, State) ->
	{T1, T2, _T3} = erlang:timestamp(),
	Time = trunc(?SEC * ((T1 * 1000000) + T2)), %% milli seconds
	Time60 = Time - (Time rem (60 * ?SEC)),
	{reply, {ok, Time60}, State#state{starting_time = Time60}};

handle_call({set_start_time, Time}, _From, State) ->
	{T1, T2, _T3} = erlang:timestamp(),
	T = trunc(?SEC * ((T1 * 1000000) + T2)),
	StartingTime = case ?TIME_SCALE of
					   undefined ->
						   State#state.starting_time;
					   1 ->
						   State#state.starting_time;
					   {ok, TimeScale} ->
						   trunc((Time - T * TimeScale )/(1 - TimeScale))
				   end,
	{reply, {ok, StartingTime}, State#state{starting_time = StartingTime}}.

handle_cast(_Request, State) ->
	{noreply, State}.

%%
%% @private
%%
handle_info({init}, State) ->
	{T1, T2, _T3} = erlang:timestamp(),
	Time = trunc(?SEC * ((T1 * 1000000) + T2)), %% milli seconds
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(erlang:timestamp()),
	IsoTime = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
		[Year, Month, Day, Hour, Minute, Second])),
	Time60 = Time - (Time rem (60 * ?SEC)),
	Time300 = trunc(Time - (Time rem (300 * ?SEC))),
	ets:insert(time, {time_unix, Time}),
	ets:insert(time, {time_iso, list_to_binary(IsoTime)}),
	ets:insert(time, {time_unix_min, Time60}),
	ets:insert(time, {timestamp, Time300}),
	erlang:send_after(?INTERVAL, self(), {interval}),
	{noreply, State#state{starting_time = Time60}};
handle_info({interval}, State) ->
	{T1, T2, _T3} = erlang:timestamp(),
	Time0 = State#state.time,
	TimeScale = case ?TIME_SCALE of
					undefined -> 1;
					{ok, TS} -> TS
				end,
	Time = case TimeScale of
			   1 ->
				   trunc(?SEC * ((T1 * 1000000) + T2));
			   _ ->
				   StartingTime = State#state.starting_time,
				   T = trunc(?SEC * ((T1 * 1000000) + T2)),
				   StartingTime + (T - StartingTime) * TimeScale
		   end,
	case Time == Time0 of
		true -> ok;
		false ->
			%% Timestamp (5 min)
			case Time rem (300 * ?SEC) of
				R when TimeScale == 1 ->
					TimeStamp = trunc(Time - (Time rem (300 * ?SEC))),
					ets:insert(time, {timestamp, TimeStamp});
				R when R > 300 - TimeScale ->
					TimeStamp = trunc(Time - (Time rem (300 * ?SEC))),
					ets:insert(time, {timestamp, TimeStamp});
				_ ->
					ok
			end,

			%% Unix Time
			ets:insert(time, {time_unix, Time}),

			%% ISO Time
			{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(unix_to_erlang_timestamp(Time)),
			IsoTime = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
				[Year, Month, Day, Hour, Minute, Second])),
			ets:insert(time, {time_iso, list_to_binary(IsoTime)}),

			%% Unix Time (minutes)
			case Time rem (60 * ?SEC) of
				0 ->
					ets:insert(time, {time_unix_min, Time});
				_ ->
					ok
			end
	end,
	erlang:send_after(?INTERVAL, self(), {interval}),
	{noreply, State#state{time = Time}};
handle_info({stop}, State) ->
	{stop, shutdown, State};
handle_info({'EXIT', _, _}, State) ->
	{stop, shutdown, State}.

%%
%% @private
%% Error in case of exit
%%
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

unix_to_erlang_timestamp(U) ->
	T1 = trunc(U/1000000),
	T2 = U - (T1 * 1000000),
	{T1, T2, 0}.

try_ets_lookup(Table, Key) ->
	try_ets_lookup(Table, Key, not_found).
try_ets_lookup(Table, Key, Default) ->
	case ets:lookup(Table, Key) of
		[{_, Val} | _] -> Val;
		[] -> Default
	end.