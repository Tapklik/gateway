-module(gateway_http_handler).

-export([init/2, info/3]).

-include("gateway_global.hrl").
-include("lager.hrl").

-record(state, {
	exchange,
	t1,
	statsderl_prefix
}).

%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%% Cowboy standard interface: init(Req, State) -> (ok, Req2, State)
%% - Receive BR and request a process from poolboy
%% - Send BR to gateway_bids_handler and wait for response,
%% 		gateway_bids_handler is responsible for building proper response
%% - Response can be either:
%% 		-- timeout => defined through header file BID_TIMEOUT (HTTP 204, no content)
%% 		-- invalid_br => usually happens when error in parsing BR (HTTP 204, no content)
%% 		-- valid RSP map => decode to json and send response (HTTP 200, RSP)
%%
init(Req, State) ->
	%% STAT
	T1 = erlang:monotonic_time(),
	[Node, Host] = binary:split(atom_to_binary(node(), latin1), <<"@">>),
	Prefix = <<"bids.gw.", Node/binary, "__", Host/binary, ".">>,
	statsderl:increment(<<Prefix/binary, "br.total">>, 1, ?STATS_P),
	{ok, BRjson, _} = cowboy_req:read_body(Req), tk_lib:echo1(brjson, BRjson),
	BR = jsx:decode(BRjson, [return_maps]),
	case gateway_bids_handler:start_link() of
		{ok, Worker} ->
			Worker,
			Exchange = proplists:get_value(exchange, State, <<"0">>),
			Worker ! {self(), br, Exchange, BR};
		{error, _} ->
			self() ! {rsp, error, no_workers}
	end,
	{cowboy_loop, Req, #state{t1 = T1, statsderl_prefix = Prefix}}.

%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

%%
%% @hidden
%% builds response in cowboy form and sends back as req2 to init/2:
%% 		cowboy_req:reply(Response Code, Headers, Content, Req)
%%
info({rsp, _, _} = RSP, Req, State) ->
	Prefix = State#state.statsderl_prefix,

	%% Build cowboy proper response
	build_response(Req, RSP, Prefix),
	Headers = #{
		<<"x-openrtb-version">> => <<"2.5">>,
		<<"charset">> => <<"utf-8">>
	},
	cowboy_req:set_resp_headers(Headers, Req),
	statsderl:increment(<<Prefix/binary, "rsp.total">>, 1, ?STATS_P),

	%% STAT: Calculate bid response time
	RSPTime = calc_time(State#state.t1),
	statsderl:timing(<<Prefix/binary, "rsp.time.total">>, RSPTime, ?STATS_P),
	case RSPTime of
		T when T < 50 ->
			statsderl:increment(<<Prefix/binary, "rsp.time.50">>, 1, ?STATS_P);
		T when T >= 50 andalso T < 70 ->
			statsderl:increment(<<Prefix/binary, "rsp.time.70">>, 1, ?STATS_P);
		T when T >= 70 andalso T < 90 ->
			statsderl:increment(<<Prefix/binary, "rsp.time.90">>, 1, ?STATS_P);
		T when T >= 90 andalso T < 120 ->
			statsderl:increment(<<Prefix/binary, "rsp.time.120">>, 1, ?STATS_P);
		T when T >= 120 andalso T < 150 ->
			statsderl:increment(<<Prefix/binary, "rsp.time.150">>, 1, ?STATS_P);
		T when T >= 150 andalso T < 200 ->
			statsderl:increment(<<Prefix/binary, "rsp.time.200">>, 1, ?STATS_P);
		T when T >= 200 ->
			statsderl:increment(<<Prefix/binary, "rsp.time.200plus">>, 1, ?STATS_P);
		T -> ok
	end,
	{stop, Req, State};
info(_Message, Req, State) ->
	{ok, Req, State}.


%%
%% @hidden
%% builds repsonse in cowboy form and sends back as req to through info/3:
%% 		cowboy_req:reply(Response Code, Headers, Content, Req)
%%
build_response(Req, {rsp, _, timeout}, Prefix) ->
	statsderl:increment(<<Prefix/binary, "rsp.result.invalid_timeout">>, 1, ?STATS_P),
	cowboy_req:reply(204, ?ADX03_RSP_HEADERS, Req);
build_response(Req, {rsp, _, invalid_br}, Prefix) ->
	statsderl:increment(<<Prefix/binary, "rsp.result.invalid_br">>, 1, ?STATS_P),
	cowboy_req:reply(204, ?ADX03_RSP_HEADERS, Req);
build_response(Req, {rsp, _, invalid_rsp}, Prefix) ->
	statsderl:increment(<<Prefix/binary, "rsp.result.invalid_rsp">>, 1, ?STATS_P),
	cowboy_req:reply(204, ?ADX03_RSP_HEADERS, Req);
build_response(Req, {rsp, _, no_workers}, Prefix) ->
	statsderl:increment(<<Prefix/binary, "rsp.result.invalid_no_workers">>, 1, ?STATS_P),
	cowboy_req:reply(204, ?ADX03_RSP_HEADERS, Req);
build_response(Req, {rsp, _, RSPmap}, Prefix) ->
	statsderl:increment(<<Prefix/binary, "rsp.result.ok">>, 1, ?STATS_P),
	RSPjson = jsx:encode(RSPmap),
	cowboy_req:reply(200, ?ADX03_RSP_HEADERS, RSPjson, Req).


%% @hidden
calc_time(T1) ->
	%% STAT: Calculate bid response time
	T2 = erlang:monotonic_time(),
	erlang:convert_time_unit(T2 - T1, native, milli_seconds).