-module(gateway_api).

-export([
	init/2,
	allowed_methods/2,
	content_types_provided/2,
	options/2
]).

-export([handle_get/2]).


init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, handle_get}
	], Req, State}.

options(Req, State) ->
	Headers = #{
		<<"Access-Control-Allow-Methods">> => <<"POST, GET, OPTIONS, DELETE, PUT">>,
		<<"Access-Control-Allow-Origin">> => <<"*">>,
		<<"Access-Control-Allow-Headers">> => <<"Authorize, Cache-Control, x-requested-with, Content-Type, origin, authorization, accept, client-security-token, x-csrf-token, X-CSRF-Token">>
	},
	{ok, cowboy_req:set_resp_headers(Headers, Req), State}.

handle_get(Req, State) ->
	Resp = case cowboy_req:binding(service, Req) of
			   <<"date">> ->
				   case time_server:get_iso_time() of
					   Body when is_binary(Body) ->
						   Body;
					   _ ->
						   eval_error(else)
				   end;
			   <<"health">> ->
				   case vm:check_health() of
					   {ok, <<"running">>, _} ->
						   <<"healthy">>;
					   _ ->
						   {error, down}
				   end;
			   _ ->
				   eval_error(not_found)
		   end,
	Headers = #{
		<<"Access-Control-Allow-Methods">> => <<"POST, GET, OPTIONS, DELETE, PUT">>,
		<<"Access-Control-Allow-Origin">> => <<"*">>,
		<<"Access-Control-Allow-Headers">> => <<"Authorize, Cache-Control, x-requested-with, Content-Type, origin, authorization, accept, client-security-token, x-csrf-token, X-CSRF-Token">>
	},
	{Resp, cowboy_req:set_resp_headers(Headers, Req), State}.



eval_error(not_found) ->
	"Resource not available";
eval_error(_) ->
	"Unknown error".



