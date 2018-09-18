%%%-------------------------------------------------------------------
%%% @author huxiaofeng
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Apr 2018 23:10
%%%-------------------------------------------------------------------
-module(root_handler).
-author("huxiaofeng").

%%-compile({parse_transform, auto_json}).

%% API
-export([
  init/2,
  allowed_methods/2,
  content_types_provided/2
]).

-export([do_get/2]).

init(Req, State) ->
  {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, do_get}], Req, State}.

%% curl -i -X GET -H "Content-Type: application/json" http://localhost:8080/api/hardware/pool
%% curl -i -X GET -H "Content-Type: application/json" http://localhost:8080/api/hardware/pool/1
%% curl -i -X GET -H "Content-Type: application/json" http://localhost:8080/api/hardware/pool\?search\=1-1-1
%% curl -i -X GET -H "Content-Type: application/json" http://localhost:8080/api/hardware/pool\?page\=1\&limit\=2
%% curl -i -X POST -H "Content-Type: application/json" -d '{"name":"1-4-1"}' http://localhost:8080/api/hardware/pool
%% curl -i -X PATCH -H "Content-Type: application/json" -d '{"name":"1-4-2","servers":[11,13,15]}' http://localhost:8080/api/hardware/pool/3692
%% curl -i -X DELETE -H "Content-Type: application/json" http://localhost:8080/api/hardware/pool/3692

do_get(Req, State) ->
  Body = #{<<"msg">> => <<"welcome!">>},
  Response = jsx:prettify(jsx:encode(Body)),

  %%Response = <<"iiiiii">>,
  %%lager:info("resp=~p", [Response]),
  {Response, Req, State}.

