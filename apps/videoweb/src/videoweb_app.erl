%%%-------------------------------------------------------------------
%% @doc videoweb public API
%% @end
%%%-------------------------------------------------------------------

-module(videoweb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", root_handler, []},
      {"/cam/[:id]", [{id, int}], cam_handler, []},
      {"/camera/:blocked/[:camera_id]", [{blocked, int},{camera_id, int}], camera_handler, []},
      {"/static/[...]", cowboy_static, {dir, "/root/temp_bwc"}}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(my_http_listener, [{port, 8088}],
    #{env => #{dispatch => Dispatch}}
  ),
  videoweb_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
