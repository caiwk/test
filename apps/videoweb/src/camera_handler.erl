%% -*- coding: utf-8 -*-

%%%-------------------------------------------------------------------
%%% @author huxiaofeng
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. May 2018 12:49
%%%-------------------------------------------------------------------
-module(camera_handler).
-author("huxiaofeng").

-include_lib("epgsql/include/epgsql.hrl").

%%-compile([{parse_transform, lager_transform}]).

%% API
-export([
  init/2, terminate/3, options/2,
  allowed_methods/2,
  allow_missing_post/2,
  resource_exists/2,
  content_types_provided/2,
  content_types_accepted/2
]).

-export([do_get/2, do_post_json/2, do_post_form/2]).

-ifndef(URL_BASE).
-define(URL_BASE, "/static").
-endif.

init(Req0, _State) ->
  DB = <<"remote">>,
  case DB of
    <<"remote">> ->
      {ok, C} = epgsql:connect("10.106.128.94", "postgres", "123456", [
        {port, 5432},
        {database, "videoanalysis"},
        {timeout, 5000}
      ]);
    <<"localhost">> ->
      {ok, C} = epgsql:connect("localhost", "postgres", "123456", [
        {port, 5432},
        {database, "postgres"},
        {timeout, 5000}
      ])
  end,

  Req1 = cowboy_req:set_resp_header(<<"access-control-max-age">>, <<"1728000">>, Req0),
  Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, POST">>, Req1),
  Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type, authorization">>, Req2),
  Req = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req3),

  State = #{<<"connection">> => C},
  {cowboy_rest, Req, State}.

terminate(_Reason, _Req, State) ->
  #{<<"connection">> := C} = State,
  ok = epgsql:close(C),
  ok.

options(Req0, State) ->
  Req1 = cowboy_req:set_resp_header(<<"access-control-max-age">>, <<"1728000">>, Req0),
  Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, POST">>, Req1),
  Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type, authorization">>, Req2),
  Req = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req3),
  {ok, Req, State}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

allow_missing_post(Req, State) ->
  {false, Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, do_get}], Req, State}.


content_types_accepted(Req, State) ->
  Method = cowboy_req:method(Req),
  case Method of
    <<"POST">> ->
      {[{<<"application/json">>, do_post_json},
        {<<"application/x-www-form-urlencoded">>, do_post_form}], Req, State}
  end.

resource_exists(Req, State) ->
  case cowboy_req:binding(camera_id, Req) of
    undefined ->
      State1 = maps:put(<<"camera">>, undefined, State),
      {true, Req, State1};
    CameraId ->
      #{<<"connection">> := Conn} = State,
      {ok, Columns, Rows} = epgsql:equery(Conn, "select id, location, realtime_path from public.camera where id=$1", [CameraId]),
      case to_map(Columns, Rows) of
        [] ->
          {false, Req, State};
        [Camera0] ->
          Camera = maps:merge(Camera0, #{<<"is_hd">> => true, <<"videos">> => [], <<"alerts">> => []}),
          State1 = maps:put(<<"camera">>, Camera, State),
          Blocked = cowboy_req:binding(blocked, Req),
          State2 = maps:put(<<"blocked">>,Blocked, State1),
          {true, Req, State2}
      end
  end.

%% curl -i -X GET -H "Content-Type: application/json" http:localhost:8080/api/hardware/pool
%% curl -i -X GET -H "Content-Type: application/json" http:localhost:8080/api/hardware/pool/1
%% curl -i -X GET -H "Content-Type: application/json" http:localhost:8080/api/hardware/pool\?search\=1-1-1
%% curl -i -X GET -H "Content-Type: application/json" http:localhost:8080/api/hardware/pool\?page\=1\&limit\=2
%% curl -i -X POST -H "Content-Type: application/json" -d '{"name":"1-4-1"}' http:localhost:8080/api/hardware/pool
%% curl -i -X PATCH -H "Content-Type: application/json" -d '{"name":"1-4-2","servers":[11,13,15]}' http:localhost:8080/api/hardware/pool/3692
%% curl -i -X DELETE -H "Content-Type: application/json" http:localhost:8080/api/hardware/pool/3692


do_get(Req, State) ->
  Body = #{<<"msg">> => <<"Please send POST method with JSON data!">>},
  Response = jsx:prettify(jsx:encode(Body)),
  {Response, Req, State}.

do_post_form(Req0, State) ->
  {ok, PostForm, Req} = cowboy_req:read_urlencoded_body(Req0),
  Search = list_to_map(PostForm),
  erlang:display(Search),
  do_post(Req, State, Search).

list_to_map(List) ->
  lists:foldl(fun(T, AccIn) ->
    {Key, Value} = T,
    case maps:is_key(Key, AccIn) of
      true ->
        V0 = maps:get(Key, AccIn),
        case erlang:is_list(V0) of
          true ->
            V1 = lists:sort(V0),
            V2 = lists:merge([Value], V1),
            maps:put(Key, V2, AccIn);
          false ->
            maps:put(Key, [Value, V0], AccIn)
        end;
      false ->
        maps:put(Key, Value, AccIn)
    end
              end, #{}, List).

do_post_json(Req0, State) ->
  {ok, PostJson, Req} = cowboy_req:read_body(Req0),
  Search = try
             jsx:decode(PostJson, [return_maps])
           catch
             _:_ -> #{}
           end,
  do_post(Req, State, Search).

do_post(Req, State, Search0) ->
  StaticBaseUrl = io_lib:format("//~s:~B/vod2", [cowboy_req:host(Req), 8080]),

  #{<<"connection">> := Conn, <<"camera">> := Camera} = State,

  Body = case Camera of
           undefined ->
             Search = set_camera_default_search_options(Search0),
             {ok, _, TotalCount0} = epgsql:equery(Conn, "SELECT COUNT(id) FROM public.camera", []),
             case TotalCount0 of
               [] ->
                 #{<<"search">> => Search,
                   <<"result">> => #{<<"paging_total_count">> => 0, <<"cameras">> => []}};
               [{PagingTotalCount}] ->
                 Cameras = find_camera(Conn, Search, StaticBaseUrl, State),
                 #{<<"search">> => Search,
                   <<"result">> => #{<<"paging_total_count">> => PagingTotalCount, <<"cameras">> => Cameras}}
             end;
           _ ->
             Search = set_video_default_search_options(Search0),
             CameraDetail = get_detail(Conn, Camera, Search, StaticBaseUrl,State),
             #{<<"search">> => Search,
               <<"result">> => CameraDetail}
         end,

  Req1 = cowboy_req:set_resp_body(jsx:prettify(jsx:encode(Body)), Req),
  {true, Req1, State}.

set_camera_default_search_options(Search0) ->
  Search1 = maps:put(<<"timestamp">>, conv_to_integer(maps:get(<<"timestamp">>, Search0, erlang:system_time(second) + 8 * 3600)), Search0),
  Search2 = maps:put(<<"back_hour">>, conv_to_integer(maps:get(<<"back_hour">>, Search0, 24)), Search1),
  Search3 = maps:put(<<"keywords">>, maps:get(<<"keywords">>, Search0, []), Search2),
  Search4 = maps:put(<<"tags">>, maps:get(<<"tags">>, Search0, []), Search3),
  Search5 = maps:put(<<"paging_first_index">>, conv_to_integer(maps:get(<<"paging_first_index">>, Search0, 0)), Search4),
  Search6 = maps:put(<<"paging_page_size">>, conv_to_integer(maps:get(<<"paging_page_size">>, Search0, 1000)), Search5),
  Search6.

set_video_default_search_options(Search0) ->
  Search1 = maps:put(<<"timestamp">>, conv_to_integer(maps:get(<<"timestamp">>, Search0, erlang:system_time(second) + 8 * 3600)), Search0),
  Search2 = maps:put(<<"back_hour">>, conv_to_integer(maps:get(<<"back_hour">>, Search0, 24)), Search1),
  Search3 = maps:put(<<"keywords">>, maps:get(<<"keywords">>, Search0, []), Search2),
  Search4 = maps:put(<<"tags">>, maps:get(<<"tags">>, Search0, []), Search3),
  Search4.

conv_to_integer(Something) ->
  case is_integer(Something) of
    true -> Something;
    false ->
      case is_binary(Something) of
        true -> erlang:binary_to_integer(Something);
        false -> string:list_to_integer(Something)
      end
  end.

find_camera(Conn, Search, StaticBaseUrl, State) ->
  #{<<"paging_first_index">> := PagingFirstIndex, <<"paging_page_size">> := PagingPageSize} = Search,

  {ok, Columns, Rows} = epgsql:equery(Conn, "select id, location, realtime_path from public.camera
  ORDER BY id ASC LIMIT $1 OFFSET $2", [PagingPageSize, PagingFirstIndex]),
  Cameras = to_map(Columns, Rows),

  lists:map(fun(Camera) ->
    get_detail(Conn, Camera, Search, StaticBaseUrl,State)
            end, Cameras).

get_detail(Conn, Camera, Search, StaticBaseUrl, State) ->
  #{<<"timestamp">> := SearchTimestamp, <<"back_hour">> := BackHour} = Search,
  #{<<"blocked">> := Blocked} = State,

  EndDate = calendar:gregorian_seconds_to_datetime(SearchTimestamp + 62167219200),
  StartDate = calendar:gregorian_seconds_to_datetime(SearchTimestamp + 62167219200 - BackHour * 3600),

  CameraId = maps:get(<<"id">>, Camera),


  {ok, _ } = epgsql:equery(Conn, "update public.alert set blocked = $1 where video_id in
  (select id from public.video where camera_id = $2) ",[Blocked, CameraId] ),


  {ok, Columns1, Rows1} = epgsql:equery(Conn, "select alert.* from public.alert
  join public.video on (video.id = alert.video_id)
  where video.camera_id=$1 AND alert.start_date > $2 AND alert.start_date <= $3 ORDER BY alert.id DESC", [CameraId, StartDate, EndDate]),
  AllAlerts = lists:map(fun(Alert) ->
    wrap_alert(Alert, StaticBaseUrl)
                        end, to_map(Columns1, Rows1)),

  {ok, Columns, Rows} = epgsql:equery(Conn, "select * from public.video
  where camera_id=$1 AND start_date > $2 AND start_date <= $3 ORDER BY id DESC", [CameraId , StartDate, EndDate]),
  Videos = lists:map(fun(Video) ->
    wrap_video(Video, AllAlerts, StaticBaseUrl)
                     end, to_map(Columns, Rows)),

  Url = io_lib:format("~s/~s", [StaticBaseUrl, maps:get(<<"realtime_path">>, Camera)]),
  Camera1 = maps:put(<<"realtime_url">>, binary:list_to_bin(Url), Camera),
  Camera2 = maps:remove(<<"realtime_path">>, Camera1),

  maps:merge(Camera2, #{<<"videos">> => Videos}).

wrap_video(Video0, AllAlerts, StaticBaseUrl) ->
  #{<<"video_path">> := VideoPath, <<"poster_path">> := PosterPath, <<"start_date">> := StartDate} = Video0,
  Url = io_lib:format("~s/~s", [StaticBaseUrl, VideoPath]),
  Poster = io_lib:format("~s/~s", [StaticBaseUrl, PosterPath]),
  Timestamp = to_timestamp(StartDate),

  VideoId = maps:get(<<"id">>, Video0),
  Alerts0 = lists:filter(fun(A) ->
    VideoId == maps:get(<<"video_id">>, A)
                         end, AllAlerts),

  Alerts = lists:map(fun(A) ->
    StartSecond = maps:get(<<"start_timestamp">>, A),
    maps:put(<<"start_sec_in_video">>, StartSecond - Timestamp, A)
                     end, Alerts0),

  A1 = maps:merge(Video0, #{<<"video_url">> => binary:list_to_bin(Url), <<"poster_url">> => binary:list_to_bin(Poster),
    <<"format">> => <<"mp4">>, <<"start_timestamp">> =>Timestamp, <<"alerts">> => Alerts}),
  A2 = maps:without([<<"camera_id">>, <<"poster_path">>, <<"video_path">>, <<"start_date">>], A1),
  map_key_replace(<<"duration_in_second">>, <<"duration">>, A2).

wrap_alert(Alert0, StaticBaseUrl) ->
  #{<<"poster_path">> := PosterPath, <<"start_date">> := StartDate, <<"end_date">> := EndDate} = Alert0,
  Poster = io_lib:format("~s/~s", [StaticBaseUrl, PosterPath]),
  StartTimestamp = to_timestamp(StartDate),
  EndTimestamp = to_timestamp(EndDate),

  A1 = maps:merge(#{<<"poster_url">> => binary:list_to_bin(Poster),
    <<"start_timestamp">>=> StartTimestamp, <<"end_timestamp">>=> EndTimestamp}, Alert0),
  A2 = maps:without([<<"poster_path">>, <<"start_date">>, <<"end_date">>], A1),
  A2.

to_map(Cols, Rows) ->
  [maps:from_list(lists:zipwith(fun(#column{name = N}, V) -> {N, V} end,
    Cols, tuple_to_list(Row))) || Row <- Rows].

map_key_replace(OldKey, NewKey, Map) ->
  V = maps:get(OldKey, Map),
  M = maps:put(NewKey, V, Map),
  maps:remove(OldKey, M).

to_timestamp(DateTimeZ) ->
  try
    {{Y, M, D}, {HH, MM, SecondZ}} = DateTimeZ,
    List = string:tokens(float_to_list(SecondZ, [{decimals, 10}, compact]), "."),
    Second = list_to_integer(lists:nth(1, List)),
    %%TZ = list_to_integer(lists:nth(2,List)),
    DateTime = {{Y, M, D}, {HH, MM, Second}},
    %% 62167219200 == calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200
  catch
    _:_ -> <<"null">>
  end.
