%% -*- coding: utf-8 -*-

%%%-------------------------------------------------------------------
%%% @author huxiaofeng
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. May 2018 12:49
%%%-------------------------------------------------------------------
-module(cam_handler).
-author("huxiaofeng").

-include_lib("epgsql/include/epgsql.hrl").

%%-compile([{parse_transform, lager_transform}]).

%% API
-export([
  init/2, terminate/3,
  allowed_methods/2,
  content_types_provided/2,
  content_types_accepted/2
]).

-export([do_get/2, do_post/2]).

-ifndef(URL_BASE).
-define(URL_BASE, "/static").
-endif.

init(Req, State) ->
  {ok, C} = epgsql:connect("localhost", "postgres", "111111", [
    {port, 5433},
    {database, "postgres"},
    {timeout, 4000}
  ]),
  State1 = #{<<"connection">> => C},
  {cowboy_rest, Req, State1}.

terminate(_Reason, _Req, State) ->
  #{<<"connection">> := C} = State,
  ok = epgsql:close(C),
  ok.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, do_get}], Req, State}.


content_types_accepted(Req, State) ->
  Method = cowboy_req:method(Req),
  case Method of
    <<"POST">> ->
      {[{<<"application/json">>, do_post}], Req, State}
  end.

%% curl -i -X GET -H "Content-Type: application/json" http:localhost:8080/api/hardware/pool
%% curl -i -X GET -H "Content-Type: application/json" http:localhost:8080/api/hardware/pool/1
%% curl -i -X GET -H "Content-Type: application/json" http:localhost:8080/api/hardware/pool\?search\=1-1-1
%% curl -i -X GET -H "Content-Type: application/json" http:localhost:8080/api/hardware/pool\?page\=1\&limit\=2
%% curl -i -X POST -H "Content-Type: application/json" -d '{"name":"1-4-1"}' http:localhost:8080/api/hardware/pool
%% curl -i -X PATCH -H "Content-Type: application/json" -d '{"name":"1-4-2","servers":[11,13,15]}' http:localhost:8080/api/hardware/pool/3692
%% curl -i -X DELETE -H "Content-Type: application/json" http:localhost:8080/api/hardware/pool/3692

do_get(Req, State) ->
  CameraId = cowboy_req:binding(id, Req),
  case CameraId of
    undefined ->
      get_list(Req, State);
    _ ->
      get_detail(CameraId, Req, State)
  end.

to_map(Cols, Rows) ->
  [maps:from_list(lists:zipwith(fun(#column{name = N}, V) -> {N, V} end,
    Cols, tuple_to_list(Row))) || Row <- Rows].

map_key_replace(OldKey, NewKey, Map) ->
  V = maps:get(OldKey, Map),
  M = maps:put(NewKey, V, Map),
  maps:remove(OldKey, M).

to_timestamp(DateTimeZ) ->
  {{Y,M,D},{HH,MM,SecondZ}} = DateTimeZ,
  List = string:tokens(float_to_list(SecondZ, [{decimals, 10}, compact]), "."),
  Second = list_to_integer(lists:nth(1,List)),
  %%TZ = list_to_integer(lists:nth(2,List)),
  DateTime = {{Y,M,D},{HH,MM,Second}},
  GSeconds = calendar:datetime_to_gregorian_seconds(DateTime) + 8*3600,
  GSeconds - 62167219200.
%% 62167219200 == calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
  %%{Seconds div 1000000, Seconds rem 1000000, 0}.

get_list(Req, State) ->
  #{firstindex := FirstIndex} = cowboy_req:match_qs([{firstindex, int, 0}], Req),
  #{pagesize := PageSize} = cowboy_req:match_qs([{pagesize, int, 1000}], Req),
  #{keyword := Keywords0} = cowboy_req:match_qs([{keyword, [], []}], Req),
  #{tag := Tags0} = cowboy_req:match_qs([{tag, [], []}], Req),
  Keywords = case is_list(Keywords0) of
               true -> Keywords0;
               false -> [Keywords0]
             end,
  Tags = case is_list(Tags0) of
           true -> Tags0;
           false -> [Tags0]
         end,
  BaseUrl = io_lib:format("//~s:~B/static", [cowboy_req:host(Req), cowboy_req:port(Req)]),

  #{<<"connection">> := C} = State,
  {ok, _, TotalCount0} = epgsql:equery(C, "SELECT COUNT(id) FROM public.camera", []),
  TotalCount = case TotalCount0 of
                 [] -> 0;
                 [{TotalCount1}] -> TotalCount1
               end,

  Basic = #{<<"timestamp">> => erlang:system_time(second), <<"keywords">> => Keywords, <<"tag">> => Tags,
    <<"totalcount">> => TotalCount, <<"firstindex">> => FirstIndex, <<"pagesize">> => PageSize},
  erlang:display(Basic),

  {ok, Columns1, Rows1} = epgsql:equery(C, "select camera.id as id, camera.location as location
from public.camera ORDER BY camera.id ASC LIMIT $1 OFFSET $2", [PageSize, FirstIndex]),
  Result = to_map(Columns1, Rows1),
  case Result of
    [] -> List = [];
    _ -> List = lists:map(fun(A) ->
      MostRecentVideo = query_most_recent_video(C, BaseUrl, A),
      Events = case maps:is_key(<<"id">>, MostRecentVideo) of
                 false -> [];
                 true -> query_event(C, BaseUrl, MostRecentVideo)
               end,
      A1 = maps:merge(A, maps:remove(<<"id">>, MostRecentVideo)),
      A2 = maps:merge(A1, #{<<"hd">> => true, <<"format">> => <<"mp4">>, <<"events">> => Events}),
      A3 = maps:remove(<<"poster_path">>, A2),
      A4 = maps:remove(<<"video_path">>, A3),
      map_key_replace(<<"id">>, <<"key">>, A4)
                          end, Result)
  end,

  Body = #{<<"basic">> => Basic, <<"list">> => List},
  Response = jsx:prettify(jsx:encode(Body)),

  %%lager:info("resp=~p", [Body]),
  {Response, Req, State}.

query_most_recent_video(C, BaseUrl, Camera) ->
  #{<<"id">> := CameraId} = Camera,
  {ok, Columns, Rows} = epgsql:equery(C, "select id, video_path, poster_path from public.video where camera_id = $1 order by id DESC LIMIT 1", [CameraId]),
  Result = to_map(Columns, Rows),
  case Result of
    [] -> #{};
    [A] ->
      #{<<"video_path">> := VideoPath, <<"poster_path">> := PosterPath} = A,
      Url = io_lib:format("~s/~s", [BaseUrl, VideoPath]),
      Poster = io_lib:format("~s/~s", [BaseUrl, PosterPath]),
      maps:merge(A, #{<<"url">> => binary:list_to_bin(Url), <<"poster">> => binary:list_to_bin(Poster)})
  end.

query_event(C, BaseUrl, Video) ->
  #{<<"id">> := VideoId} = Video,
  {ok, Columns, Rows} = epgsql:equery(C, "select id, type, level, message, poster_path, create_date
   from public.event where video_id = $1 order by id DESC", [VideoId]),
  Result = to_map(Columns, Rows),

  case Result of
    [] -> [];
    _ -> lists:map(fun(A) ->
      #{<<"poster_path">> := PosterPath} = A,
      #{<<"create_date">> := CreateDate} = A,
      %%io:format("CreateDate=~w~n", [CreateDate]),
      Poster = io_lib:format("~s/~s", [BaseUrl, PosterPath]),
      A1 = maps:put(<<"poster">>, binary:list_to_bin(Poster), A),
      A2 = maps:remove(<<"poster_path">>, A1),
      Timestamp = to_timestamp(CreateDate),
      A3 = maps:put(<<"timestamp">>, Timestamp, A2),
      A4 = maps:remove(<<"create_date">>, A3),
      maps:remove(<<"id">>, A4)
                   end, Result)
  end.

get_detail(CameraId, Req, State) ->
  #{day := Day} = cowboy_req:match_qs([{day, [], <<"2018-05-01">>}], Req),
  #{keyword := Keywords0} = cowboy_req:match_qs([{keyword, [], []}], Req),
  #{tag := Tags0} = cowboy_req:match_qs([{tag, [], []}], Req),
  Keywords = case is_list(Keywords0) of
               true -> Keywords0;
               false -> [Keywords0]
             end,
  Tags = case is_list(Tags0) of
           true -> Tags0;
           false -> [Tags0]
         end,
  BaseUrl = io_lib:format("//~s:~B/static", [cowboy_req:host(Req), cowboy_req:port(Req)]),

  #{<<"connection">> := C} = State,
  {ok, Columns, Rows} = epgsql:equery(C, "select * from public.camera where id=$1", [CameraId]),
  Result = to_map(Columns, Rows),
  CameraData = case Result of
                 [] -> #{};
                 [Camera] ->
                   Videos = query_video_with_events(C, BaseUrl, Camera, Day),
                   #{<<"id">> := CameraId} = Camera,
                   Seq = lists:seq(1, length(Videos)),
                   FileList = lists:map(fun(N) ->
                     Video = lists:nth(N, Videos),
                     #{<<"events">> := E} = Video,
                     Alert = case length(E) of
                               0 -> false;
                               _ -> true
                             end,
                     maps:merge(#{<<"index">> => N, <<"alert">> => Alert}, maps:remove(<<"events">>, Video))
                                        end, Seq),
                   Events = lists:flatmap(fun(Video) ->
                     #{<<"events">> := EventList} = Video,
                     #{<<"url">> := Url} = Video,
                     lists:map(fun(E) ->
                       maps:merge(#{<<"file_url">> => Url}, E)
                               end, EventList)
                                          end, Videos),
                   C1 = #{<<"hd">> => true, <<"key">>=> CameraId,
                     <<"file_list">> => FileList, <<"events">> => Events},
                   A1 = maps:merge(C1, Camera),
                   A2 = maps:remove(<<"id">>, A1),
                   A3 = maps:remove(<<"rtsp_url">>, A2),
                   maps:remove(<<"title">>, A3)
               end,
  QueryData = #{<<"date">> => Day, <<"search_keyword">> => Keywords, <<"search_tag">> => Tags},
  Body = maps:merge(QueryData, CameraData),
  Response = jsx:prettify(jsx:encode(Body)),

  %%lager:info("resp=~p", [Response]),
  {Response, Req, State}.

query_video_with_events(C, BaseUrl, Camera, Day) ->
  #{<<"id">> := CameraId} = Camera,
  {ok, Columns, Rows} = epgsql:equery(C, "select id, video_path, poster_path, start_date, duration_in_second from public.video where camera_id = $1 order by id ASC", [CameraId]),
  Result = to_map(Columns, Rows),
  case Result of
    [] -> [];
    _ -> lists:map(fun(A) ->
      Events = query_event(C, BaseUrl, A),
      #{<<"video_path">> := VideoPath, <<"poster_path">> := PosterPath, <<"start_date">> := StartDate} = A,
      Url = io_lib:format("~s/~s", [BaseUrl, VideoPath]),
      Poster = io_lib:format("~s/~s", [BaseUrl, PosterPath]),
      Timestamp = to_timestamp(StartDate),
      A1 = maps:merge(A, #{<<"url">> => binary:list_to_bin(Url), <<"poster">> => binary:list_to_bin(Poster),
        <<"format">> => <<"mp4">>,        <<"start_timestamp">> =>Timestamp, <<"events">> => Events}),
      A2 = maps:remove(<<"id">>, A1),
      A3 = maps:remove(<<"poster_path">>, A2),
      A4 = maps:remove(<<"video_path">>, A3),
      A5 = maps:remove(<<"start_date">>, A4),
      map_key_replace(<<"duration_in_second">>,<<"duration">>,A5)
                   end, Result)
  end.

do_post(Req, State) ->
  Body = #{<<"msg">> => <<"welcome!">>},
  Response = jsx:prettify(jsx:encode(Body)),

  %%lager:info("resp=~p", [Response]),
  {Response, Req, State}.