%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%%
%%%-------------------------------------------------------------------

-module(status_handler_SUITE).
-author("Duncan Sparrell").

%% for test
-compile(export_all).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% tests to run
all() ->
    [ test_status
    ].

%% timeout if no reply in a minute
suite() ->
    [{timetrap,{minutes,2}}].

%% setup config parameters
init_per_suite(Config) ->
      {ok, _AppList} = application:ensure_all_started(shotgun),
      {ok, _AppList2} = application:ensure_all_started(sFractals),

      Config.

test_status(_Config) ->
  %%Env = application:get_all_env(sFractals),
  %%ct:pal("ENV: ~p", [Env]),
  MyPort = application:get_env(sFractals, port, 8080),
  {ok, Conn} = shotgun:open("localhost", MyPort),
  {ok, Response} = shotgun:get(Conn, "/status"),
  #{body := <<"<html><body>Status Works - needs more later</body></html>">>
   , status_code := 200} = Response,
  shotgun:close(Conn),
  ok.
