%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%%
%%%-------------------------------------------------------------------

-module(status_SUITE).
-author("Duncan Sparrell").

%% for test
-compile(export_all).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% tests to run
all() ->
    [ test_status
    , test_post
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
  Env = application:get_all_env(sFractals),
  ct:pal("ENV: ~p", [Env]),
  MyPort = application:get_env(sFractals, port, 8081),
  {ok, Conn} = shotgun:open("localhost", MyPort),
  {ok, Response} = shotgun:get(Conn, "/status"),
  ct:pal("Response = ~p", [Response]),
  #{body := <<"<html><body>Status Works - needs more later</body></html>">>
   , status_code := 200} = Response,
  shotgun:close(Conn),
  ok.

test_post(_Config) ->
  MyPort = application:get_env(sFractals, port, 8080),
  {ok, Conn} = shotgun:open("localhost", MyPort),
  Headers = [ {<<"content-type">>,<<"application/json">>} ],
  FractalConfig = #{ fractalAlg    => julian,  % Fractal Algorithm is julian
   imageFileName => "testpost.png",  %image file created
   colorAlg      => simplest,  % 0-11 map to colors
   width         => 10,        % width=10
   height        => 10,        % height=10
   cReal         => 0.5,       % real portion of C0
   cImaginary    => -0.5,      % imaginary portion of C0
   zReal         => -0.1,      % real portion of Z0 (na Julian)
   zImaginary    => -0.1,      % imaginary portion of Z0 (na Julian)
   xRealRight    => 3.0,
   xRealLeft     => -3.0,
   yImaginaryLow => -3.0,
   yImaginaryHigh => 3.0,
   bailoutThreshold => 4.0,
   maxIterationThreshold => 11 },
  Body = jiffy:encode(FractalConfig),
  Options = #{},
  {ok, Response} = shotgun:post(Conn, "/sFractal", Headers, Body, Options),
  ct:pal("Response = ~p", [Response]),
  ok.
