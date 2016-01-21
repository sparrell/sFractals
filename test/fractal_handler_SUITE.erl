%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%%
%%%-------------------------------------------------------------------

-module(fractal_handler_SUITE).
-author("Duncan Sparrell").

%% for test
-compile(export_all).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% tests to run
all() ->
    [ test_post
    ].

%% timeout if no reply in a minute
suite() ->
    [{timetrap,{minutes,2}}].

%% setup config parameters
init_per_suite(Config) ->
      {ok, _AppList} = application:ensure_all_started(shotgun),
      {ok, _AppList2} = application:ensure_all_started(sFractals),

      Config.

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
  #{ status_code := 201, headers := RespHeaders } = Response,
  %% test header contents are correct
  { <<"server">>, <<"Cowboy">>} =  lists:keyfind(<<"server">>, 1, RespHeaders),
  { <<"date">>, _Date } =  lists:keyfind(<<"date">>, 1, RespHeaders),
  { <<"content-length">>, <<"0">>} =  lists:keyfind(<<"content-length">>, 1, RespHeaders),
  { <<"content-type">>, <<"text/html">>} =  lists:keyfind(<<"content-type">>, 1, RespHeaders),
  FractalLink = <<"http://localhost:8080/images/testpost.png">>,
  { <<"location">>, FractalLink } =  lists:keyfind(<<"location">>, 1, RespHeaders),

  %% test file got created and is right size 
  %%   really should have deleted it first to make sure this is new
  {ok, ImResponse} = shotgun:get(Conn, "/images/testpost.png"),
  %% check status code correct and get body and headers for further tests
  #{ status_code := 200, headers := ImHeaders, body := ImBody } = ImResponse,
  %% check headers
  { <<"server">>, <<"Cowboy">>} =  lists:keyfind(<<"server">>, 1, ImHeaders),
  { <<"date">>, _Date } =  lists:keyfind(<<"date">>, 1, ImHeaders),
  { <<"content-length">>, <<"145">>} =  lists:keyfind(<<"content-length">>, 1, ImHeaders),
  { <<"content-type">>, <<"image/png">>} =  lists:keyfind(<<"content-type">>, 1, ImHeaders),

  %% check body
  ImBody = <<137,80,78,71,13,10,26,10,0,0,0,13,73,72,68,82,0,0,0,10,0,0,0,10,8,
            3,0,0,0,186,236,63,143,0,0,0,36,80,76,84,69,255,255,255,0,255,255,
            0,0,255,255,0,0,0,255,0,255,255,0,210,180,140,240,128,128,255,165,
            0,128,128,0,255,0,255,0,0,0,205,64,91,207,0,0,0,40,73,68,65,84,120,
            156,99,96,192,5,24,129,0,202,98,102,101,129,178,25,153,88,216,152,
            96,76,54,22,24,147,145,133,149,153,17,93,27,22,0,0,15,120,0,71,22,
            14,92,252,0,0,0,0,73,69,78,68,174,66,96,130>>,

  ok.
