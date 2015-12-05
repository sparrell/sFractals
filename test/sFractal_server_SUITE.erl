%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%%
%%%-------------------------------------------------------------------

-module(sFractal_server_SUITE).
-author("Duncan Sparrell").

%% for test
-compile(export_all).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% tests to run
all() ->
    [
      fakeTestOne
      ].

%% timeout if no reply in a minute
suite() ->
    [{timetrap,{minutes,2}}].

%% setup config parameters
init_per_suite(Config) ->
    [ {fractalAlg,julian},
      {cReal,0.5 },
      {cImaginary,0.6 },
      {zReal,0.7 },
      {zImaginary,0.8 },
      {iterCount, 1},
      {maxIterationThreshold, 10},
      {bailoutThreshold, 4 }
      | Config ].

%% init some values - returns { FractalAlg, CReal, CImaginary, ZReal, ZImaginary }
getCZconfig(Config) ->
    { ?config(fractalAlg,Config),
      ?config(cReal, Config),
      ?config(cImaginary, Config),
      ?config(zReal, Config),
      ?config(zImaginary, Config) }.

fakeTestOne(Config) ->
     %% some meaningless tests as placeholder
     CZconfig = getCZconfig(Config),
     A = ?config(cReal, Config),
     B = ?config(zReal, Config),
     A < B,
     ok.
