%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%%
%%%-------------------------------------------------------------------

-module(compute_points_SUITE).
-author("Duncan Sparrell").

%% for test
-compile(export_all).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% tests to run
all() ->
    [
      test_new_imaginary_c
    , test_new_real_c
    , test_new_imaginary_z
    , test_new_real_z
    , test_exceed_iter
    , test_exceed_bound
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

test_new_real_c(Config) ->
    {FractalAlg, CReal, CImaginary, ZReal, ZImaginary } = getCZconfig(Config),
    0.5  = compute_points:new_real_c({ FractalAlg
                                     , CReal
                                     , CImaginary
                                     , ZReal
                                     , ZImaginary
                                     }),
    -2.0 = compute_points:new_real_c({ FractalAlg
                                     , -2.0
                                     , CImaginary
                                     , ZReal
                                     , ZImaginary
                                     }),
    0.0  = compute_points:new_real_c({ FractalAlg
                                     , 0.0
                                     , CImaginary
                                     , ZReal
                                     , ZImaginary
                                     }),
    ok.

test_new_imaginary_c(Config) ->
    {FractalAlg, CReal, CImaginary, ZReal, ZImaginary } = getCZconfig(Config),
    0.6 = compute_points:new_imaginary_c({ FractalAlg
                                         , CReal
                                         , CImaginary
                                         , ZReal
                                         , ZImaginary
                                         }),
    -2.0 = compute_points:new_imaginary_c({ FractalAlg
                                          , CReal
                                          , -2.0
                                          , ZReal
                                          , ZImaginary
                                          }),
    0.0 = compute_points:new_imaginary_c({ FractalAlg
                                         , CReal
                                         , 0.0
                                         , ZReal
                                         , ZImaginary
                                         }),
    ok.

test_new_imaginary_z(Config) ->
    {FractalAlg, CReal, CImaginary, ZReal, ZImaginary } = getCZconfig(Config),
    %round rather than deal with floating point arith precision issues
    1.72 = round(1000*compute_points:new_imaginary_z({ FractalAlg
                                                     , CReal
                                                     , CImaginary
                                                     , ZReal
                                                     , ZImaginary}))/1000,
    ok.

test_new_real_z(Config) ->
    {FractalAlg, CReal, CImaginary, ZReal, ZImaginary } = getCZconfig(Config),
    0.35 = round(1000*compute_points:new_real_z({ FractalAlg
                                                , CReal
                                                , CImaginary
                                                , ZReal
                                                , ZImaginary}))/1000,
    ok.

test_exceed_iter(Config) ->
    {FractalAlg, CReal, CImaginary, ZReal, ZImaginary } = getCZconfig(Config),
    IterCount =  ?config(iterCount, Config),
    MaxIterationThreshold = ?config(maxIterationThreshold, Config),
    BailoutThreshold = ?config(bailoutThreshold, Config),
    10 = compute_points:compute_iteration_value( FractalAlg
                                               , CReal
                                               , CImaginary
                                               , ZReal
                                               , ZImaginary
                                               , 11             %note IterCountis > Max
                                               , MaxIterationThreshold
                                               , BailoutThreshold
                                               ),
    % now test can go thru an iteration or two without exceeding
    3 = compute_points:compute_iteration_value( FractalAlg
                                              , CReal
                                              , CImaginary
                                              , ZReal
                                              , ZImaginary
                                              , IterCount
                                              , MaxIterationThreshold
                                              , BailoutThreshold
                                              ),
    ok.

test_exceed_bound(Config) ->
    {FractalAlg, CReal, CImaginary, ZReal, ZImaginary } = getCZconfig(Config),
    IterCount =  ?config(iterCount, Config),
    MaxIterationThreshold = ?config(maxIterationThreshold, Config),
    BailoutThreshold = ?config(bailoutThreshold, Config),
    1 = compute_points:compute_iteration_value( FractalAlg
                                              , CReal
                                              , CImaginary
                                              , 100.0 %big so exceed bailout
                                              , ZImaginary
                                              , IterCount           %start at 1
                                              , MaxIterationThreshold
                                              , BailoutThreshold ),
    % now test can go thru wo exceeding (although will eventually)
    3 = compute_points:compute_iteration_value( FractalAlg
                                              , CReal
                                              , CImaginary
                                              , ZReal
                                              , ZImaginary
                                              , IterCount
                                              , MaxIterationThreshold
                                              , BailoutThreshold
                                              ),
    ok.

