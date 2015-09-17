%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-module(simpleFractal_SUITE).
-author("Duncan Sparrell").

%% for test
-compile(export_all).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% tests to run
all() ->
    [testNewImaginaryC,testNewRealC,testNewImaginaryZ,testNewRealZ,
     testExceedIter, testExceedBound, testAddOnePoint].

%% timeout if no reply in a minute
suite() -> 
    [{timetrap,{minutes,1}}].

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

testNewRealC(Config) ->
    {FractalAlg, CReal, CImaginary, ZReal, ZImaginary } = getCZconfig(Config),
    0.5  = simpleFractal:newRealC({FractalAlg,CReal,CImaginary, ZReal,ZImaginary}),
    -2.0 = simpleFractal:newRealC({FractalAlg,-2.0, CImaginary, ZReal,ZImaginary}),
    0.0  = simpleFractal:newRealC({FractalAlg,0.0, CImaginary, ZReal,ZImaginary}),
    ok.

testNewImaginaryC(Config) ->
    {FractalAlg, CReal, CImaginary, ZReal, ZImaginary } = getCZconfig(Config),
    0.6 = simpleFractal:newImaginaryC({FractalAlg,CReal,CImaginary, ZReal,ZImaginary}),
    -2.0 = simpleFractal:newImaginaryC({FractalAlg,CReal,-2.0, ZReal,ZImaginary}),
    0.0 = simpleFractal:newImaginaryC({FractalAlg,CReal,0.0, ZReal,ZImaginary}),
    ok.

testNewImaginaryZ(Config) ->
    {FractalAlg, CReal, CImaginary, ZReal, ZImaginary } = getCZconfig(Config),
    %round rather than deal with floating point arith precision issues
    1.72 = round(1000*simpleFractal:newImaginaryZ({FractalAlg,CReal,CImaginary, ZReal,ZImaginary}))/1000,
    ok.

testNewRealZ(Config) ->
    {FractalAlg, CReal, CImaginary, ZReal, ZImaginary } = getCZconfig(Config),
    0.35 = round(1000*simpleFractal:newRealZ({FractalAlg,CReal,CImaginary, ZReal,ZImaginary}))/1000,
    ok.

testExceedIter(Config) ->
    {FractalAlg, CReal, CImaginary, ZReal, ZImaginary } = getCZconfig(Config),
    IterCount =  ?config(iterCount, Config),
    MaxIterationThreshold = ?config(maxIterationThreshold, Config),
    BailoutThreshold = ?config(bailoutThreshold, Config),
    10 = simpleFractal:computeIterationValue( FractalAlg,
                                               CReal, 
                                               CImaginary, 
                                               ZReal,
                                               ZImaginary, 
                                               11,             %note IterCountis > Max
                                               MaxIterationThreshold, 
                                               BailoutThreshold ),
    % now test can go thru an iteration or two without exceeding
    3 = simpleFractal:computeIterationValue( FractalAlg,
                                               CReal, 
                                               CImaginary, 
                                               ZReal,
                                               ZImaginary, 
                                               IterCount,             
                                               MaxIterationThreshold, 
                                               BailoutThreshold ),
    ok.

testExceedBound(Config) ->
    {FractalAlg, CReal, CImaginary, ZReal, ZImaginary } = getCZconfig(Config),
    IterCount =  ?config(iterCount, Config),
    MaxIterationThreshold = ?config(maxIterationThreshold, Config),
    BailoutThreshold = ?config(bailoutThreshold, Config),
    1 = simpleFractal:computeIterationValue( FractalAlg,
                                               CReal, 
                                               CImaginary, 
                                               100,                 %exceed bailout
                                               ZImaginary, 
                                               IterCount,           %start at 1
                                               MaxIterationThreshold, 
                                               BailoutThreshold ),
    % now test can go thru wo exceeding (although will eventually)
    3 = simpleFractal:computeIterationValue( FractalAlg,
                                               CReal, 
                                               CImaginary, 
                                               ZReal,
                                               ZImaginary, 
                                               IterCount,             
                                               MaxIterationThreshold, 
                                               BailoutThreshold ),
    ok.

testAddOnePoint(Config) ->
    % initialize some test data
    CurrentPixelX = 13,
    CurrentPixelY = 14,
    {FractalAlg, CReal, CImaginary, XReal, YImaginary } = getCZconfig(Config),
    ConfigMap = #{ fractalAlg => FractalAlg, 
       cReal => CReal,
       cImaginary => CImaginary,
       bailoutThreshold => ?config(bailoutThreshold, Config),
       maxIterationThreshold => ?config(maxIterationThreshold, Config) },
    %% start with empty IterCounts
    IterCounts = [ ],
    % test adding pint at (13,14) of 3
    [{13,14,2}] = simpleFractal:addOnePoint(CurrentPixelX,
                               CurrentPixelY,
                               XReal,
                               YImaginary,
                               FractalAlg,
                               IterCounts,
                               ConfigMap),
    ok.

