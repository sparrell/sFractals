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
     testExceedIter, testExceedBound, testAddOnePoint, testMakePointsEnd,
     testMakePoints, testComputeFractalData2Finish,
     testComputeFractalData2EOL, testComputeFractalData2addRow ].

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
    % test adding point at (13,14) of 2
    [{13,14,2}] = simpleFractal:addOnePoint(CurrentPixelX,
                               CurrentPixelY,
                               XReal,
                               YImaginary,
                               FractalAlg,
                               IterCounts,
                               ConfigMap),
    ok.

testMakePointsEnd(_Config) ->
    % test end of line
    [ {1,2,3} ] = simpleFractal:makePoints(10,    % width = 10
                                           11,      % CurrentPixelX = 11 (ie greater than width)
                                           2,       % CurrentPixelY = don't care for this test
                                           3.0,     % CurrentRealX = don't care for this test
                                           4.0,     % CurrentImaginaryY = don't care for this test
                                           0.2,     % DeltaX = don't care for this test
                                           [ {1,2,3} ], % IterCounts = test data = output unchanged
                                           #{}),    % ConfigMap = don't care for this test
    ok.

testMakePoints(Config) ->
    % initialize some test data
    ConfigMap = #{ fractalAlg => ?config(fractalAlg, Config), 
                   cReal => ?config(cReal, Config),
                   cImaginary => ?config(cImaginary, Config),
                   bailoutThreshold => ?config(bailoutThreshold, Config),
                   maxIterationThreshold => ?config(maxIterationThreshold, Config) },
    % make a valid row
    [{3,2,2},{2,2,3},{1,2,5}] = simpleFractal:makePoints(3,    % width = 3
                                 1,      % CurrentPixelX = 1 = start at begining of line
                                 2,       % CurrentPixelY 
                                 0.3,     % CurrentRealX 
                                 0.4,     % CurrentImaginaryY 
                                 0.2,     % DeltaX 
                                 [ ],      % IterCounts = start empty
                                 ConfigMap),    
    ok.

testComputeFractalData2Finish(_Config) ->
    %% test finish clause

    %% setup some test config and test data
    TestConfig1 = #{ fractalAlg => julian,  % Fractal Algorithm is julian
       fractalImageFileName => './FinishTest.png',  %image file created
       width => 10, % width=10
       height => 5, 
       cReal => 0.5, % real portion of C0
       cImaginary => -0.5, % imaginary portion of C0
       zReal => -0.1, %real portion of Z0 (don't care for Julian)
       zImaginary => -0.1, %imaginary portion of Z0 (don't care for Julian)
       xRealRight => 3.0,
       xRealLeft => -3.0,
       yImaginaryLow => -3.0,
       yImaginaryHigh => 3.0,
       bailoutThreshold => 4,
       maxIterationThreshold => 11 },

    % data that doesn't matter for this test
    ThisRow = [ ],
    XPix = 3,
    XR = 0.5,
    DeltaX = 0.1,
    Width = 10,
    YI = 0.5,
    DeltaY = 0.1,

    % data that matters for the test
    RowsIn = [ [0],[0],[0],[0],[0] ],  % has five rows
    YPix = 0,
    Height = 5,

    % see if computeFractalData correctly comes back when rows are full
    RowsOut = simpleFractal:computeFractalData( RowsIn, ThisRow,
               XPix, XR, DeltaX, Width,  
               YPix, YI, DeltaY, Height,  % only height matters
               TestConfig1),

    % test correct answer came back
    RowsIn = RowsOut.

testComputeFractalData2EOL(_Config) ->
    % test end of row works correctly

    %% setup some test config and test data
    ConfigMap = #{ fractalAlg => julian,  % Fractal Algorithm is julian
       fractalImageFileName => './EolTest.png',  %image file created
       width => 5, % width=10
       height => 5, 
       cReal => 0.5, % real portion of C0
       cImaginary => -0.5, % imaginary portion of C0
       zReal => -0.1, %real portion of Z0 (don't care for Julian)
       zImaginary => -0.1, %imaginary portion of Z0 (don't care for Julian)
       xRealRight => 3.0,
       xRealLeft => -3.0,
       yImaginaryLow => -3.0,
       yImaginaryHigh => 3.0,
       bailoutThreshold => 4,
       maxIterationThreshold => 11 },

    % data that doesn't matter for this test
    XR = 0.5,
    DeltaX = 0.1,
    Width = 5,
    YI = 0.5,
    DeltaY = 0.1,
    Height = 5,

    % data that matters for the test
    RowsIn = [ [0],[0],[0] ],  % has 3 rows
    ThisRow = [ 1,2,3,4,5 ],
    YPix = 1,    % last row (so wont compute more)
    XPix = 0,    % testing reached begin of row

    % see if computeFractalData correctly comes back at end of row
    RowsOut = simpleFractal:computeFractalData( RowsIn, ThisRow,
               XPix, XR, DeltaX, Width,  
               YPix, YI, DeltaY, Height,  % only height matters
               ConfigMap),

    %% is output what was expected?
    [ ThisRow | RowsIn ] = RowsOut.

testComputeFractalData2addRow(_Config) ->
    % test computing a row of data works right
    %% setup some test config and test data
    ConfigMap = #{ fractalAlg => julian,  % Fractal Algorithm is julian
       fractalImageFileName => './EolTest.png',  %image file created
       width => 5, 
       height => 5, 
       cReal => 0.1, % real portion of C0
       cImaginary => -0.1, % imaginary portion of C0
       zReal => -0.1, %real portion of Z0 (don't care for Julian)
       zImaginary => -0.1, %imaginary portion of Z0 (don't care for Julian)
       xRealRight => 0.1,
       xRealLeft => -0.1,
       yImaginaryLow => -3.0,
       yImaginaryHigh => 3.0,
       bailoutThreshold => 4,
       maxIterationThreshold => 11 },

    % data for test computations
    XR = 0.0,
    DeltaX = 0.1,
    Width = 5,
    YI = 0.0,
    DeltaY = 0.1,
    Height = 5,

    RowsIn = [ [0],[0],[0] ],  % has 3 rows
    ThisRow = [ 0,0,0,0,0 ],
    YPix = 2,    % 1 row to go
    XPix = 0,

    % see if computeFractalData computes more data
    RowsOut = simpleFractal:computeFractalData( RowsIn, ThisRow,
               XPix, XR, DeltaX, Width,  
               YPix, YI, DeltaY, Height,  
               ConfigMap),

    %% is output what was expected?
    [[11,11,11,11,11],[0,0,0,0,0],[0],[0],[0]] = RowsOut.

