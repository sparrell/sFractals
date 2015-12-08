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
    [
     testComputeFractalData2EOL,
     testComputeFractalData2addRow,
     testMakeData,
     testMakeDataFile
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

testMakeData(_Config) ->
    %% example config
    FractalImageFileName = "./testMakeData.png",  %image file created
    DataFileName         = "./testMakeData.erl.txt",  %put data here
    ConfigMap = #{ fractalAlg => julian,  % Fractal Algorithm is julian
                   imageFileName => FractalImageFileName,
                   dataFileName         => DataFileName,
                   colorAlg => simplest,  % 0-11 map to colors
                   width => 10,           % width=10
                   height => 10,          % height=10
                   cReal => 0.5,          % real portion of C0
                   cImaginary => -0.5,    % imaginary portion of C0
                   zReal => -0.1,         % real portion of Z0 (na Julian)
                   zImaginary => -0.1,    % imaginary portion of Z0 (na Julian)
                   xRealRight => 3.0,
                   xRealLeft => -3.0,
                   yImaginaryLow => -3.0,
                   yImaginaryHigh => 3.0,
                   bailoutThreshold => 4,
                   maxIterationThreshold => 11 },

    %% reference test data
    RefData = [[0,0,0,0,0,0,0,0,0,0],
               [0,0,0,1,1,1,0,0,0,0],
               [0,0,1,3,4,1,1,0,0,0],
               [0,1,2,4,10,3,1,1,0,0],
               [0,1,1,3,5,3,1,1,0,0],
               [0,1,1,3,10,4,2,1,0,0],
               [0,0,1,1,4,3,1,0,0,0],
               [0,0,0,1,1,1,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0]],
    RefFileSize = 150,
    RefPngData = <<137,80,78,71,13,10,26,10,0,0,0,13,73,72,68,82,0,0,0,10,
                       0,0,0,10,8,3,0,0,0,186,236,63,143,0,0,0,36,80,76,84,69,
                       255,255,255,0,255,255,0,0,255,255,0,0,0,255,0,255,255,
                       0,210,180,140,240,128,128,255,165,0,128,128,0,255,0,
                       255,0,0,0,205,64,91,207,0,0,0,45,73,68,65,84,120,156,
                       99,96,64,1,140,140,140,48,22,51,11,148,205,200,196,194,
                       197,12,97,51,50,50,179,34,152,92,44,76,80,21,140,44,
                       204,140,24,38,96,1,0,20,175,0,86,186,174,33,152,0,0,0,
                       0,73,69,78,68,174,66,96,130>>,

    %% create data 
    RowData = simpleFractal:computeFractalData( ConfigMap ),

    %% is it right data? (compare computed data to reference data)
    RefData = RowData,

    %% make png
    simpleFractal:makePngFromData(RowData, ConfigMap),

    %% is png right size?
    { ok, { file_info, OutputFileSize, _reg,_rw,_t1,_t2,_t3,_,_,_,_,_,_,_} } =
            file:read_file_info(FractalImageFileName),
    RefFileSize  = OutputFileSize,

    %% is png right content?
    { ok, RefPngData} = file:read_file(FractalImageFileName),




    ok.

testMakeDataFile(_Config) ->
    %% example config
    FractalImageFileName = "./testMakeDataFile.png",  %image file created
    DataFileName         = "./testMakeDataFile.erl.txt",  %put data here
    ConfigMap = #{ fractalAlg => julian,  % Fractal Algorithm is julian
                   imageFileName => FractalImageFileName,
                   dataFileName         => DataFileName,
                   colorAlg => simplest,  % 0-11 map to colors
                   width => 10,           % width=10
                   height => 10,          % height=10
                   cReal => 0.5,          % real portion of C0
                   cImaginary => -0.5,    % imaginary portion of C0
                   zReal => -0.1,         % real portion of Z0 (na Julian)
                   zImaginary => -0.1,    % imaginary portion of Z0 (na Julian)
                   xRealRight => 3.0,
                   xRealLeft => -3.0,
                   yImaginaryLow => -3.0,
                   yImaginaryHigh => 3.0,
                   bailoutThreshold => 4,
                   maxIterationThreshold => 11 },

    %% reference test data
    RefData = [ [0,0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0,0],
                [0,0,0,1,1,1,0,0,0,0],
                [0,0,1,1,4,3,1,0,0,0],
                [0,1,1,3,10,4,2,1,0,0],
                [0,1,1,3,5,3,1,1,0,0],
                [0,1,2,4,10,3,1,1,0,0],
                [0,0,1,3,4,1,1,0,0,0],
                [0,0,0,1,1,1,0,0,0,0],
                [0,0,0,0,0,0,0,0,0,0] ],
    RefFileSize = 153,
    RefPngData = <<137,80,78,71,13,10,26,10,0,0,0,13,73,72,68,82,0,0,0,10,
             0,0,0,10,8,3,0,0,0,186,236,63,143,0,0,0,36,80,76,84,69,
             255,255,255,0,255,255,0,0,255,255,0,0,0,255,0,255,255,
             0,210,180,140,240,128,128,255,165,0,128,128,0,255,0,
             255,0,0,0,205,64,91,207,0,0,0,48,73,68,65,84,120,156,
             99,96,192,5,24,25,25,225,44,22,102,70,40,139,153,139,
             133,137,17,202,100,101,134,40,97,100,98,225,130,50,25,
             24,153,89,144,244,49,162,26,201,0,0,17,8,0,86,73,146,
             186,65,0,0,0,0,73,69,78,68,174,66,96,130>>,

    %% create data and put in file
    ok = simpleFractal:computeFractalDataIntoFile( ConfigMap ),

    %% is it right data? (compare test file to reference data)
    { ok, RefData} = file:consult(DataFileName),

    %% make png
    simpleFractal:makePngFromDataFile(ConfigMap),

    %% is png right size?
    { ok, { file_info, OutputFileSize, _reg,_rw,_t1,_t2,_t3,_,_,_,_,_,_,_} } =
            file:read_file_info(FractalImageFileName),
    RefFileSize  = OutputFileSize,

    %% is png right content?
    { ok, RefPngData} = file:read_file(FractalImageFileName),

    ok.

testComputeFractalData2EOL(_Config) ->
    % test end of row works correctly

    %% setup some test config and test data
    ConfigMap = #{ fractalAlg => julian,  % Fractal Algorithm is julian
       imageFileName => './EolTest.png',  %image file created
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
       imageFileName => './EolTest.png',  %image file created
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
