%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%%
%%%-------------------------------------------------------------------

-module(other_compute_SUITE).
-author("Duncan Sparrell").

%% for test
-compile(export_all).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% tests to run
all() ->
    [
      testXList
    , testYList
    , testComputeRowOfFractalData
    , testComputeFractalData
    , testMakePng
    , testMakePngUsingPool
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

testXList(_Config) ->
    %% test XList created OK
    %% setup some test config
    ConfigMap = #{ fractalAlg => julian,  % Fractal Algorithm is julian
       <<"width">> => 5,
       height => 5,
       cReal => 0.1, % real portion of C0
       cImaginary => -0.1, % imaginary portion of C0
       zReal => -0.1, %real portion of Z0 (don't care for Julian)
       zImaginary => -0.1, %imaginary portion of Z0 (don't care for Julian)
       xRealRight => 3.0,
       xRealLeft => -3.0,
       yImaginaryLow => -3.0,
       yImaginaryHigh => 3.0
       },
    [ {1, -3.0}
    , {2, -1.5}
    , {3, 0.0}
    , {4, 1.5}
    , {5, 3.0}
    ] = other_compute:compute_xlist(ConfigMap),
    ok.

testYList(_Config) ->
    %% setup some test config
    ConfigMap = #{ fractalAlg => julian,  % Fractal Algorithm is julian
       <<"width">> => 5,
       height => 5,
       cReal => 0.1, % real portion of C0
       cImaginary => -0.1, % imaginary portion of C0
       zReal => -0.1, %real portion of Z0 (don't care for Julian)
       zImaginary => -0.1, %imaginary portion of Z0 (don't care for Julian)
       xRealRight => 3.0,
       xRealLeft => -3.0,
       yImaginaryLow => -6.0,
       yImaginaryHigh => 6.0
       },
    [ {1,-6.0}
    , {2, -3.0}
    , {3, 0.0}
    , {4, 3.0}
    , {5, 6.0}] = other_compute:compute_ylist(ConfigMap),
    ok.

testComputeRowOfFractalData(_Config) ->
    %% setup some test config
    ConfigMap = #{ fractalAlg => julian,  % Fractal Algorithm is julian
       <<"width">> => 5,
       height => 5,
       cReal => 0.1, % real portion of C0
       cImaginary => -0.1, % imaginary portion of C0
       zReal => -0.1, %real portion of Z0 (don't care for Julian)
       zImaginary => -0.1, %imaginary portion of Z0 (don't care for Julian)
       xRealRight => 1.4,
       xRealLeft => 0.8,
       yImaginaryLow => -0.9,
       yImaginaryHigh => 0.7,
       maxIterationThreshold => 32,
       bailoutThreshold => 4
       },
    %% pick one row and test created correctly
    ExpectedResult = {{3,0}, [{1,0.8000000000000003,32},
                              {2,0.9500000000000002,4},
                              {3,1.1,3},
                              {4,1.25,2},
                              {5,1.4,1}]},
    XList = other_compute:compute_xlist(ConfigMap),
    ExpectedResult = other_compute:compute_row( julian
                                               , {3,0}
                                               , XList
                                               , ConfigMap
                                               ),
    ok.

testComputeFractalData(_Config) ->
    %% setup some test config
    ConfigMap = #{ fractalAlg => julian,  % Fractal Algorithm is julian
       <<"width">> => 5,
       height => 5,
       cReal => 0.1, % real portion of C0
       cImaginary => -0.1, % imaginary portion of C0
       zReal => -0.1, %real portion of Z0 (don't care for Julian)
       zImaginary => -0.1, %imaginary portion of Z0 (don't care for Julian)
       xRealRight => 1.4,
       xRealLeft => 0.8,
       yImaginaryLow => -0.9,
       yImaginaryHigh => 0.7,
       maxIterationThreshold => 32,
       bailoutThreshold => 4
       },
    ExpectedResult =
                 [{{1,-0.9000000000000001},
                   [{1,0.8000000000000003,2},
                    {2,0.9500000000000002,2},
                    {3,1.1,1},
                    {4,1.25,1},
                    {5,1.4,1}]},
                  {{2,-0.5000000000000001},
                   [{1,0.8000000000000003,7},
                    {2,0.9500000000000002,3},
                    {3,1.1,2},
                    {4,1.25,2},
                    {5,1.4,1}]},
                  {{3,-0.10000000000000009},
                   [{1,0.8000000000000003,32},
                    {2,0.9500000000000002,4},
                    {3,1.1,3},
                    {4,1.25,2},
                    {5,1.4,1}]},
                  {{4,0.29999999999999993},
                   [{1,0.8000000000000003,32},
                    {2,0.9500000000000002,32},
                    {3,1.1,3},
                    {4,1.25,2},
                    {5,1.4,1}]},
                  {{5,0.7},
                   [{1,0.8000000000000003,6},
                    {2,0.9500000000000002,3},
                    {3,1.1,2},
                    {4,1.25,1},
                    {5,1.4,1}]}],
    ExpectedResult = other_compute:compute_rows(ConfigMap),
    ok.

testMakePng(_Config) ->
    %% set up some config
    FractalImageFileName = "test.example01c.png",  %image file created
    ConfigMap = #{ fractalAlg => julian,  % Fractal Algorithm is julian
                   imageFileName => FractalImageFileName,
                   colorAlg      => simplest,  % 0-11 map to colors
                   <<"width">>         => 10,        % width=10
                   height        => 10,        % height=10
                   cReal         => 0.5,       % real portion of C0
                   cImaginary    => -0.5,      % imaginary portion of C0
                   zReal         => -0.1,      % real portion of Z0 (na Julian)
                   zImaginary    => -0.1,      % imaginary portion of Z0 (na Julian)
                   xRealRight    => 3.0,
                   xRealLeft     => -3.0,
                   yImaginaryLow => -3.0,
                   yImaginaryHigh => 3.0,
                   bailoutThreshold => 4,
                   maxIterationThreshold => 11,
                   timeout       => 5000 },

    %% set up some expected results
    RefFileSize = 145,
    RefPngData = <<137,80,78,71,13,10,26,10,0,0,0,13,73,72,68,82,0,0,0,10,
                   0,0,0,10,8,3,0,0,0,186,236,63,143,0,0,0,36,80,76,84,69,
                   255,255,255,0,255,255,0,0,255,255,0,0,0,255,0,255,255,
                   0,210,180,140,240,128,128,255,165,0,128,128,0,255,0,
                   255,0,0,0,205,64,91,207,0,0,0,40,73,68,65,84,120,156,
                   99,96,192,5,24,129,0,202,98,102,101,129,178,25,153,88,
                   216,152,96,76,54,22,24,147,145,133,149,153,17,93,27,22,
                   0,0,15,120,0,71,22,14,92,252,0,0,0,0,73,69,78,68,174,
                   66,96,130>>,

    %% create fractal data, make image,  and put image into a file
    ok = other_compute:make_png(ConfigMap),

    %% is png right size?
    { ok, { file_info, OutputFileSize, _reg,_rw,_t1,_t2,_t3,_,_,_,_,_,_,_} } =
            file:read_file_info(FractalImageFileName),
    RefFileSize  = OutputFileSize,

    %% is png right content?
    { ok, RefPngData} = file:read_file(FractalImageFileName),

    ok.

testMakePngUsingPool(_Config) ->
    %% set up some config
    FractalImageFileName = "test.example01d.png",  %image file created
    ConfigMap = #{ fractalAlg => julian,  % Fractal Algorithm is julian
                   imageFileName => FractalImageFileName,
                   colorAlg      => simplest,  % 0-11 map to colors
                   <<"width">>         => 10,        % width=10
                   height        => 10,        % height=10
                   cReal         => 0.5,       % real portion of C0
                   cImaginary    => -0.5,      % imaginary portion of C0
                   zReal         => -0.1,      % real portion of Z0 (na Julian)
                   zImaginary    => -0.1,      % imaginary portion of Z0 (na Julian)
                   xRealRight    => 3.0,
                   xRealLeft     => -3.0,
                   yImaginaryLow => -3.0,
                   yImaginaryHigh => 3.0,
                   bailoutThreshold => 4,
                   maxIterationThreshold => 11,
                   timeout       => 5000 },

    %% set up some expected results
    RefFileSize = 145,
    RefPngData = <<137,80,78,71,13,10,26,10,0,0,0,13,73,72,68,82,0,0,0,10,
                   0,0,0,10,8,3,0,0,0,186,236,63,143,0,0,0,36,80,76,84,69,
                   255,255,255,0,255,255,0,0,255,255,0,0,0,255,0,255,255,
                   0,210,180,140,240,128,128,255,165,0,128,128,0,255,0,
                   255,0,0,0,205,64,91,207,0,0,0,40,73,68,65,84,120,156,
                   99,96,192,5,24,129,0,202,98,102,101,129,178,25,153,88,
                   216,152,96,76,54,22,24,147,145,133,149,153,17,93,27,22,
                   0,0,15,120,0,71,22,14,92,252,0,0,0,0,73,69,78,68,174,
                   66,96,130>>,

    %% create fractal data, make image,  and put image into a file
    ok = other_compute:make_png_using_pool(ConfigMap),

    %% is png right size?
    { ok, { file_info, OutputFileSize, _reg,_rw,_t1,_t2,_t3,_,_,_,_,_,_,_} } =
            file:read_file_info(FractalImageFileName),
    RefFileSize  = OutputFileSize,

    %% is png right content?
    { ok, RefPngData} = file:read_file(FractalImageFileName),

    ok.
