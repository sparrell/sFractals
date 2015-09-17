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
    [testNewImaginaryC].

%% timeout if no reply in a minute
suite() -> 
    [{timetrap,{minutes,1}}].

%% setup config parameters
init_per_suite(Config) -> 
    [ {someatom,someterm}, { another,value } | Config ].

testNewImaginaryC(_) ->
    FractalAlg = julian,
    CReal = 0.5,
    CImaginary = 0.6,
    ZReal = 0.7,
    ZImaginary = 0.8,
    0.6 = simpleFractal:newImaginaryC({FractalAlg,CReal,CImaginary, ZReal,ZImaginary}),
    CImaginary2 = -2.0,
    -2.0 = simpleFractal:newImaginaryC({FractalAlg,CReal,CImaginary2, ZReal,ZImaginary}),
    CImaginary3 = 0.0,
    0.0 = simpleFractal:newImaginaryC({FractalAlg,CReal,CImaginary3, ZReal,ZImaginary}),
    ok.




