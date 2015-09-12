-module(make_image_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([all/0,suite/0,init_per_suite/1]).

%% Test cases
-export([make1/1]).

all() ->
    [make1].

suite() -> 
    [{timetrap,{minutes,1}}].

init_per_suite(Config) -> 
    %% standardize test data in config so can reuse in multiple tests (eventually)
    TestFileName = "./testImage.png",
    TestFileSize = 329,
    Red = egd:color( {255,0,0} ),
    Green = egd:color( {0,255,0} ),
    Blue = egd:color( {0,0,255} ),
    Black = egd:color( {0,0,0} ),
    Width = 150,
    Height = 100,
    TestColorData = [ {X,Y,Red} || X <- lists:seq(1, 20), Y <- lists:seq(1, 10) ] ++
        [ {X,Y,Blue} || X <- lists:seq(21, 30), Y <- lists:seq(11, 30) ] ++
        [ {X,Y,Green} || X <- lists:seq(31, 40), Y <- lists:seq(31, 40) ] ++
        [ {41,41,Black} ],

    %% return config with new test data added
    [ {testFileName,TestFileName},  {testFileSize,TestFileSize},
        {width, Width}, {height,Height}, 
        {testColorData,TestColorData} | Config ].

make1(Config) ->
    %% create a blank image with some test data on it

    %% get config data from init
    TestFileName  = ?config(testFileName,Config),
    TestFileSize  = ?config(testFileSize,Config),
    Width         = ?config(width, Config),
    Height        = ?config(height, Config),
    TestColorData = ?config(testColorData, Config),

    % make a image from test data
    imagelib:makeImageFromData( TestColorData, Width, Height, TestFileName ),

    % check file got created and is correct size
    { ok, { file_info, OutputFileSize, _reg, _rw, _t1, _t2, _t3, _, _, _, _, _, _, _ } } =file:read_file_info(TestFileName),
    TestFileSize = OutputFileSize,

    ok.


