-module(make_image_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([all/0,suite/0,init_per_suite/1]).

%% Test cases
-export([testAnalyze/1,makePng/1,testMakePng/1]).

all() ->
    [testAnalyze,makePng,testMakePng].

suite() -> 
    [{timetrap,{minutes,1}}].

init_per_suite(Config) -> 
    %% standardize test data in config so can reuse in multiple tests (eventually)
    %% return config with new test data added
    [ {someAtom,someValue} | Config ].

testAnalyze(_Config) ->
    % create some test data and check historgram comes out right
    TestCountData = [ {100,101,0}, {102,103,1}, {105,106,1}, {103,102,1}, {100,106,2} ],
    [{0,1},{1,3},{2,1}] = imagelib:analyzeData( TestCountData ),
    ok.

makePng(_Config) ->
    %% create some test data and config
    %% need to do

    ConfigMap = #{ fractalImageFileName => "firstPng.png", %image file created
                  width => 150, 
                  height => 100,
                  colorAlg => simplest 
                },

    imagelib:makePng( ConfigMap ),
    ok.

testMakePng(_Config) ->
    %% create some test data and config
    TestFileName = "secondPng.png",
    TestFileSize = 189,
    ConfigMap = #{ fractalImageFileName => TestFileName, %image file created
                  width => 120, 
                  height => 100,
                  colorAlg => simplest 
                },
    %% create a row of data of 12 bars of color, each 10 pixels wide
    RowData = lists:duplicate(10, 0) 
           ++ lists:duplicate(10, 1)
           ++ lists:duplicate(10, 2)
           ++ lists:duplicate(10, 3)
           ++ lists:duplicate(10, 4)
           ++ lists:duplicate(10, 5)
           ++ lists:duplicate(10, 6)
           ++ lists:duplicate(10, 7)
           ++ lists:duplicate(10, 8)
           ++ lists:duplicate(10, 9)
           ++ lists:duplicate(10, 10)
           ++ lists:duplicate(10, 11),

    %% get started
    Png = imagelib:startPng( ConfigMap ),

    %% add 100 rows
    addRows(RowData, Png),

    %% finish up
    imagelib:finishPng(Png), 

    %% test file got created and is right size
    %%    maybe add exact match later
    { ok, { file_info, OutputFileSize, _reg, _rw, _t1, _t2, _t3, _, _, _, _, _, _, _ } } =
            file:read_file_info(TestFileName),
    TestFileSize  = OutputFileSize,

    ok.

addRows(RowData, Png) -> 
    addRow(RowData, Png, 0).

%% done when height of data is height finished
addRow(_RowData, #{size := {_Width, Height}}, Height) ->
    ok;

%% if got to this clause, add a row and recurse
addRow(RowData, #{size := {_Width, _Height}} = Png, Y) ->
    png:append(Png, {row, RowData}),
    addRow(RowData, Png, Y + 1).
