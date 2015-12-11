-module(make_image_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([all/0,suite/0,init_per_suite/1]).

%% Test cases
-export([testAnalyze/1,makePng/1,testMakePng/1,testPalette/1]).

all() ->
    [testAnalyze,makePng,testMakePng,testPalette].

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

    ConfigMap = #{ imageFileName => "firstPng.png", %image file created
                  <<"width">> => 150, 
                  height => 100,
                  colorAlg => simplest 
                },

    imagelib:makePng( ConfigMap ),
    ok.

testMakePng(_Config) ->
    %% create some test data and config
    TestFileName = "secondPng.png",
    TestFileSize = 189,
    ConfigMap = #{ imageFileName => TestFileName, %image file created
                  <<"width">> => 120, 
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

testPalette(_Config) ->
    %% test all the palettes load and are used (even if not used completely)
    %% create some test data and config
    PaletteFileSize = [ {simplest,"simplestPalette.png",12},
                        {simplest2,"simplest2Palette.png",12},
                        {simple16,"simple16Palette.png",16},
                        {simple32,"simple32Palette.png",32},
                        {blue32,"blue32Palette.png",32},
                        {simple64,"simple64Palette.png",64} ],

    makeImages(PaletteFileSize).

makeImages([]) ->
    %% empty list so done
    ok;

makeImages( [ {ColorAlg,FileName,PaletteSize} | RestOfConfig ] ) ->

    Width = PaletteSize * 10,
    
    ConfigMap = #{ imageFileName => FileName, 
                  <<"width">> => Width, 
                  height => 100,
                  colorAlg => ColorAlg
                },
    %% create a row of data of PaletteSize bars of color, each 10 pixels wide
    RowData = makeRowData(PaletteSize,[]),

    %% get started
    Png = imagelib:startPng( ConfigMap ),

    %% add height worth of rows
    addRows(RowData, Png),

    %% finish up
    imagelib:finishPng(Png), 

    makeImages( RestOfConfig ).

makeRowData(0,RowData) ->
    %% no colors left so done, return RowData
    RowData;

makeRowData(PaletteSize,RowData) ->
    ColorNum = PaletteSize - 1,
    NewRowData = [ lists:duplicate(10,ColorNum) | RowData ],
    makeRowData(PaletteSize-1,NewRowData).

