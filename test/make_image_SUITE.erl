-module(make_image_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([all/0,suite/0,init_per_suite/1]).

%% Test cases
-export([make1/1,testAnalyze/1,makePng/1]).

all() ->
    [make1,testAnalyze,makePng].

suite() -> 
    [{timetrap,{minutes,1}}].

init_per_suite(Config) -> 
    %% standardize test data in config so can reuse in multiple tests (eventually)
    %% return config with new test data added
    [ {someAtom,someValue} | Config ].

make1(_Config) ->
    %% create a blank image with some test data on it

    %% set up some test data and expectations
    
    TestFileName = "./testImage.png",
    TestFileSize = 329,
    Red = egd:color( {255,0,0} ),
    Green = egd:color( {0,255,0} ),
    Blue = egd:color( {0,0,255} ),
    Black = egd:color( {0,0,0} ),
    TestColorData = [ {X,Y,Red} || X <- lists:seq(1, 20), Y <- lists:seq(1, 10) ] ++
        [ {X,Y,Blue} || X <- lists:seq(21, 30), Y <- lists:seq(11, 30) ] ++
        [ {X,Y,Green} || X <- lists:seq(31, 40), Y <- lists:seq(31, 40) ] ++
        [ {41,41,Black} ],

    %% map of image config parameters
    ImageMap = #{ fractalImageFileName => TestFileName, %image file created
                  width => 150, % width=10
                  height => 100 % height=10
                },


    % make a image from test data
    imagelib:makeImageFromData( TestColorData, ImageMap),

    %% check file got created and is correct size
    { ok, { file_info, OutputFileSize, _reg, _rw, _t1, _t2, _t3, _, _, _, _, _, _, _ } } =file:read_file_info(TestFileName),
    TestFileSize = OutputFileSize,

    %% compare actual file contents

    %% reference file contents 
    ReferenceFileContents = <<137,80,78,71,13,10,26,10,0,0,0,13,73,72,68,82,0,0,0,150,0,0,
                   0,100,8,2,0,0,0,235,57,94,167,0,0,1,16,73,68,65,84,120,156,
                   237,216,193,9,128,48,16,69,193,237,191,105,5,11,240,160,9,
                   225,193,12,75,10,248,239,150,185,136,155,231,153,239,199,
                   105,18,230,73,152,39,97,158,132,121,18,230,73,152,39,97,158,
                   132,121,18,230,253,109,160,239,113,18,230,73,152,39,97,158,
                   132,121,18,230,73,152,39,97,158,132,121,18,230,73,152,39,97,
                   158,132,121,18,230,73,152,39,97,158,132,121,18,230,73,152,
                   39,97,222,222,153,95,251,42,188,134,132,121,18,230,73,152,
                   39,97,158,132,121,18,230,73,152,39,97,158,132,121,39,119,28,
                   95,112,43,24,49,79,194,60,9,243,36,204,147,48,79,194,60,9,
                   243,36,204,147,48,79,194,60,9,243,36,204,147,48,79,194,60,9,
                   243,36,204,147,48,79,194,60,9,243,36,204,147,48,79,194,60,9,
                   243,36,204,147,48,79,194,60,9,243,36,204,147,48,79,194,60,9,
                   243,36,204,147,48,79,194,60,9,243,36,204,147,48,79,194,60,9,
                   243,36,204,147,48,79,194,60,9,243,36,204,147,48,79,194,60,9,
                   243,36,204,147,48,79,194,60,9,243,36,204,147,48,79,194,60,9,
                   243,36,204,147,48,79,194,188,27,85,50,59,41,50,152,213,156,
                   0,0,0,0,73,69,78,68,174,66,96,130>>,

    %% See if test produced file with same contents
    {ok,TestFileContents}=file:read_file(TestFileName),
    ReferenceFileContents=TestFileContents,
    ok.


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
