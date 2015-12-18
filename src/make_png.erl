%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%%
%%%-------------------------------------------------------------------

-module(make_png).
-author("Duncan Sparrell").
%% given fractal data (in memory or in file), 
%% turn it into a fractal image using png

%% public API
-export([
          make_png_from_data/2,         % create Png from block of fractal data
          make_png_from_file/1          % create Png from file of fractal data
          ]).

%% expose functions for test
-export([ ]).

%% public API for making fractal
make_png_from_file(#{dataFileName := DataFileName } = ConfigMap) ->
    %% get the data
    lager:debug("change make_png_from_file from consult to binary"),
    {ok, Rows} = file:consult( dataFileName ),

    %% make the png
    make_png_from_data(Rows, ConfigMap).


%% create a Png from block of fractal data
make_png_from_data(Rows, ConfigMap) ->
    %% initialize the png
    ThisPng = imagelib:startPng( ConfigMap ),

    %% add all the rows, one at a time, to the image
    addRows(ThisPng, Rows),

    %% finalize the png
    imagelib:finishPng( ThisPng ),

    ok.

addRows(_ThisPng, []) ->
    %% no rows left so done
    ok;

addRows(ThisPng, [ThisRow | RestOfRows ] ) ->
    %% add row to png
    imagelib:addRow( ThisRow, ThisPng ),

    %%recurse
    addRows(ThisPng, RestOfRows).
