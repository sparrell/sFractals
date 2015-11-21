#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/test/lib/sFractals/ebin ../_build/test/lib/png/ebin
%% assumes running out of _build
%% output image ../examples directory

%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-author("Duncan Sparrell").
-mode(compile).

main(_) ->

    % example 1b

    %% base config
    {ok, [ ConfigMap | _T ] } = file:consult("../examples/example1b.ecfg"),
    FractalAlg = maps:get(fractalAlg,ConfigMap),
    XList = fractalHelpers:computeXList(ConfigMap),
    YList = fractalHelpers:computeYList(ConfigMap),

    %% initialize the png
    ThisPng = imagelib:startPng( ConfigMap ),

    %% add all the rows, one at a time, to the image
    addRows(FractalAlg, ThisPng, XList, YList, ConfigMap),

    %% finalize the png
    imagelib:finishPng( ThisPng ),

    % create the data
    %%AnnotatedData = fractalHelpers:computeAllRowsOfFractalData( ConfigMap ),
    %%SortedAnnotatedData = lists:sort(AnnotatedData),

    %%io:format("SortedAnnotatedData ~n~p~n",[SortedAnnotatedData]),

    %% make image from data
    %%simpleFractal:makePngFromData(Rows,ConfigMap),

    io:format("Done~n"),
    ok.

addRows(_FractalAlg, _ThisPng, _XList, YList, _ConfigMap)
        when YList == [] ->
    %% no rows left to compute so complete
    ok;

addRows(FractalAlg, ThisPng, XList, YList, ConfigMap) ->
    %% otherwise pop off a row, compute fractal data, add to png, recurse

    %% pop off a row
    [ {PixelY,ImgY} | NewYList ] = YList,

    %% compute row of fractal data
         RowOfFractalData = fractalHelpers:computeRowOfFractalData(FractalAlg, {PixelY,ImgY},XList,ConfigMap),
         io:format("{~p,~p} Row: ~p~n", [PixelY,ImgY,RowOfFractalData]),
         ThisRowDataOnly = [ C || {_P,_I,C} <- RowOfFractalData ],
         io:format("Data: ~p~n",[ThisRowDataOnly]),

    %% add to png
    imagelib:addRow( ThisRowDataOnly, ThisPng ),

    %% recurse
    addRows(FractalAlg, ThisPng, XList, NewYList, ConfigMap).

