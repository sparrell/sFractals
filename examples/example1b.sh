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

    % create the data
    AnnotatedData = fractalHelpers:computeAllRowsOfFractalData( ConfigMap ),
    SortedAnnotatedData = lists:sort(AnnotatedData),

    io:format("SortedAnnotatedData ~n~p~n",[SortedAnnotatedData]),

    %% make image from data
    %%simpleFractal:makePngFromData(Rows,ConfigMap),

    io:format("Done~n"),
    ok.

