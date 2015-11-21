#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/test/lib/sFractals/ebin ../_build/test/lib/png/ebin
%% assumes running out of _build
%% output ../examples

%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-author("Duncan Sparrell").
-mode(compile).

main(_) ->

    % example 5b

    %% base config
    {ok, [ ConfigMap | _T ] } = file:consult("../examples/example5b.ecfg"),


    % create the data
    %~io:format("computing data~n"),
    AnnotatedData = lists:sort(fractalHelpers:computeAllRowsOfFractalData( ConfigMap )) ,

    %% make image from data
    %~io:format("rendering image~n"),
    simpleFractal:makePngFromData(Rows,ConfigMap),

    ok.

