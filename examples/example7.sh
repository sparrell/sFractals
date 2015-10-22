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

    % example 7

    %% base config
   {ok, [ ConfigMap | _T ] } = file:consult("../examples/example7.ecfg"),

    %% uncomment %~ to give staus io
    %~statistics(runtime),
    %~statistics(wall_clock),

    % create the data
    %~io:format("computing data~n"),
    ok = simpleFractal:computeFractalDataIntoFile( ConfigMap ),

    %% make image from data
    %~io:format("rendering image~n"),
    simpleFractal:makePngFromDataFile(ConfigMap),

    %~{_, TimeRun} = statistics(runtime),
    %~{_, TimeWall} = statistics(wall_clock),
    %~SecRun = TimeRun / 1000.0,
    %~SecWall = TimeWall / 1000.0,
    %~io:format("    ~p runtime seconds~n    ~p wall clock seconds~n",
    %~          [SecRun, SecWall]),

    ok.

