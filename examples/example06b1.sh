#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/test/lib/sFractals/ebin ../_build/test/lib/png/ebin
%% assumes running out of _build
%% output image ../examples/example6.png
%% output data ../examples/example6.erltxt

%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-author("Duncan Sparrell").
-mode(compile).

main(_) ->

    % example 06b1

    %% base config
    {ok, [ ConfigMap | _T ] } = file:consult("../examples/Config/example06b1.ecfg"),

    %% uncomment %~ to give staus io
    %~statistics(runtime),
    %~statistics(wall_clock),

    % create the data and put in file
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

