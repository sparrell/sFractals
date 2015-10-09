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

    % example 4

    %% base config
    ConfigMap = #{ fractalAlg => julian,  % Fractal Algorithm is julian
                    fractalImageFileName => "../examples/example4.png",  %image file created
                    colorAlg => simple16,  % 0-15 map to colors
                    width => 4000,           % width=4k
                    height => 4000,          % height=4k
                    cReal => 0.5,          % real portion of C0
                    cImaginary => -0.5,    % imaginary portion of C0
                    zReal => -0.1,         % real portion of Z0 (na Julian)
                    zImaginary => -0.1,    % imaginary portion of Z0 (na Julian)
                    xRealRight => 1.0,
                    xRealLeft => -1.0,
                    yImaginaryLow => 0.0,
                    yImaginaryHigh => 2.0,
                    bailoutThreshold => 4,
                    maxIterationThreshold => 15 },

    %% uncomment %~ to give staus io
    %~statistics(runtime),
    %~statistics(wall_clock),

    % create the data
    %~io:format("computing data~n"),
    Rows = simpleFractal:computeFractalData( ConfigMap ),

    %% make image from data
    %~io:format("rendering image~n"),
    simpleFractal:makePngFromData(Rows,ConfigMap),

    %~{_, TimeRun} = statistics(runtime),
    %~{_, TimeWall} = statistics(wall_clock),
    %~SecRun = TimeRun / 1000.0,
    %~SecWall = TimeWall / 1000.0,
    %~io:format("    ~p runtime seconds~n    ~p wall clock seconds~n",
    %~          [SecRun, SecWall]),

    ok.

