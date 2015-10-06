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

    % example 6

    %% base config
    ConfigMap = #{ fractalAlg => julian,  % Fractal Algorithm is julian
                    fractalImageFileName => "../examples/example6.png",  %image file created
                    dataFileName  => "../examples/example6.erltxt",  %put data here
                    colorAlg => simplest,  % 0-11 map to colors
                    width => 10,           % width=10
                    height => 10,          % height=10
                    cReal => 0.5,          % real portion of C0
                    cImaginary => -0.5,    % imaginary portion of C0
                    zReal => -0.1,         % real portion of Z0 (na Julian)
                    zImaginary => -0.1,    % imaginary portion of Z0 (na Julian)
                    xRealRight => 3.0,
                    xRealLeft => -3.0,
                    yImaginaryLow => -3.0,
                    yImaginaryHigh => 3.0,
                    bailoutThreshold => 4,
                    maxIterationThreshold => 11 },

    %% uncomment %~ to give staus io
    %~statistics(runtime),
    %~statistics(wall_clock),

    % create the data and put in file
    %~io:format("computing data~n"),
    ok = simpleFractal:computeFractalDataIntoFile( ConfigMap ),

    %% make image from data
    %~io:format("rendering image~n"),
%%    simpleFractal:makePngFromData(Rows,ConfigMap),

    %~{_, TimeRun} = statistics(runtime),
    %~{_, TimeWall} = statistics(wall_clock),
    %~SecRun = TimeRun / 1000.0,
    %~SecWall = TimeWall / 1000.0,
    %~io:format("    ~p runtime seconds~n    ~p wall clock seconds~n",
    %~          [SecRun, SecWall]),

    ok.

