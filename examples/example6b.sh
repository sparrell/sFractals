#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/test/lib/sFractals/ebin ../_build/test/lib/png/ebin
%% assumes running out of _build
%% output image ../examples

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
                    fractalDataFileName  => "../examples/example6.erl.txt",  % data file created
                    fractalData2FileName => "../examples/example62.erl.txt",  %alt data file created
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

    % create the data
    %~io:format("computing data~n"),
    Rows = simpleFractal:computeFractalData( ConfigMap ),

    %% write a file with row data
    file:write_file(maps:get(fractalDataFileName,ConfigMap),io_lib:fwrite("~p.\n",[Rows])),

    %% try out one line a time
    {ok,Data2File} = file:open(maps:get(fractalData2FileName,ConfigMap),[write]),
    addRow(Data2File,Rows),
    file:close(Data2File),

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

%% finished when list empty
addRow(_DataFile,[]) -> ok;

%% add a row of data to the file
addRow(DataFile, [ H|T ] ) ->
    %% write one row as an erlang term
    io:format(DataFile,"~p.~n",[H]),
    %% recurse
    addRow(DataFile, T).
