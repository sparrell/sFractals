#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa /Users/duncan/MyDev/GitHub/sFractals/_build/test/lib/sFractals/ebin /Users/duncan/MyDev/GitHub/sFractals/_build/test/lib/png/ebin

%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-author("Duncan Sparrell").
-mode(compile).

main(_) ->
    makeFractal2( config1() ),
    %makeFractal2( config2() ),
    %makeFractal2( config3() ),
    %%makeFractal2( config4() ),
    %makeFractal( config1() ),
    %makeFractal( config2() ),
    %makeFractal( config3() ),
    %%makeFractal( config4() ),
    ok.

% time making each fractal
makeFractal2(Config) ->
    io:format("Rendering2 ~s ~n", [ maps:get(fractalImageFileName,Config)] ),
    statistics(runtime),
    statistics(wall_clock),
    simpleFractal:makeFractalPng( Config ),
    {_, TimeRun} = statistics(runtime),
    {_, TimeWall} = statistics(wall_clock),
    SecRun = TimeRun / 1000.0,
    SecWall = TimeWall / 1000.0,
    io:format("    ~p runtime seconds~n    ~p wall clock seconds~n",
              [SecRun, SecWall]),
    ok.

% time making each fractal
makeFractal(Config) ->
    io:format("Rendering ~s ~n", [ maps:get(fractalImageFileName,Config)] ),
    statistics(runtime),
    statistics(wall_clock),
    simpleFractal:simplFrac( Config ),
    {_, TimeRun} = statistics(runtime),
    {_, TimeWall} = statistics(wall_clock),
    SecRun = TimeRun / 1000.0,
    SecWall = TimeWall / 1000.0,
    io:format("    ~p runtime seconds~n    ~p wall clock seconds~n",
              [SecRun, SecWall]),
    ok.


%% config 1 
config1() ->
    % return map of config parameters
    #{ fractalAlg => julian,  % Fractal Algorithm is julian
       fractalImageFileName => "./julian.0010.0010.dot5.dot5.4.100.png",  %image file created
       colorAlg => simplest,  % 0-11 map to colors
       width => 10,           % width=10
       height => 10,          % height=10
       cReal => 0.5,          % real portion of C0
       cImaginary => -0.5,    % imaginary portion of C0
       zReal => -0.1,         %real portion of Z0 (don't care for Julian)
       zImaginary => -0.1,    %imaginary portion of Z0 (don't care for Julian)
       xRealRight => 3.0,
       xRealLeft => -3.0,
       yImaginaryLow => -3.0,
       yImaginaryHigh => 3.0,
       bailoutThreshold => 4,
       maxIterationThreshold => 100 }.

%% config 2 
config2() ->
    % return map of config parameters
    #{ fractalAlg => julian,  % Fractal Algorithm is julian
       fractalImageFileName => "./julian.0100.0100.dot5.dot5.4.11.png",  %image file created
       colorAlg => simplest,  % 0-11 map to colors
       width => 100, % width=10
       height => 100, % height=10
       cReal => 0.5, % real portion of C0
       cImaginary => -0.5, % imaginary portion of C0
       zReal => -0.1, %real portion of Z0 (don't care for Julian)
       zImaginary => -0.1, %imaginary portion of Z0 (don't care for Julian)
       xRealRight => 3.0,
       xRealLeft => -3.0,
       yImaginaryLow => -3.0,
       yImaginaryHigh => 3.0,
       bailoutThreshold => 4,
       maxIterationThreshold => 11 }.

%% config 3 
config3() ->
    % return map of config parameters
    #{ fractalAlg => julian,  % Fractal Algorithm is julian
       fractalImageFileName => "./julian.0200.0200.dot5.dot5.4.11.png",  %image file created
       colorAlg => simplest,  % 0-11 map to colors
       width => 200, % width=10
       height => 200, % height=10
       cReal => 0.5, % real portion of C0
       cImaginary => -0.5, % imaginary portion of C0
       zReal => -0.1, %real portion of Z0 (don't care for Julian)
       zImaginary => -0.1, %imaginary portion of Z0 (don't care for Julian)
       xRealRight => 3.0,
       xRealLeft => -3.0,
       yImaginaryLow => -3.0,
       yImaginaryHigh => 3.0,
       bailoutThreshold => 4,
       maxIterationThreshold => 11 }.

%% config 4 
config4() ->
    % return map of config parameters
    #{ fractalAlg => julian,  % Fractal Algorithm is julian
       fractalImageFileName => "./julian.1000.1000.dot5.dot5.4.11.png",  %image file created
       colorAlg => simplest,  % 0-11 map to colors
       width => 1000, % 
       height => 1000, % 
       cReal => 0.5, % real portion of C0
       cImaginary => -0.5, % imaginary portion of C0
       zReal => -0.1, %real portion of Z0 (don't care for Julian)
       zImaginary => -0.1, %imaginary portion of Z0 (don't care for Julian)
       xRealRight => 2.0,
       xRealLeft => -2.0,
       yImaginaryLow => -2.0,
       yImaginaryHigh => 2.0,
       bailoutThreshold => 4,
       maxIterationThreshold => 11 }.

