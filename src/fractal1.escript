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
    %base config
    ConfigMap1 = config1(),
    makeFractal( ConfigMap1 ),

    % new file name
    Change1 = #{fractalImageFileName => "./m2.julian.0010.0010.dot5.dot5.4.11.png"},
    ConfigMap1b = maps:merge(ConfigMap1, Change1),
    makeFractal2( ConfigMap1b ),

    Change2 = #{fractalImageFileName => "./m1.julian.0100.0100.dot5.dot5.4.11.png",
                width => 100, 
                height => 100},
    ConfigMap2 = maps:merge(ConfigMap1b, Change2),
    makeFractal( ConfigMap2 ),

    Change3 = #{fractalImageFileName => "./m2.julian.0100.0100.dot5.dot5.4.11.png"},
    ConfigMap2b = maps:merge(ConfigMap2,Change3),
    makeFractal2( ConfigMap2b ),

    %ConfigMap3 = config3(),
    %makeFractal( ConfigMap3 ),

    %ConfigMap3b = maps:put(fractalImageFileName, "./m2.julian.0200.0200.dot5.dot5.4.11.png",ConfigMap3),  % new file name
    %makeFractal2( ConfigMap3b ),

    %ConfigMap4 = config4(),
    %makeFractal( ConfigMap4 ),

    %ConfigMap4b = maps:put(fractalImageFileName, "./m2.julian.1000.1000.dot5.dot5.4.11.png",ConfigMap4),  % new file name
    %makeFractal2( ConfigMap4b ),

    Change4 = #{fractalImageFileName => "./m2.julian.1k.1k.dot5.dot5.4.11.png",
                width  => 1000, 
                height => 1000},
    ConfigMap4 = maps:merge(ConfigMap2b,Change4),
    makeFractal2( ConfigMap4 ),

    Change5 = #{fractalImageFileName => "./m2.julian.4k.4k.dot5.dot5.4.11.png",
                width  => 4000, 
                height => 4000},
    ConfigMap5 = maps:merge(ConfigMap4,Change5),
    makeFractal2( ConfigMap5 ),

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
       fractalImageFileName => "./m1.julian.0010.0010.dot5.dot5.4.11.png",  %image file created
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
       maxIterationThreshold => 11 }.

%% config 2 
config2() ->
    % return map of config parameters
    #{ fractalAlg => julian,  % Fractal Algorithm is julian
       fractalImageFileName => "./m1.julian.0100.0100.dot5.dot5.4.11.png",  %image file created
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
       fractalImageFileName => "./m1.julian.0200.0200.dot5.dot5.4.11.png",  %image file created
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
       fractalImageFileName => "./m1.julian.1000.1000.dot5.dot5.4.11.png",  %image file created
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

