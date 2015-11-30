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

    %% example 08a 
    %% note 2x deep, 2x wide, 2x high

    %% base config
    {ok, [ ConfigMap | _T ] } = file:consult("../examples/Config/example08b1.ecfg"),

    % create the data and put into a file
    ok = simpleFractal:computeFractalDataIntoFile( ConfigMap ),

    %% make image from data
    simpleFractal:makePngFromDataFile(ConfigMap),

    ok.

