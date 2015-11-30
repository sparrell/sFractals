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

    %% example 08b2 
    %% note 2x deep, 2x wide, 2x high

    %% base config
    {ok, [ ConfigMap | _T ] } = file:consult("../examples/Config/example08b2.ecfg"),

    %% data already put into a file in 08b1
    %% make image from data
    simpleFractal:makePngFromDataFile(ConfigMap),

    ok.

