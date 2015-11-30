#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/test/lib/sFractals/ebin ../_build/test/lib/png/ebin
%% assumes running out of _build
%% output image ../examples/example7b.png
%% input data ../examples/example7.erl.txt

%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-author("Duncan Sparrell").
-mode(compile).

main(_) ->

    % example 7b

    %% base config
    {ok, [ ConfigMap | _T ] } = file:consult("../examples/Config/example07b2.ecfg"),

    %% make image from data stored previously
    simpleFractal:makePngFromDataFile(ConfigMap),

    ok.

