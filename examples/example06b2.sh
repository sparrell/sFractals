#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/test/lib/sFractals/ebin ../_build/test/lib/png/ebin
%% assumes running out of _build
%% output image ../examples/example6b.png
%% input data ../examples/example6.erl.txt

%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-author("Duncan Sparrell").
-mode(compile).

main(_) ->

    % example 6b

    %% base config
    {ok, [ ConfigMap | _T ] } = file:consult("../examples/example6b.ecfg"),

    %% make image from data stored previously
    simpleFractal:makePngFromDataFile(ConfigMap),

    ok.

