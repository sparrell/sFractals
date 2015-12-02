#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/test/lib/sFractals/ebin ../_build/test/lib/png/ebin ../_build/test/lib/worker_pool/ebin
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

    %% example 08d 
    %% note 2x deep, 2x wide, 2x high

    %% base config
    {ok, [ ConfigMap | _T ] } = file:consult("../examples/Config/example08d.ecfg"),

    %% create the data and png
    fractalHelpers:makePngUsingPool(ConfigMap),

    ok.

