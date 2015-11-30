#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/test/lib/sFractals/ebin ../_build/test/lib/png/ebin ../_build/test/lib/worker_pool/ebin
%% assumes running out of _build
%% output image ../examples directory

%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-author("Duncan Sparrell").
-mode(compile).

main(_) ->

    % example 05d

    %% base config
    {ok, [ ConfigMap | _T ] } = file:consult("../examples/Config/example05d.ecfg"),

    %% create the data and png
    fractalHelpers:makePngUsingPool(ConfigMap),

    ok.
