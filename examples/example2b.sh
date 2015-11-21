#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/test/lib/sFractals/ebin ../_build/test/lib/png/ebin
%% assumes running out of _build
%% output ../examples/

%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-author("Duncan Sparrell").
-mode(compile).

main(_) ->

    % example 2b

    %% base config
    {ok, [ ConfigMap | _T ] } = file:consult("../examples/example2b.ecfg"),

    %% create the data and png
    fractalHelpers:makePng(ConfigMap),

    ok.

