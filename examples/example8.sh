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

    % example 8

    %% base config
    {ok, [ ConfigMap | _T ] } = file:consult("../examples/example8.ecfg"),

    dataFileSvr:start(ConfigMap),

    dataFileSvr:addARow( {5, [5,3,4]} ),
    dataFileSvr:addARow( {2, [2,3,4]} ),
    dataFileSvr:addARow( {6, [6,3,4]} ),

    dataFileSvr:rowStatus(),

    dataFileSvr:stop(),

    ok.

