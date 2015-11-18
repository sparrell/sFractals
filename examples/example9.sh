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

    %% example 9

    %% base config
   {ok, [ ConfigMap | _T ] } = file:consult("../examples/example9.ecfg"),

    % create the data
    %~io:format("computing data~n"),
    %ok = simpleFractal:computeFractalDataIntoFile2( ConfigMap ),
    Debug = simpleFractal:computeFractalDataIntoFile2( ConfigMap ),
    io:format("Debug: ~p~n", [Debug]),

    %% make image from data
    %~io:format("rendering image~n"),
    %simpleFractal:makePngFromDataFile(ConfigMap),


    shell:start(),
    timer:sleep(infinity),
    ok.

