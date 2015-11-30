#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/test/lib/sFractals/ebin ../_build/test/lib/png/ebin
%% assumes running out of _build
%% output image ../examples/example6.png
%% output data ../examples/example6.erltxt

%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-author("Duncan Sparrell").
-mode(compile).

main(_) ->

    % example 06b1

    %% base config
    {ok, [ ConfigMap | _T ] } = file:consult("../examples/Config/example06b1.ecfg"),

    % create the data and put in file
    ok = simpleFractal:computeFractalDataIntoFile( ConfigMap ),

    %% make image from data
    simpleFractal:makePngFromDataFile(ConfigMap),

    ok.

