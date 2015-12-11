#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/test/lib/sFractals/ebin ../_build/test/lib/png/ebin
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

    % example 1a

    %% base config
    ConfigFile = "../examples/Config/example01a.ecfg",
    {ok, [ ConfigMap | _T ] } = file:consult(ConfigFile),

    % create the data
    Rows = compute_fractal_data:compute_fractal_data( ConfigMap ),

    %% make image from data
    compute_fractal_data:make_png_from_data(Rows,ConfigMap),

    ok.

