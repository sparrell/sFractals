#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/sFractals/ebin ../_build/default/lib/epocxy/ebin
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

    % example 11

    %% base config
    ConfigFile = "../examples/Config/example11.ecfg",
    {ok, [ ConfigMap | _T ] } = file:consult(ConfigFile),

    % create the data
    controller:make_data( ConfigMap ),

    %% make image from data
    %%compute_fractal_data:make_png_from_data(Rows,ConfigMap),

    ok.

