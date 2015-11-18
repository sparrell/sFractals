#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/test/lib/sFractals/ebin ../_build/test/lib/png/ebin ../_build/test/lib/worker_pool/ebin -config example10.config
%% assumes running out of examples
%% output ../examples

%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-author("Duncan Sparrell").
-copyright("sFractal Consulting LLC").
-github("https://github.com/sparrell/sFractals").
-license("Apache License 2.0").

-mode(compile).

main(_) ->

    %% example 10

    %% base config
   {ok, [ ConfigMap | _T ] } = file:consult("../examples/example10.ecfg"),

    io:format("starting example~n"),
   
    %% setup stuff
    PathGotten = code:get_path(),
    io:format("Path=~p ~n",[PathGotten]),
    EnvGotten = application:get_all_env(),
    io:format("Env=~p ~n",[EnvGotten]),

    %% start apps
    start_apps(),

    io:format("made it past starting apps ~n"),
    EnvAppAll = application:get_all_env(),
    io:format("Env=~p ~n",[EnvAppAll]),
    EnvApp = application:get_all_env(worker_pool),
    io:format("Env=~p ~n",[EnvApp]),
    ThisApp = application:get_application(),
    io:format("ThisApp=~p ~n",[ThisApp]),
    AllApps = application:loaded_applications(),
    io:format("AllApps=~p ~n",[AllApps]),
    StartedApps = application:which_applications(),
    io:format("StartedApps=~p ~n",[StartedApps]),

    ModL = code:all_loaded(),
	io:format("modules: ~p~n~n",[ModL]),


    timer:sleep(3000),

    wpool:start_pool(
    my_pool,
    [{workers, 200}, {worker, {simpleFractal,dataFileSvr,[]} }]),

    io:format("actually do some stuff here ~n"),

    timer:sleep(3000),

    ok.

start_apps() ->
  Apps = [
    worker_pool
  ],
  [ ok = application:start(X) || X <- Apps].
