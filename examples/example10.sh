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
   %%{ok, [ ConfigMap | _T ] } = file:consult("../examples/example10.ecfg"),
    {ok, [ ConfigMap | _T ] } = file:consult("../examples/example1.ecfg"),

    io:format("starting example~n"),
   
    %% setup stuff
    PathGotten = code:get_path(),
    %%io:format("Path=~p ~n",[PathGotten]),
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
	%%io:format("modules: ~p~n~n",[ModL]),

    %%timer:sleep(3000),

    {ok, PoolPid} = wpool:start_sup_pool(
         my_pool,
         %% use default worker
         [{workers, 200}]),         %[{workers, 200}, {worker, {SpecFun,[]} }]),
	io:format("PoolPid: ~p~n~n",[PoolPid]),


    MyMessage = {erlang,'+',[0,0] },
    Answer = wpool:call(my_pool, MyMessage),
	io:format("Answer: ~p~n~n",[Answer]),
    MyMessage2 = {simpleFractal,computeFractalData,[ConfigMap] },
    Answer2 = wpool:call(my_pool, MyMessage2),
	io:format("Answer2: ~p~n~n",[Answer2]),

    io:format("actually do some stuff here ~n"),

    Stats = wpool:stats(my_pool),
	%%io:format("stats: ~p~n~n",[Stats]),

    %%timer:sleep(3000),

    ok.

start_apps() ->
  Apps = [
    worker_pool
  ],
  [ ok = application:start(X) || X <- Apps].

