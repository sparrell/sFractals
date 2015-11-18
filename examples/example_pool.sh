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

-define(CLD(Name, Module, Options),
  { Name
  , {sumo_store, start_link, [Name, Module, Options]}
  , permanent
  , 5000
  , worker
  , [Module]
  }
).

main(_) ->

    Stores = [
        {mysql, sumo_store_mysql, [
          {storage_backend, mysql_backend},
          {workers, 5}
        ]},
        {mongo, sumo_store_mongo, [
          {storage_backend, mongo_backend},
          {workers, 5}
        ]}
      ],

    Children = lists:map(
         fun({Name, Module, Options}) -> ?CLD(Name, Module, Options) end,
         Stores
         ),
    
    io:format("Children: ~p~n",[Children]),

    shell:start(),
    timer:sleep(infinity),

    ok.
