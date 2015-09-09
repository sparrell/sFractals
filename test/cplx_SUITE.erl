-module(cplx_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([all/0]).

%% Test cases
-export(make1/0, add1/0]).

all() ->
          [make1,add1].

make1() ->
          A = cplx:make(15.0e7,3),
          A = {complex,1.5e8,3}.

add1() ->
          B = cplx:add(15.0e7,3),
          B = {complex,1.5e8,3}.

