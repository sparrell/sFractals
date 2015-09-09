-module(cplx_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([all/0,suite/0,init_per_suite/1]).

%% Test cases
-export([make1/1, add1/1,mult1/1]).

all() ->
          [make1,add1,mult1].

suite() -> 
    [{timetrap,{minutes,1}}].

init_per_suite(Config) -> 
    [ {a,{complex,1.5e8,3}}, { b,{complex,150000001.0,3} } , { c,{complex,0.0014,157.2} } | Config ].

make1(_) ->
          {complex,1.5e8,3} = cplx:make(15.0e7,3),
          {complex,0,-3} = cplx:make(0,-3),
          {complex,-5.7,0} = cplx:make(-5.7,0),
          A = {complex,2.4e3,8.6},
          8.6 = cplx:imag(A),
          2400.0 = cplx:real(A),
          ok.

add1(_) ->
          {complex,150000001.0,3} = cplx:add({complex,1.5e8,3},1),
          ok.

mult1(_) ->
    A = {complex,1.5e8,3},
    B = {complex,0.0014,157.2},
    {complex,209528.4,23580000000.0042} = cplx:multiply(A,B),
    ok.
    

