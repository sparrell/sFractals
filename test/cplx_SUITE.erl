-module(cplx_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([all/0,suite/0,init_per_suite/1]).

%% Test cases
-export([make1/1, add1/1, mult1/1, sqrt1/1, root1/1, polar1/1, cos1/1, 
         iscomplex1/1, subtract1/1, divide1/1, absolute1/1]).

all() ->
    [make1, add1, mult1, sqrt1, root1, polar1, cos1, iscomplex1, subtract1, divide1, absolute1].

suite() -> 
    [{timetrap,{minutes,1}}].

init_per_suite(Config) -> 
    [ {a,{complex,1.5e8,3}}, { b,{complex,150000001.0,3} } , { c,{complex,0.0014,157.2} } | Config ].

iscomplex1(_) ->
    true = cplx:is_complex( {complex,1.5e8,3} ),
    ok.
make1(_) ->
          {complex,1.5e8,3} = cplx:make(15.0e7,3),
          {complex,0,-3} = cplx:make(0,-3),
          {complex,-5.7,0} = cplx:make(-5.7,0),
          {complex,-6.2,0} = cplx:make(-6.2),
          {complex,6,0} = cplx:make(6),
          A = {complex,2.4e3,8.6},
          8.6 = cplx:imag(A),
          2400.0 = cplx:real(A),
          17 = cplx:real(17),
          17.1 = cplx:real(17.1),
          ok.

add1(_) ->
          {complex,150000001.0,3} = cplx:add({complex,1.5e8,3},1),
          {complex,210.0,21.0} = cplx:add({complex,200,20.0},{complex,10.0,1}),
          ok.

subtract1(_) ->
    {complex, 5, 3} = cplx:subtract({complex, 9, 5},{complex, 4, 2}),
    {complex, 6, 5} = cplx:subtract({complex, 9, 5}, 3),
    ok.

mult1(_) ->
    A = {complex,1.5e8,3},
    B = {complex,0.0014,157.2},
    {complex,209528.4,23580000000.0042} = cplx:multiply(A,B),
    {complex, 0.0028, 314.4} = cplx:multiply(B,2),
    ok.

divide1(_) ->
    CmplxAns = cplx:divide( {complex, -7, 22}, {complex, 2, 3} ),
    %% need to round to avoid precision issues
    RealPart = round(cplx:real(CmplxAns) * 1000 ) / 1000,
    ImgPart  = round(cplx:imag(CmplxAns) * 1000 ) / 1000,
    4.0 = RealPart,
    5.0 = ImgPart,

    %% now test when divisor has only real part
    {complex, -3.5, 11.0} = cplx:divide( {complex, -7, 22}, 2 ),

    %% test when numerator small and denomintor larger
    {complex, 0.1, -0.075} = cplx:divide( {complex, 0.5, 0.25}, {complex, 2, 4} ),

    %% test when z2 real greater than z2 img
    {complex, 0.125, 0.0} = cplx:divide( {complex, 0.5, 0.25}, {complex, 4, 2} ),
    ok.

absolute1(_) ->
    0.25 = cplx:absolute( {complex, 0, 0.25} ),
    0.5  = cplx:absolute( {complex, 0.5, 0} ),
    5.0  = cplx:absolute( {complex, 3, 4} ),
    5.0  = cplx:absolute( {complex, 4, 3} ),
    ok.

sqrt1(_) ->
    A = {complex,0.0014,157.2},
    {complex,8.865703581956542,8.865624625660454} = cplx:sqrt(A),
    ok.

root1(_) ->
    A = {complex,0.0014,157.2},
    B = {complex,27,0},
    C = {complex,-64,0},
    {complex,-8.865703581956543,-8.865624625660448} = cplx:root(A,2),
    {complex,-4.673914188006709,2.6985041147310245} = cplx:root(A,3),
    {complex,3.0,0} = cplx:root(B,3),
    %% need to account for precision on the next one (round off error from 4)
    CmplxAns = cplx:root(C,3),
    0 = cplx:real(CmplxAns),
    4.0 = round(cplx:imag(CmplxAns) * 1000 ) / 1000,
    ok.

polar1(_) ->
    A = {complex,0.0014,157.2},
    {complex,157.20000000623406,1.5707874209424795} = cplx:polar(A),
    ok.

cos1(_) ->
    A = {complex,0.0014,157.2},
    {complex,9.333878297670655e67,-1.306743815413296e65} = cplx:cos(A),
    ok.

