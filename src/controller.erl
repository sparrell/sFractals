%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%%
%%%-------------------------------------------------------------------

-module(controller).
-author("Duncan Sparrell").
%% control the flow of making a fractal
%% use epocxy to spawn actual computations

%% public API
-export([ make_data/1 % create a block of fractal data
        , data2file/2  % write fractal data to file
        , data2svr/2  % write fractal data to svr
        , point/5     % worker
          ]).

%% expose functions for test
%% none?

make_data( ConfigMap ) ->

  %% compute the box (ie list of x's and y's)
  XList = databox:compute_xlist(ConfigMap),
  YList = databox:compute_ylist(ConfigMap),

  %% create ETS table to hold data
  FractalEts = ets:new(fractal_ets, [set, public]),

  %% spawn processes to populate ETS with fractal data
  %%    initialize concurrency using epocxy concurrency
  %%    cfp = concurrent fractal processing
  true = cxy_ctl:init([{cfp, unlimited, 1000, 100000}]),
  %% spawn the workers for each point
  make_rows( cfp, XList, YList, ConfigMap, FractalEts ),

  %% wait for all rows of data to finish
  wait_for_rows( XList, YList),

  %% transform data

  %% return data
  ok.

wait_for_rows( _XList, []) ->
  %% if YList empty then you are done
  ok;
wait_for_rows( XList, [Y | RestY] ) ->
  %% wait for next row
  wait_for_row( XList, Y ),
  %% recurse thru rest of rows
  wait_for_rows( XList, RestY ).

wait_for_row( [], _Y) ->
  %% if XList empty then you are done with this row
  ok;
wait_for_row( [ X | RestX ], Y ) ->
  %% wait for next point in row
  receive
    %% match when get a message that a point is done
    %% note it might not be this XY
    %%   doesn't matter - just counting got right number back
    did_a_point ->
      ok
  after 5000 ->  %hardcoded 5s timeout for next point
      lager:error("wait_for_row timeout. XY = ~p, ~p",[X,Y]),
      erlang:error("wait_for_row timeout")
  end,

  %% recurse thru rest of points in row
  wait_for_row( RestX, Y).


make_rows( _Pool, _XList, [], _ConfigMap, _FractalEts ) ->
  %% done when YList empty
  ok;

make_rows( Pool, XList, [FirstY | RestY], ConfigMap, FractalEts ) ->
  %% make another row of data
  make_a_row( Pool, XList, FirstY, ConfigMap, FractalEts),
  %% recurse thru rest of rows
  make_rows( Pool, XList, RestY, ConfigMap, FractalEts ).

make_a_row( _Pool, [], _Y, _ConfigMap, _FractalEts) ->
  %% done when XList empty
  ok;
make_a_row( Pool, [X | RestX], Y, ConfigMap, FractalEts) ->
  %% make a point
  cxy_ctl:execute_task(cfp, ?MODULE, point, [ X, Y, ConfigMap, FractalEts, self()]),
  %% recurse thru rest of row
  make_a_row( Pool, RestX, Y, ConfigMap, FractalEts).

data2file( _Data, _ConfigMap) ->
  lager:debug("need to implement data2file using binary"),
  ok.

data2svr( _Data, _ConfigMap) ->
  lager:debug("need to implement data2svr"),
  ok.

%%%%%%%
%% point is the worker to compute one data point
%%     inputs: {PixelX, RealX}, {PixelY, ImgY}, ConfigMap
%%              PixelX = integer x pixel co-ordinate
%%              RealX  = real floating point number corresponding to X
%%              PixelY = integer y pixel co-ordinate
%%              ImgY   = imaginary floating point number corresponding to Y
%%              ConfigMap = configuration data
point( {PixelX, RealX}
     , {PixelY, ImgY}
     , #{ fractalAlg := julian
        , cReal := CReal
        , cImaginary := CImaginary
        , maxIterationThreshold := MaxIterationThreshold
        , bailoutThreshold := BailoutThreshold
        } = ConfigMap 
     , FractalEts
     , ControllerPid
     ) 
    when is_integer(PixelX)
       , is_integer(PixelY)
       , is_float(RealX)
       , is_float(ImgY)
       , is_float(CReal)
       , is_float(CImaginary)
       , is_integer(MaxIterationThreshold)
       , is_float(BailoutThreshold)
    ->
  %% calculate iterations
  Iter = compute_points:compute_iteration_value( julian
                                               , CReal
                                               , CImaginary
                                               , RealX
                                               , ImgY
                                               , 0
                                               , MaxIterationThreshold
                                               , BailoutThreshold ),

  %% store in ets table at x,y
  ets:insert(FractalEts, {{PixelX ,PixelY}, Iter}),

  %% message that data stored
  ControllerPid ! did_a_point,

  %% done
  ok.
