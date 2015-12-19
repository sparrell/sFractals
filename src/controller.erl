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
-export([ compute_xlist/1, compute_ylist/1 ]).

make_data( ConfigMap ) ->

  %% compute the box (ie list of x's and y's)
  XList = compute_xlist(ConfigMap),
  YList = compute_ylist(ConfigMap),

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

%%%%%%%%%%%%%%%%%%%%%%%%%
% compute_xlist and compute_ylist set up x/y pixel/number data
%%%%%%%%%%%%%%%%%%%%%%%%%


%% compute_xlist creates a row (list) of X, ZsubR, CsubR for a given fractal alg
%%    This list, along with the Y list, create the 'box'
%%        where each dot (x, y) fractal value is computed
compute_xlist( #{ width := Width
                , xRealRight := XRealRight
                , xRealLeft := XRealLeft
                }
             ) when is_integer(Width)
                  , is_float(XRealRight)
                  , is_float(XRealLeft)
             ->
    %% each pixel has a corresonding complex number defined
    %%         by corners of the box
    %%   note whether Y is dependent on Z or C is algorithm dependent
    %%     and taken into account in config file
    %% box is bounded on left by x > XRealLeft
    %%    and bounded on right by x < XRealRight
    %% box is width pixels wide and height pixels high

    %% step is floating range divided by number of pixels
    %% note -1 in next statement since one less intervals than points
    DeltaX = (XRealRight - XRealLeft) / (Width-1),

    %% iteratively compute the values in the list
    %%    starting at right (x=width, xreal=XRealRight) and decrementing down
    %% the returned list is returned by this function
    %%    for use in fractal computations
    compute_xlist([], Width, XRealRight, DeltaX).

compute_xlist(Row, PixelX, _RealX, _DeltaX)
         when is_integer(PixelX), PixelX =< 0 ->
    %% since PixelX decremented to end, the Row is now complete, so return Row
    Row;

compute_xlist(Row, PixelX, RealX, DeltaX)
        when is_integer(PixelX)
           , is_float(RealX)
           , is_float(DeltaX)
        ->
    %% otherwise add another point and recurse
    NewRow = [ {PixelX, RealX} | Row ],
    compute_xlist(NewRow, PixelX-1, RealX-DeltaX, DeltaX).

%% compute_ylist creates a column (list) of Y, ZsubI, CsubI for a
%%    given fracal alg
%%  This list, along with the X list, create the 'box' where
%%    each dot (x, y) fractal value is computed
compute_ylist( #{ height := Height
                , yImaginaryLow := YImaginaryLow
                , yImaginaryHigh := YImaginaryHigh
                }
             ) when is_integer(Height)
                  , is_float(YImaginaryLow)
                  , is_float(YImaginaryHigh)
             ->
    %% each pixel has a corresonding complex number defined by corners
    %%    of the box
    %%  note whether Y is dependent on Z or C is algorithm dependent
    %%     and taken into account in config file
    %% box is bounded on top by y > YImaginaryHigh
    %%    and bounded on bottom by y > YImaginaryLow
    %% box is width pixels wide and height pixels high
    %% note -1 since one less intervals than points
    DeltaY = (YImaginaryHigh - YImaginaryLow) / (Height-1),

    %% iteratively compute the values in the list
    %%    starting at top (y=height, yImg=YImgTop) and decrementing down
    %% the returned list is returned by this function
    %%    for use in fractal computations
    compute_ylist([], Height, YImaginaryHigh, DeltaY).

compute_ylist(Column, PixelY, _ImgY, _DeltaY)
         when is_integer(PixelY), PixelY =< 0 ->
    %% since PixelY decremented to end, the Column is now complete,
    %%    so return Column
    Column;

compute_ylist(Column, PixelY, ImgY, DeltaY)
        when is_integer(PixelY)
           , is_float(ImgY)
           , is_float(DeltaY)
        ->
    %% otherwise add another point and recurse
    NewColumn = [ {PixelY, ImgY} | Column ],
    compute_ylist(NewColumn, PixelY-1, ImgY-DeltaY, DeltaY).

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
