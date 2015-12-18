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
          ]).

%% expose functions for test
-export([ compute_xlist/1, compute_ylist/1 ]).

make_data( ConfigMap ) ->

  %% compute the box (ie list of x's and y's)
  XList = compute_xlist(ConfigMap),
  YList = compute_ylist(ConfigMap),

  %% create ETS table to hold data

  %% spawn processes to populate ETS with fractal data

  %% wait for all data to finish

  %% transform data

  %% return data
  ok.

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

