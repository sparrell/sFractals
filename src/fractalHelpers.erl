%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-module(fractalHelpers).
-author("Duncan Sparrell").
%% routines for making fractals

%% public API
-export([ 
          computeRowOfFractalData/4,      % create one row of fractal data
          computeAllRowsOfFractalData/1      % create one row of fractal data
          ]).

%% expose functions for test
-export([ computeXList/1,computeYList/1,
          newImaginaryC/1,newRealC/1,newImaginaryZ/1,newRealZ/1,
          computeIterationValue/8 
          ]).
 
%%%%%%%%
%% computeAllRowsOfFractalData/1 API
%%        ConfigMap    - config info
%%               add parameters here to explain api
%%        returns Rows (list of rows where each row is list of counts, 1 per pixel)
%%               add output format here
%%%%%%%%
computeAllRowsOfFractalData(ConfigMap) ->
    FractalAlg = maps:get(fractalAlg,ConfigMap),
    XList = computeXList(ConfigMap),
    YList = computeYList(ConfigMap),
    FractalData = [ {{PixelY,ImgY}, computeRowOfFractalData(FractalAlg, {PixelY,ImgY},XList,ConfigMap)} || 
                        {PixelY,ImgY} <- YList ],
    FractalData.

%%%%%%%%%%%%%%%%%%%%%%%%%
%% computeRowOfFractalData computes one row of fractal data
%%        inputs:
%%               FractalAlg - which algorithm eg julian, mandelbrot, ...
%%               {PixelY,ImgY} - the Y cordinate in pixels (an integer), and number (imaginary value of Z or C)
%%               XList - the list of x coords to be computer over
%%               ConfigMap - configuration data, values needed for:
%%                           fractalAlg (must be same as FractalAlg)
%%%%%%%%%%%%%%%%%%%%%%%%%

computeRowOfFractalData(FractalAlg, {PixelY,ImgY},XList,ConfigMap) ->
    %% given Y value for the row, and given Xlist (the x values in the row), compute the fractal values
    computeRowOfFractalData(FractalAlg, {PixelY,ImgY},XList,[], ConfigMap).

computeRowOfFractalData(_FractalAlg, {_PixelY,_ImgY}, XList, RowOfFractalData, _ConfigMap) 
        when XList == [] ->
    %% XList empty so done, return RowOfFractalData sorted by pixel value (ie 1 first)
    lists:sort(RowOfFractalData);

computeRowOfFractalData(FractalAlg, {PixelY,ImgY}, XList, RowOfFractalData, ConfigMap) 
            when FractalAlg == julian ->
    %% otherwise pop off one x value, compute data, insert answer in RowOfFractalData, and recurse
    [ {PixelX, RealX} | NewXList ] = XList,   % pop off first x value, remainder is used for next iteration
    %% compute fractal value for x,y
    IterCount = computeIterationValue(FractalAlg,
                                      maps:get(cReal,ConfigMap), %since julian, C remains constant
                                      maps:get(cImaginary,ConfigMap), %since julian, C remains constant
                                      RealX,                          %ZReal since julian
                                      ImgY,                           %ZImg since julian
                                      0,                              %IterCount = 0 to start
                                      maps:get(maxIterationThreshold,ConfigMap),
                                      maps:get(bailoutThreshold,ConfigMap)
                                      ),
    %% push new value onto RowOfFractalData list
    NewRowOfFractalData = [{PixelX, RealX, IterCount} | RowOfFractalData ],

    %% recurse
    computeRowOfFractalData(FractalAlg, {PixelY,ImgY}, NewXList, NewRowOfFractalData, ConfigMap).
    

%%%%%%%%%%%%%%%%%%%%%%%%%
%% ComputeXlist and ComputeYList set up x/y pixel/number data
%%%%%%%%%%%%%%%%%%%%%%%%%


%% ComputeXlist creates a row (list) of X,ZsubR,CsubR for a given fractal alg
%%    This list, along with the Y list, create the 'box' where each dot (x,y) fractal value is computed
computeXList(ConfigMap) ->
     %% get parameters
    Width      = maps:get(width,ConfigMap),
    %% each pixel has a corresonding complex number defined by corners of the box
    %%   note whether Y is dependent on Z or C is algorithm dependent
    %%     and taken into account in config file
    XRealRight      = maps:get(xRealRight,ConfigMap),
    XRealLeft       = maps:get(xRealLeft,ConfigMap),
    %% box is bounded on left by x > XRealLeft and bounded on right by x < XRealRight
    %% box is width pixels wide and height pixels high

    %% step is floating range divided by number of pixels
    DeltaX = (XRealRight - XRealLeft) / (Width-1), % note -1 since one less intervals than points

    %% iteratively compute the values in the list
    %%    starting at right (x=width, xreal=XRealRight) and decrementing down
    %% the returned list is returned by this function for use in fractal computations
    computeXList([], Width, XRealRight, DeltaX).

computeXList(Row, PixelX, _RealX, _DeltaX)
         when PixelX =< 0 ->
    %% since PixelX decremented to end, the Row is now complete, so return Row
    Row;

computeXList(Row, PixelX, RealX, DeltaX) ->
    %% otherwise add another point and recurse
    NewRow = [ {PixelX,RealX} | Row ],
    computeXList(NewRow, PixelX-1, RealX-DeltaX, DeltaX).

%% ComputeYlist creates a column (list) of Y,ZsubI,CsubI for a given fracal alg
%%    This list, along with the X list, create the 'box' where each dot (x,y) fractal value is computed
computeYList(ConfigMap) ->
     %% get parameters
    Height          = maps:get(height,ConfigMap),
    %% each pixel has a corresonding complex number defined by corners of the box
    %%   note whether Y is dependent on Z or C is algorithm dependent
    %%     and taken into account in config file
    YImaginaryLow   = maps:get(yImaginaryLow,ConfigMap),
    YImaginaryHigh  = maps:get(yImaginaryHigh,ConfigMap),
    %% box is bounded on top by y > YImaginaryHigh and bounded on bottom by y > YImaginaryLow
    %% box is width pixels wide and height pixels high
    DeltaY = (YImaginaryHigh - YImaginaryLow) / (Height-1), % note -1 since one less intervals than points

    %% iteratively compute the values in the list
    %%    starting at top (y=height, yImg=YImgTop) and decrementing down
    %% the returned list is returned by this function for use in fractal computations
    computeYList([], Height, YImaginaryHigh, DeltaY).

computeYList(Column, PixelY, _ImgY, _DeltaY)
         when PixelY =< 0 ->
    %% since PixelY decremented to end, the Column is now complete, so return Column
    Column;

computeYList(Column, PixelY, ImgY, DeltaY) ->
    %% otherwise add another point and recurse
    NewColumn = [ {PixelY,ImgY} | Column ],
    computeYList(NewColumn, PixelY-1, ImgY-DeltaY, DeltaY).


%% computeIterationValue computes fractal value and returns iteration count
%% function clause for exceeding iteration count
computeIterationValue(_FractalAlg,
                      _CReal, 
                      _CImaginary, 
                      _ZReal,
                      _ZImaginary, 
                      IterCount,                     %for this clause only need IterCount and Max
                      MaxIterationThreshold, 
                      _BailoutThreshold )
        when IterCount >= MaxIterationThreshold ->   % reached iteration limit so return count=limit
    MaxIterationThreshold;

%% function clause for exceeding bound
computeIterationValue(_FractalALg,
                      _CReal,
                      _CImaginary, 
                      ZReal,
                      ZImaginary, 
                      IterCount, 
                      _MaxIterationThreshold, 
                      BailoutThreshold ) 
        when ((ZReal*ZReal)+(ZImaginary*ZImaginary)) > BailoutThreshold -> 
    %bailout exceeded so return iterCount
    IterCount;

%% function clause for recursing further
computeIterationValue(FractalAlg,
                      CReal,
                      CImaginary, 
                      ZReal,
                      ZImaginary, 
                      IterCount, 
                      MaxIterationThreshold, 
                      BailoutThreshold ) ->

    % compute new Z and C based on fractal algorithm used
    ZCParams      = {FractalAlg,CReal,CImaginary, ZReal,ZImaginary},
    NewZReal      = newRealZ(ZCParams),
    NewZImaginary = newImaginaryZ(ZCParams),
    NewCReal      = newRealC(ZCParams),
    NewCImaginary = newImaginaryC(ZCParams),

    computeIterationValue(FractalAlg,
                          NewCReal,
                          NewCImaginary, 
                          NewZReal, 
                          NewZImaginary, 
                          IterCount+1, 
                          MaxIterationThreshold, 
                          BailoutThreshold ).

% function for creating new real value for Julian algorithm
newRealZ({FractalAlg,CReal,_CImaginary, ZReal,ZImaginary}) 
        when FractalAlg == julian ->
    (ZReal*ZReal) - (ZImaginary*ZImaginary) + CReal.

% function for creating new imaginary value for Julian algorithm
newImaginaryZ({FractalAlg,_CReal,CImaginary, ZReal,ZImaginary}) 
        when FractalAlg == julian ->
    (2 * ZReal * ZImaginary) + CImaginary.

% function for creating new real value for Julian Algorithm (ie remains unchanged)
newRealC({FractalAlg,CReal,_CImaginary, _ZReal,_ZImaginary}) 
        when FractalAlg == julian ->
    CReal.

% function for creating new imaginary value for Julian Algorithm (ie remains unchanged)
newImaginaryC({FractalAlg,_CReal,CImaginary, _ZReal,_ZImaginary}) 
        when FractalAlg == julian ->
    CImaginary.
