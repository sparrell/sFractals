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
          computeFractalData/1,           % create a block of fractal data
          computeRowOfFractalData/4      % create one row of fractal data
          ]).

%% expose functions for test
-export([ computeXList/1,computeYList/1,
          newImaginaryC/1,newRealC/1,newImaginaryZ/1,newRealZ/1,
          computeIterationValue/8, 
          computeFractalData/11 ]).
 
%%%%%%%%
%% computeFractalData/1 API
%%        ConfigMap    - config info
%%        returns Rows (list of rows where each row is list of counts, 1 per pixel)
%%%%%%%%
computeFractalData(ConfigMap) ->
    %% get config needed
    %% create height rows of width columns of pixels
    Width           = maps:get(width,ConfigMap),
    Height          = maps:get(height,ConfigMap),
    %% each pixel has a corresonding complex number defined by corners of the box
    XRealRight      = maps:get(xRealRight,ConfigMap),
    XRealLeft       = maps:get(xRealLeft,ConfigMap),
    YImaginaryLow   = maps:get(yImaginaryLow,ConfigMap),
    YImaginaryHigh  = maps:get(yImaginaryHigh,ConfigMap),
    %% box is bounded on left by x > XRealLeft and bounded on right by x < XRealRight
    %% box is bounded on top by y > YImaginaryHigh and bounded on bottom by y > YImaginaryLow
    %% box is width pixels wide and height pixels high

    %% step is floating range divided by number of pixels
    DeltaX = (XRealRight - XRealLeft) / Width,
    DeltaY = (YImaginaryHigh - YImaginaryLow) / Height,

    %% call computeFractalData/11
    % return Rows from computeFractalData/11
    computeFractalData( [],             % Rows starts empty
                        [],             % ThisRow starts empty
                        Width,          % XPix starts width and decrements to 1
                        XRealRight,     %% XR starts at right and decrements to by deltaX
                        DeltaX,         
                        Width,          % width
                        Height,         % YPix starts height and decrements to 1
                        YImaginaryHigh, % YI starts at top and decrements by delta y
                        DeltaY,         
                        Height,         % height
                        ConfigMap).    % map of parameters



%%%%%%%%
%% computeFractalData/11 API
%%        Rows         - a list of the rows(lines) of data, starts empty and builds until height reached, built top down
%%        ThisRow      - a list of the points(count value) in a row/line, starts empty and builds R->L until width reached
%%        XPix         - the integer X value of the pixel 
%%        XR           - the real component of the floating point number for computing fractal for this XPix
%%        DeltaX       - for each pixel, XR increases by this amount
%%        Width        - width of fractal in pixels
%%        YPix         - the integer Y value of the pixel
%%        YI           - the imaginary component of the floating point number for computing fractal for this YPix
%%        DeltaY       - for each pixel, YI increases by this amount
%%        Height       - height(intger) of image = number of rows
%%        ConfigMap    - config info
%%%%%%%%
% clause when height is reached, return the rows of data
computeFractalData( Rows, _ThisRow,
               _XPix, _XR, _DeltaX, _Width,  
               YPix, _YI, _DeltaY, _Height,  % only height matters
               _ConfigMap)
        when YPix =< 0 ->
               
    %% pixels all made already so done
    Rows;

% clause when row is complete  but height not reached - process row and recurse
computeFractalData( Rows, ThisRow,        % row data computed so far
               XPix, _XR, DeltaX, Width,  % info for points in a row
               YPix, YI, DeltaY, Height,  % info for rows
               ConfigMap)
        when XPix =< 0, YPix >= 0 ->

    % add row
    NewRows = [ ThisRow | Rows ],

    % reset to begining of next row 
    NewRowData = [],                                 % reset data for row to empty
    NewXPix    = Width,                              % reset to end of line
    NewXR      = maps:get(xRealRight,ConfigMap),     % reset to end of line
    NewYPix    = YPix - 1,                           % increment row
    NewYI      = YI - DeltaY,                          % increment row
    computeFractalData( NewRows, NewRowData,
                   NewXPix, NewXR, DeltaX, Width,
                   NewYPix, NewYI, DeltaY,Height,
                   ConfigMap);

computeFractalData( Rows, RowData,       % row data computed so far
               XPix, XR, DeltaX, Width,   % info for points in a row
               YPix, YI, DeltaY, Height,  % info for rows
               ConfigMap)
        when XPix > 0, YPix > 0 ->

    %% get iteration count for this point
    NewPoint = computeIterationValue( maps:get(fractalAlg,ConfigMap),
                                      maps:get(cReal,ConfigMap),
                                      maps:get(cImaginary,ConfigMap),
                                      XR,
                                      YI,
                                      0,          %iteration count starts at zero
                                      maps:get(maxIterationThreshold,ConfigMap),
                                      maps:get(bailoutThreshold,ConfigMap)
                                      ),
                           
    NewRowData = [ NewPoint | RowData ],
    NewXPix    = XPix - 1,                           % decrement moving left building row
    NewXR      = XR - DeltaX,                        % decrease XR to the left
    computeFractalData( Rows, NewRowData,
                   NewXPix, NewXR, DeltaX, Width,
                   YPix, YI, DeltaY,Height,
                   ConfigMap).

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

computeRowOfFractalData(_FractalAlg, {_PixelY,_ImgY}, XList, FractalData, _ConfigMap) 
        when XList == [] ->
    %% XList empty so done, return FractalData
    FractalData;

computeRowOfFractalData(FractalAlg, {PixelY,ImgY}, XList, FractalData, ConfigMap) 
            when FractalAlg == julian ->
    %% otherwise pop off one x value, compute data, insert answer in FractalData, and recurse
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
    %% push new value onto FractalData list
    NewFractalData = [{PixelX, RealX, IterCount} | FractalData ],

    %% recurse
    computeRowOfFractalData(FractalAlg, {PixelY,ImgY}, NewXList, NewFractalData, ConfigMap).
    

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

