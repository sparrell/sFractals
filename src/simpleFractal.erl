%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-module(simpleFractal).
-author("Duncan Sparrell").
%% try to make simple fractal data and then turn into color data and then make image

%% public API
-export([ simplFrac/1, createPointData/3 ]).

%% expose functions for test
-export([ newImaginaryC/1,newRealC/1,newImaginaryZ/1,newRealZ/1,
          computeIterationValue/8, addOnePoint/7, makePoints/8 ]).
 

% create a simple fractal
simplFrac(ConfigMap) ->

    %% create box of data for every X,Y
    Width           = maps:get(width,ConfigMap),
    Height          = maps:get(height,ConfigMap),
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

    %% create the count data
    IterationCountData = createPointData( {XRealLeft,DeltaX,Width},       %info for points in a row
                                          {YImaginaryLow,DeltaY,Height},  %info for rows
                                          ConfigMap),

    %% print some stats on distribution of counts
    io:format("stats~n~p~n", [imagelib:analyzeData(IterationCountData) ] ),

    %% colorize the data using simplest alg
    ColorData = imagelib:colorizeData(IterationCountData,simplest,ConfigMap),

    %% take the x,y,color data and creates an image
    imagelib:makeImageFromData( ColorData, ConfigMap ),

    ok.

createPointData( {XRealLeft,DeltaX,Width}, {YImaginaryLow,DeltaY,Height}, ConfigMap)
        when DeltaX > 0, DeltaY > 0 ->

    CurrentPixelY = 1,   % start at bottom and work up (so head of list will be top row)
    IterCounts = [],     % start with empty data box
    %% makeAllRows returns InterCount, which is then returned by createPointData 
    makeAllRows( {XRealLeft,DeltaX,Width}, 
                 {YImaginaryLow,DeltaY,Height,CurrentPixelY},
                 IterCounts, 
                 ConfigMap).

%% make one row at a time, starting at bottom and working up 
%%  note builds list from tail so this yeilds highest Y nearest front 
%%  which is useful in case later we might to build file on fly that way 'from top'

% function head for done
%new
makeAllRows( {_XRealLeft,_DeltaX,_Width}, 
                 {_CurrentImaginaryY,_DeltaY,Height,CurrentPixelY},
                 IterCounts, 
                 _ConfigMap)
        when CurrentPixelY > Height ->  
    IterCounts;   %reached bottom so return the iteration count data

% function head for not-done so recurse
makeAllRows( {XRealLeft,DeltaX,Width}, 
                 {CurrentImaginaryY,DeltaY,Height,CurrentPixelY},
                 IterCounts, 
                 ConfigMap) ->

    % reached here so need to make another row
	% IterCounts is updated with the new row calculated by makePoints and is the returned value
    NewIterCounts = makePoints(Width,
                               1,                   % set CurrentPixelX to 1
                               CurrentPixelY,
                               XRealLeft, 
                               CurrentImaginaryY,
                               DeltaX,
                               IterCounts,
                               ConfigMap),
    makeAllRows( {XRealLeft,DeltaX,Width}, 
                 {CurrentImaginaryY+DeltaY,DeltaY,Height,CurrentPixelY+1},   % increment float & pixel Y's
                 NewIterCounts,                                   % new counts
                 ConfigMap).
    
% function head for done-with-row
makePoints(Width,
           CurrentPixelX,
           _CurrentPixelY,
           _CurrentRealX,
           _CurrentImaginaryY,
           _DeltaX,
           IterCounts,
           _ConfigMap)
        when CurrentPixelX > Width ->  
    IterCounts;   %reached end of line so return the iteration count data

% function head for not-done so recurse
makePoints(Width,
           CurrentPixelX,
           CurrentPixelY,
           CurrentRealX,
           CurrentImaginaryY,
           DeltaX,
           IterCounts,
           ConfigMap) ->   
    %reached here so need to add another pixel
    % CurrentPixelX starts at 1 increases by 1 each recurse
    % CurrentRealX starts at XRealLeft and increases by DeltaX each recurse
    % IterCounts has point {X,Y,Iter} added by function addOnePoint
    FractalAlg= maps:get(fractalAlg,ConfigMap),

    %% note addOnePoint returns the new value of IterCounts into the recursive call
    NewIterCounts = addOnePoint(CurrentPixelX,
                                CurrentPixelY,
                                CurrentRealX,
                                CurrentImaginaryY,
                                FractalAlg,
                                IterCounts,
                                ConfigMap),

    makePoints(Width,
               CurrentPixelX+1,
               CurrentPixelY,
               CurrentRealX+DeltaX,
               CurrentImaginaryY,
               DeltaX, 
               NewIterCounts, 
               ConfigMap ).

%% Compute fractal (bounded or unbounded)
    %% FractalAlg is which algorithm (only one at moment)
% this function clause for creating julian fractals. eventually will generalize
addOnePoint(CurrentPixelX,
            CurrentPixelY,
            Xreal,
            Yimaginary,
            FractalAlg,
            IterCounts,
            ConfigMap) 
    when FractalAlg == julian ->

    CReal = maps:get(cReal,ConfigMap),
    CImaginary = maps:get(cImaginary,ConfigMap),
    MaxIterationThreshold = maps:get(maxIterationThreshold,ConfigMap),
    BailoutThreshold = maps:get(bailoutThreshold,ConfigMap),
    %% return IterCounts with new point added. Note 3rd item in tuple is Count, and 4th item in compute call is initial iteration
    Count = computeIterationValue(FractalAlg,
                                  CReal,
                                  CImaginary,
                                  Xreal,
                                  Yimaginary,
                                  0,                     % initial iteration count
                                  MaxIterationThreshold, 
                                  BailoutThreshold ),

    [ {CurrentPixelX, CurrentPixelY, Count } | IterCounts ].

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

