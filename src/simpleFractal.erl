%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-module(simpleFractal).
-author("Duncan Sparrell").
%% try to make simple fractal data and then turn into color data and then make image

%% API
-export([ simplFrac/1 ]).
 


% create a simple fractal
simplFrac(_) ->

    %% FractalConfig = {FractalAlg,CReal,CImaginary, ZReal,ZImaginary,BailoutThreshold,MaxIterationThreshold}
    %% simple config
    FractalConfig = {julian,0.5,-0.5, 0,0,4,100},
        %% FractalAlg = julian
        %% CReal = 0.5
        %% CImaginary = -0.5
        %% ZReal = 0 (don't care for Julian)
        %% ZImaginary = 0 (don't care for Julian)
        %% BailoutThreshold = 4
        %% MaxIterationThreshold = 100
    FractalImageFileName = "./myFirstFractal.png",
    Width = 10,
    Height = 10,

    %% first create julian data set with some hardcoded configs (add configing later)
        %% XRealRight = 3.0 
        %% XRealLeft = -3.0
        %% YImaginaryLow = -3.0
        %% YImaginaryHigh = 3.0
    IterationCountData = createPointData(Width,Height,-3.0, 3.0, -3.0, 3.0, FractalConfig),

    %% print some stats on distribution of counts
    %% need to do

    %% colorize the data
%%  OtherParameters = {0,0},
%%  ColorData = imagelib:colorizeData(IterationCountData,OtherParameters),

    %% take the x,y,color data and creates an image
%%  imagelib:makeImageFromData( ColorData, Width, Height, FractalImageFileName ),

%%  ok.
    IterationCountData.

createPointData(Width,Height, XRealLeft, XRealRight, YImaginaryLow, YImaginaryHigh, FractalConfig) 
    when XRealLeft < XRealRight, YImaginaryLow < YImaginaryHigh ->
    %% box is bounded on left by x > XRealLeft and bounded on right by x < XRealRight
    %% box is bounded on top by y > YImaginaryHigh and bounded on bottom by y > YImaginaryLow
    %% box is width pixels wide and height pixels high

    %% step is floating range divided by number of pixels
    DeltaX = (XRealRight - XRealLeft) / Width,
    DeltaY = (YImaginaryHigh - YImaginaryLow) / Height,

    makeAllRows(Width,Height,1,XRealLeft,YImaginaryLow,DeltaX,DeltaY,FractalConfig,[]).
        %% note the 3rd parameter is "CurrentPixelY" which starts at bottom
        %% note the 9th parameter is "IterCounts" which starts as empty list
		%% makeAllRows returns InterCount, which is then returned by createPointData 

%% make one row at a time, starting at bottom and working up 
%%  note builds list from tail so this yeilds highest Y nearest front 
%%  which is useful in case later we might to build file on fly that way 'from top'

% function head for done
makeAllRows(_Width,Height, CurrentPixelY,_XRealLeft,_CurrentImaginaryY,_DeltaX,_DeltaY,_FractalConfig,IterCounts)
        when CurrentPixelY > Height ->  
    IterCounts;   %reached bottom so return the iteration count data

% function head for not-done so recurse
makeAllRows(Width,Height,CurrentPixelY,XRealLeft,CurrentImaginaryY,DeltaX,DeltaY,FractalConfig,IterCounts) ->
    % reached here so need to make another row
    % note Y-1 one since doing top down
    % similarly Imaginary& went down by delta
	% IterCounts is updated with the new row calculated by makePoints and is the returned value
    makeAllRows(Width,Height,CurrentPixelY+1,XRealLeft,CurrentImaginaryY+DeltaY,DeltaX,DeltaY,FractalConfig,
        makePoints(Width,1,CurrentPixelY,XRealLeft, CurrentImaginaryY,DeltaX,FractalConfig,IterCounts) ).
    
% function head for done-with-row
makePoints(Width,CurrentPixelX,_CurrentPixelY,_CurrentRealX,_CurrentImaginaryY,_DeltaX,_FractalConfig,IterCounts)
        when CurrentPixelX > Width ->  
    IterCounts;   %reached end of line so return the iteration count data

% function head for not-done so recurse
makePoints(Width,CurrentPixelX,CurrentPixelY,CurrentRealX,CurrentImaginaryY,DeltaX,FractalConfig,IterCounts) ->   
    %reached here so need to add another pixel
    % CurrentPixelX starts at 1 increases by 1 each recurse
    % CurrentRealX starts at XRealLeft and increases by DeltaX each recurse
    % IterCounts has point {X,Y,Iter} added by function addOnePoint
    makePoints(Width,CurrentPixelX+1,CurrentPixelY,CurrentRealX+DeltaX,CurrentImaginaryY,DeltaX,FractalConfig,
        addOnePoint(CurrentPixelX,CurrentPixelY,CurrentRealX,CurrentImaginaryY,FractalConfig,IterCounts) ).
        %% note addOnePoint returns the new value of IterCounts into the recursive call

%% Compute fractal (bounded or unbounded)
    %% FractalConfig = {FractalAlg,CReal,CImaginary, ZReal,ZImaginary,BailoutThreshold,MaxIterationThreshold}
    %% FractalAlg is which algorithm (only one at moment)
% this function clause for creating julian fractals. eventually will generalize
addOnePoint(CurrentPixelX,CurrentPixelY,Xreal,Yimaginary,{FractalAlg,CReal,CImaginary, ZReal,ZImaginary,BailoutThreshold,MaxIterationThreshold},IterCounts) 
    when FractalAlg == julian ->

    %% return IterCounts with new point added. Note 3rd item in tuple is Count, and 4th item in compute call is initial iteration
    [ {CurrentPixelX, CurrentPixelY,
        computeIterationValue(CReal,CImaginary,Xreal,Yimaginary,
            0, MaxIterationThreshold, BailoutThreshold ) } | IterCounts ].

%% computeIterationValue computes fractal value and returns iteration count
%% function clause for exceeding iteration count
computeIterationValue(_CReal, _CImaginary, _ZReal,_ZImaginary, 
            IterCount, MaxIterationThreshold, _BailoutThreshold )
        when IterCount >= MaxIterationThreshold ->   % reached iteration limit so return count=limit
    MaxIterationThreshold;
%% function clause for exceeding bound
computeIterationValue(_CReal,_CImaginary, ZReal,ZImaginary, IterCount, 
            _MaxIterationThreshold, BailoutThreshold ) 
        when ((ZReal*ZReal)+(ZImaginary*ZImaginary)) > BailoutThreshold -> %bailout exceeded so return iterCount
    IterCount;
%% function clause for recursing further
computeIterationValue(CReal,CImaginary, ZReal,ZImaginary, IterCount, 
        MaxIterationThreshold, BailoutThreshold ) ->
    computeIterationValue(CReal,CImaginary, (ZReal*ZReal) - (ZImaginary*ZImaginary) + CReal, (2 * ZReal * ZImaginary) + CImaginary, IterCount+1, MaxIterationThreshold, BailoutThreshold ).

