%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-module(simpleFractal).
-author("Duncan Sparrell").
%% try to make simple fractal data and then turn into color data and then make image

%% API
-export([ simplFrac/1, /4 ]).
 
%%ColorData = imagelib:colorizeData(IterationCountData),

%%imagelib:makeImageFromData( ColorData, Width, Height, FractalImageFileName ) ->
    %% this function takes the x,y,color data and creates an image


% create a simple fractal
simplFrac(_) ->

    %% FractalConfig = {FractalAlg,C0,Z0,BailoutThreshold,MaxIterationThreshold}
    %% simple config
    FractalConfig = {julian,C0,Z0,BailoutThreshold,MaxIterationThreshold}

    %% first create julian data set with some hardcoded configs (add configing later)
    IterationCountData = createPointData(Width,Height,X_real_right, X_real_left, Y_imaginary_low, Y_imaginary_high),

    %% print some stats on distribution of counts
    %% need to do

    %% colorize the data
    ColorData = imagelib:colorizeData(IterationCountData,OtherParameters),

    ok.


createPointData(Width,Height,X_real_right, X_real_left, Y_imaginary_low, Y_imaginary_high) 
    when X_real_right < X_real_left,
    when Y_imaginary_low < Y_imaginary_high ->
    %% box is bounded on left by x > x_real_left and bounded on right by x < x_real_right
    %% box is bounded on top by y > y_imaginary_high and bounded on bottom by y > y_imaginary_low
    %% box is width pixels wide and height pixels high

    %% step is floating range divided by number of pixels
    DeltaX = (X_real_left - X_real_right) / Width,
    DeltaY = (Y_imaginary_high - Y_imaginary_low) / Height,

    makeAllRows(Width,Height,XRealLeft,YImaginaryHigh,DeltaX,DeltaY,FractalConfig,[]).
        %% note the 2nd parameter is "CurrentPixelY" which starts at top so starts at "Height"
        %% note the 8th parameter is "IterCounts" which starts as empty list
		%% makeAllRows returns InterCount which in turn createPointData returns

%% make one row at a time, starting at top and working down (incase later we might to build file on fly that way)

% function head for done
makeAllRows(_Width,CurrentPixelY,_XRealLeft,_CurrentImaginaryY,_DeltaX,_DeltaY,_FractalConfig,IterCounts)
        when CurrentPixelY =< 0 ->  % should be 1 or zero or -1??
    IterCounts;   %reached bottom so return the iteration count data

% function head for not-done so recurse
makeAllRows(Width,CurrentPixelY,XRealLeft,CurrentImaginaryY,DeltaX,DeltaY,FractalConfig,IterCounts) ->
    % reached here so need to make another row
    % note Y-1 one since doing top down
    % similarly Imaginary& went down by delta
	% IterCounts is updated with the new row calculated by makePoints and is the returned value
    makeAllRows(Width,CurrentPixelY-1,XRealLeft,CurrentImaginaryY-DeltaY,DeltaX,DeltaY,FractalConfig,
        makePoints(Width,1,CurrentPixelY,XRealLeft, CurrentImaginaryY,DeltaX,FractalConfig,IterCounts) ).
    
% function head for done-with-row
makePoints(Width,CurrentPixelX,_CurrentPixelY,_CurrentRealX,_CurrentImaginaryY,_DeltaX,_FractalConfig,IterCounts)
        when CurrentPixelX >= Width ->  % width+1? greater only?
    IterCounts;   %reached end of line so return the iteration count data

% function head for not-done so recurse
makePoints(Width,CurrentPixelX,CurrentPixelY,CurrentRealX,CurrentImaginaryY,DeltaX,FractalConfig,IterCounts) ->   
    %reached here so need to add another pixel
    % CurrentPixelX starts at 1 increases by 1 each recurse
    % CurrentRealX starts at XRealLeft and increases by DeltaX each recurse
    % IterCounts has point {X,Y,Iter} added by function addOnePoint
    makePoints(Width,CurrentPixelX+1,CurrentPixelY,CurrentRealX+DeltaX,CurrentImaginaryY,DeltaX,FractalConfig,
        addOnePoint(CurrentRealX,CurrentImaginaryY,FractalConfig,IterCounts) ).
        %% note addOnePoint returns the new value of IterCounts into the recursive call

%% Compute fractal (bounded or unbounded)
    %% FractalConfig = {FractalAlg,CReal,CImaginary, ZReal,ZImaginary,BailoutThreshold,MaxIterationThreshold}
    %% FractalAlg is which algorithm (only one at moment)
% this function clause for creating julian fractals. eventually will generalize
addOnePoint(Xreal,Yimaginary,{FractalAlg,CReal,CImaginary, ZReal,ZImaginary,BailoutThreshold,MaxIterationThreshold},IterCounts) 
    when FractalAlg == julian ->

    %% return IterCounts with new point added. Note 3rd item in tuple is Count, and 4th item in compute call is initial iteration
    [ {Xreal, Yimaginary, computeIterationValue(CReal,CImaginary,ZReal,ZImaginary,0, MaxIterationThreshold ) } | IterCounts ].

%% computeIterationValue computes fractal value and returns iteration count
%% function clause for exceeding iteration count
computeIterationValue(CReal,CImaginary, ZReal,ZImaginary, iterCount, MaxIterationThreshold )
        when iterCount >= MaxIterationThreshold ->   % reached iteration limit so return count=limit
    MaxIterationThreshold;
%% function clause for exceeding bound
computeIterationValue(CReal,CImaginary, ZReal,ZImaginary, iterCount, MaxIterationThreshold ) 
        when ((ZReal*ZReal)+(ZImaginary*ZImaginary)) > BailoutThreshold -> %bailout exceeded so return iterCount
    iterCount;
%% function clause for recursing further
computeIterationValue(CReal,CImaginary, ZReal,ZImaginary, iterCount, MaxIterationThreshold ) ->
    ZRealNew = (ZReal*ZReal) - (ZImaginary*ZImaginary) + CReal,
    ZImaginaryNew = 2 * ZReal * ZImaginary + CImaginary,
    computeIterationValue(CReal,CImaginary, (ZReal*ZReal) - (ZImaginary*ZImaginary) + CReal, (2 * ZReal * ZImaginary) + CImaginary, iterCount+1, MaxIterationThreshold ).

