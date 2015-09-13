%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-module(simpleFractal).
-author("Duncan Sparrell").
%% try to make simple fractal data and then turn into color data and then make image

%% API
-export([ simplFrac/1, colorData/4 ]).
 
%% What does a complex number look like?
-record(complex, {r=0.0, i=0.0}).


%% x_real, y_imaginary
%% x_real_left, x_real_right
%% y_real_high, y_real_low
%% box is bounded on left by x > x_real_left and bounded on right by x < x_real_right
%% box is bounded on top by y > y_imaginary_high and bounded on bottom by y > y_imaginary_low
%% box is width pixels wide and height pixels high
%% x,y are floating numbers
%% width, height are positive integers
%% w,h are positive intergers from 1 to width, height
%% step_x = (x_real_right - x_real_left) / width
%% step_y = (y_imaginary_high - y_imaginary_low) / height
%% 
%% x = x_real_right + N * step_x
%% y = y_imaginary_low + N * step_y
%% 
%% 
%% Inter_Count_Data = [ {iter, Iter=0,+},{ {x,X},{y,Y} },{ {w,W},{h,H} },
%% 
%% Iter is 0 or number_iterations to reach Threshold
%% 

%%ColorData = imagelib:colorizeData(IterationCountData),

%%imagelib:makeImageFromData( ColorData, Width, Height, FractalImageFileName ) ->
    %% this function takes the x,y,color data and creates an image


simplFrac(_) ->
    ok.

colorData(_,_,_,_) ->
    ok.

createPointData(Width,Height,X_real_right, X_real_left, Y_imaginary_low, Y_imaginary_high) 
    when X_real_right < X_real_left,
    when Y_imaginary_low < Y_imaginary_high,
    ->
    %% box is bounded on left by x > x_real_left and bounded on right by x < x_real_right
    %% box is bounded on top by y > y_imaginary_high and bounded on bottom by y > y_imaginary_low
    %% box is width pixels wide and height pixels high

    %% step is floating range divided by number of pixels
    DeltaX = (X_real_left - X_real_right) / Width,
    DeltaY = (Y_imaginary_high - Y_imaginary_low) / Height,

    %% define horizontal and vertical steps in floating point
    %PointBox = [ { {X,Y}, {X_real_right + X*DeltaX, Y_imaginary_low + Y*DeltaY} ||
    %    X <- lists:seq(1, Width), Y <- lists:seq(1, Height) ],
        
    %% define horizontal and vertical steps in floating point, and compute iteration value
    [ { {X,Y}, computeIterationValue(X_real_right + X*DeltaX, Y_imaginary_low + Y*DeltaY) }
        || X <- lists:seq(1, Width), Y <- lists:seq(1, Height) ].
or shouldthis be recursive function walking thru x,y to get adds instead of multiply
pointbox = [ {X,Y} || X <- lists:seq(1, Width), Y <- lists:seq(1, Height) ].
-------------
or build pointbox as you go recursively
iter box = recurseThruRowsY of recurse thru pointsX of create real/imag and recurse to get iter count

    IterCounts = makeAllRows(Width,Height,Height,XRealLeft,YImaginaryHigh,DeltaX,DeltaY,C0,Z0,FractalAlg),
        % note the 3rd parameter is "CurrentPixelY" which starts at top

%% make one row at a time, starting at top and working down (incase later we might to build file on fly that way)
%%%makeAllRows(Width,Height,X_real_left,Y_imaginary_high,DeltaX,DeltaY,C0,Z0,FractalAlg) ->
%%%    makeAllRows(extra elements including Current_Real_Y{start at Y_imaginary_high and inc by DeltaY}
%%%                and including IterCount as accumulator{starting empty);

% function head for done
makeAllRows(_Width,_Height,CurrentPixelY,_CurrentRealX,_CurrentImaginaryY,_DeltaX,_DeltaY,_C0,_Z0,_FractalAlg,IterCounts)
        when CurrentPixelY >= 1 ->  % should be 1 or zero or -1??
    IterCounts;   %reached bottom so return the iteration count data

% function head for not done
makeAllRows(Width,Height,CurrentPixelY,CurrentRealX,CurrentImaginaryY,DeltaX,DeltaY,C0,Z0,FractalAlg,IterCounts) ->
    % reached here so need to make another row
    makeAllRows(Width,Height,CurrentPixelY-1,CurrentRealX,CurrentImaginaryY-DeltaY,DeltaX,DeltaY,C0,Z0,FractalAlg,
        makePoints(fill in) ).
        % note Y-1 one since doing top down
        % similarly Imaginary& went down by delta
		% IterCounts is updated with the new row calculated by makePoints
    

------------
pointbox2data(_Itermax,_ValueBound,...,_restofpointbox=[],databox) -> databox;
pointbox2data(Itermax,ValueBound,LastRealX,DeltaX,C0,Z0,...,restofpointbox=[H|T],databox) -> 
add H to databox, recurse on T
H: lastRealX +deltaX = realX,
;
        
    
computeIterationValue(Xreal,Yimaginary) ->
    BailoutThreshold = 2,
    MaxIterationThreshold = 10,
    C = { complex, {r=1,i=-1}, %make a record
    Z0 = { complex, {r=Xreal,i=Yimaginary}, %make a record
    computeIterationValue(C, Z0, 0
need guard for abs value of x/y > bailout
maybe precompute abs value and include in call to computeIterationValue so could put guard on it
