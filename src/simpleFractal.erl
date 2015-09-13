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

    %%create the x,y box first
    PixelBox = createPixelBox(Width,Height)),

    %% step is floating range divided by number of pixels
    DeltaX = (X_real_left - X_real_right) / Width,
    DeltaY = (Y_imaginary_high - Y_imaginary_low) / Height,

    %% define horizontal and vertical steps in floating point
    PointBox = [ { {X,Y}, {X_real_right + X*DeltaX, Y_imaginary_low + Y*DeltaY} ||
        X <- lists:seq(1, Width), Y <- lists:seq(1, Height) ],
        
    %% define horizontal and vertical steps in floating point, and compute iteration value
    IterBox = [ { {X,Y}, computeIterationValue(X_real_right + X*DeltaX, Y_imaginary_low + Y*DeltaY) }
        || X <- lists:seq(1, Width), Y <- lists:seq(1, Height) ],
        
    
