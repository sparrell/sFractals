%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-module(imagelib).
-author("Duncan Sparrell").
%% make images
%% inspirations included ??

%% API
-export([ makeImageFromData/4 ]).
 
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

makeImageFromData( ColorData, Width, Height, FractalImageFileName ) ->
    %% this function takes the x,y,color data and creates an image

    %% create a blank image object
    FractalImage = egd:create(Width, Height),

    %% call function to recurse thru list adding each point
    addAllData(FractalImage, ColorData),
    
    %%  render and save in file
    egd:save(egd:render(FractalImage, png), FractalImageFileName),

    %% clean up (is this needed?)
    egd:destroy(FractalImage).

addAllData( FractalImage, [ColorDataHead | ColorDataTail] ) ->
    %% this function recurses thru the data adding each point
    %% each point is of form {X,Y,ColorValue}
    addOnePoint(FractalImage, ColorDataHead),
    addAllData(FractalImage, ColorDataTail);
addAllData( _FractalImage, [] ) ->
    %% done
    ok.


addOnePoint(FractalImage, {X, Y, ColorValue}) ->
    %% this function adds one data point to image
    egd:line(FractalImage, {X, Y}, {X, Y}, ColorValue).
