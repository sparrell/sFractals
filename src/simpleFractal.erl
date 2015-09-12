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

%%imagelib:makeImageFromData( ColorData, Width, Height, FractalImageFileName ) ->
    %% this function takes the x,y,color data and creates an image


simplFrac(_) ->
    ok.

colorData(_) ->
    ok.
