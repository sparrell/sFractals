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
%% need to add function to colorize iter data
 
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
