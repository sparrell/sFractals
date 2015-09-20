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
-export([ makeImageFromData/2, colorizeData/3, analyzeData/1 ]).
%% need to add function to colorize iter data
 
makeImageFromData( ColorData, Config ) ->
    %% this function takes the x,y,color data and creates an image

    %% create a blank image object
    FractalImage = egd:create( maps:get(width,Config), maps:get(height,Config)),

    %% call function to recurse thru list adding each point
    addAllData(FractalImage, ColorData),
    
    %%  render and save in file
    egd:save(egd:render(FractalImage, png), maps:get(fractalImageFileName,Config)),

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

% take count data and turn into colors based on color algorithm and parameters
colorizeData(CountData,ColorAlg, Parameters) 
    when ColorAlg == simplest ->
    ColorDict = orddict:from_list( [ {0, egd:color({255,255,255})},   % white
                                     {1, egd:color({0,255,255})},   %cyan
                                     {2, egd:color({0,0,255})},  % blue
                                     {3, egd:color({255,0,0})},  % red
                                     {4, egd:color({0,255,0})},  % green
                                     {5, egd:color({255,255,0})}, % yellow
                                     {6, egd:color({210,180,140})}, %tan
                                     {7, egd:color({240,128,128})}, %lightcoral
                                     {8, egd:color({255,165,0})}, %orange
                                     {9, egd:color({128,128,0})}, %olive
                                     {10, egd:color({255,0,255})}, %fuschia
                                     {11, egd:color({0,0,0})}  %black
                                    ] ),
  
    %% simplest alg is counts 0 thru 10 map to 11 colors (all others white)
    colorizeData(CountData,ColorAlg, Parameters, [],ColorDict).  %% accumulator is ColorData

colorizeData([],_ColorAlg, _Parameters, ColorData,_ColorMap) -> % CountData empty so work is done
    ColorData;
colorizeData(CountData,ColorAlg, Parameters,ColorData,ColorDict) 
        when ColorAlg == simplest ->
        %% simplest alg is counts 0 thru 11 map to 12 colors 
    [ {X,Y,Count} | NewCountData ] = CountData,    %get point at head of list
    % convert Count to Color
    Color = orddict:fetch(Count,ColorDict),
    colorizeData(NewCountData,ColorAlg, Parameters,[ {X,Y,Color} | ColorData], ColorDict).

% public api
analyzeData(CountData) ->
    % go thru grabbing 3rd element (the value) and count them in a dictionary. return the dict
    lists:foldl(fun( {_,_,V}, OldDict ) -> orddict:update_counter(V,1, OldDict) end,
         orddict:new(),
         CountData).
