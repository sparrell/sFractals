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
-export([ analyzeData/1, makePng/1,
          finishPng/1, addRow/2, startPng/1 ]).


% public api
analyzeData(CountData) ->
    % go thru grabbing 3rd element (the value) and count them in a dictionary. return the dict
    lists:foldl(fun( {_,_,V}, OldDict ) -> orddict:update_counter(V,1, OldDict) end,
         orddict:new(),
         CountData).

makePng( ConfigMap ) ->
    Width  = maps:get(width, ConfigMap),
    Height = maps:get(height,ConfigMap),
    PngFileName = maps:get(fractalImageFileName,ConfigMap),
    ColorAlg = maps:get(colorAlg,ConfigMap),

    ColorPalette = makeColorPalette(ColorAlg),

    {ok, PngFile} = file:open(PngFileName, [write]),
    Png = png:create(#{size => {Width, Height},
                       mode => {indexed, 8},
                       file => PngFile,
                       palette => ColorPalette}),

    ok = append_rows(Png),

    ok = png:close(Png),
    ok.

makeColorPalette(ColorAlg) 
        when ColorAlg == simplest ->
    %% return palette of 12 colors
    {rgb, 8, [{255,255,255},   % 0=white
              {000,255,255},   % 1=cyan
              {000,000,255},   % 2=blue
              {255,000,000},   % 3=red
              {000,255,000},   % 4=green
              {255,255,000},   % 5=yellow
              {210,180,140},   % 6=tan
              {240,128,128},   % 7=lightcoral
              {255,165,000},   % 8=orange
              {128,128,000},   % 9=olive
              {255,000,255},   % 10=fuschia
              {000,000,000}   % 11=black
              ] };

makeColorPalette(ColorAlg) 
        when ColorAlg == simple16 ->
    %% return palette of 16 colors
    {rgb, 8, [{255,255,255},   % 0=white
              {000,255,255},   % 1=cyan
              {000,000,255},   % 2=blue
              {255,000,000},   % 3=red
              {000,255,000},   % 4=green
              {255,255,000},   % 5=yellow
              {210,180,140},   % 6=tan
              {240,128,128},   % 7=lightcoral
              {255,165,000},   % 8=orange
              {128,128,000},   % 9=olive
              {255,000,255},   % 10=fuschia
              {000,000,255},   % 11=blue
              {000,255,255},   % 12=cyan
              {255,000,000},   % 13=red
              {255,255,255},   % 14=white
              {000,000,000}   % 15=black
              ] };

makeColorPalette(ColorAlg) 
        when ColorAlg == simple32 ->
    %% return palette of 16 colors
    {rgb, 8, [{255,255,255},   % 0=white
              {000,255,255},   % 1=cyan
              {000,000,255},   % 2=blue
              {255,000,000},   % 3=red
              {000,255,000},   % 4=green
              {255,255,000},   % 5=yellow
              {210,180,140},   % 6=tan
              {240,128,128},   % 7=lightcoral
              {255,165,000},   % 8=orange
              {128,128,000},   % 9=olive
              {255,000,255},   % 10=fuschia
              {000,000,255},   % 11=blue
              {000,255,255},   % 12=cyan
              {255,000,000},   % 13=red
              {000,255,000},   % 14=green
              {255,000,255},   % 15=fuschia
              {255,255,255},   % 0=white
              {000,255,255},   % 1=cyan
              {000,000,255},   % 2=blue
              {255,000,000},   % 3=red
              {000,255,000},   % 4=green
              {255,255,000},   % 5=yellow
              {210,180,140},   % 6=tan
              {240,128,128},   % 7=lightcoral
              {255,165,000},   % 8=orange
              {128,128,000},   % 9=olive
              {255,000,255},   % 10=fuschia
              {000,000,255},   % 11=blue
              {000,255,255},   % 12=cyan
              {255,000,000},   % 13=red
              {255,255,255},   % 14=white
              {000,000,000}   % 15=black
              ] }.

append_rows(Png) ->
    append_row(Png, 0).


append_row(#{size := {_, Height}}, Height) ->
    ok;

append_row(#{size := {Width, _Height}} = Png, Y) ->
    Thickness = Width div 4,
    F = fun
            (X) when X > (Y + Thickness) -> 2;
            (X) when (X + Thickness) < Y -> 1;
            (_) -> 0 end,
    Row = lists:map(F, lists:seq(1, Width)),
    png:append(Png, {row, Row}),
    append_row(Png, Y + 1).

%%%%
%% 3-part API - startPng, addRow, finishPng
%%%%

%% Open the png file and return the png object
startPng( ConfigMap ) ->
    Width  = maps:get(width, ConfigMap),
    Height = maps:get(height,ConfigMap),
    PngFileName = maps:get(fractalImageFileName,ConfigMap),
    ColorAlg = maps:get(colorAlg,ConfigMap),

    ColorPalette = makeColorPalette(ColorAlg),

    {ok, PngFile} = file:open(PngFileName, [write]),

    %% return Png
    png:create(#{size => {Width, Height},
                       mode => {indexed, 8},
                       file => PngFile,
                       palette => ColorPalette}).

%% add a row to Png and return new Png
addRow(RowData, Png ) ->
    png:append(Png, {row, RowData}).

%% Finish up closing Png
finishPng( Png ) ->
    ok = png:close(Png).

