%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%% @doc imagelib contains routines for making fractal image
%%% @end
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
    PngFileName = maps:get(imageFileName,ConfigMap),
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

-include("colorPaletteSimplest12.hrl").
-include("colorPaletteSimplest12b.hrl").
-include("colorPaletteSimple16.hrl").
-include("colorPaletteSimple32.hrl").
-include("colorPaletteBlue32.hrl").
-include("colorPaletteSimple64.hrl").
makeColorPalette(ColorAlg)
        when ColorAlg == simplest ->
    %% return palette of 12 colors
    ?SIMPLEST_PALETTE_A;

makeColorPalette(ColorAlg)
        when ColorAlg == simplest2 ->
    %% return palette of diff 12 colors
    ?SIMPLEST_PALETTE_B;

makeColorPalette(ColorAlg)
        when ColorAlg == simple16 ->
    %% return palette of 16 colors
    ?SIMPLE_PALETTE;

makeColorPalette(ColorAlg)
        when ColorAlg == simple32 ->
    %% return palette of 32 colors
    ?SIMPLE32_PALETTE;

makeColorPalette(ColorAlg)
        when ColorAlg == blue32 ->
    %% return palette of 32 colors with blue theme
    ?BLUE32_PALETTE;

makeColorPalette(ColorAlg)
        when ColorAlg == simple64 ->
    %% return palette of 64 colors with blue theme
    ?SIMPLE64_PALETTE.

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
    PngFileName = maps:get(imageFileName,ConfigMap),
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

