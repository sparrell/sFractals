-module(imagelib).
-author("Duncan Sparrell").
-license("Apache 2.0").
%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%%
%%% @doc imagelib contains routines for making fractal image
%%% @end
%%%
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are
%%% met:
%%%
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%%
%%% * The names of its contributors may not be used to endorse or promote
%%%   products derived from this software without specific prior written
%%%   permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------

%% make images
%% inspirations included ??

%%%-------------------------------------------------------------------
-include("colorPaletteSimplest12.hrl").
-include("colorPaletteSimplest12b.hrl").
-include("colorPaletteSimple16.hrl").
-include("colorPaletteSimple32.hrl").
-include("colorPaletteBlue32.hrl").
-include("colorPaletteSimple64.hrl").
%%%-------------------------------------------------------------------

%% API
-export([ analyzeData/1, makePng/1,
          finishPng/1, addRow/2, startPng/1 ]).


% public api
analyzeData(CountData) ->
    %% go thru grabbing 3rd element (the value)
    %%    and count them in a dictionary. return the dict
    lists:foldl( fun( {_, _, V}, OldDict ) ->
                       orddict:update_counter(V, 1, OldDict) end
               , orddict:new()
               , CountData
               ).

makePng( ConfigMap ) ->
    Width  = maps:get(width, ConfigMap),
    Height = maps:get(height, ConfigMap),
    PngFileName = maps:get(imageFileName, ConfigMap),
    ColorAlg = maps:get(colorAlg, ConfigMap),

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
    Height = maps:get(height, ConfigMap),
    PngFileName = maps:get(imageFileName, ConfigMap),
    ColorAlg = maps:get(colorAlg, ConfigMap),

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

