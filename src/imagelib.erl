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
        when ColorAlg == simplest2 ->
    %% return palette of diff 12 colors
    {rgb, 8, [{000,000,000},   % 0=black
              {000,000,096},   % 1=dk blue
              {000,000,255},   % 2=blue
              {000,255,255},   % 3=cyan
              {255,000,000},   % 4=red
              {000,255,000},   % 5=green
              {210,180,140},   % 6=tan
              {255,165,000},   % 7=orange
              {240,128,128},   % 8=lightcoral
              {255,000,255},   % 9=fuschia
              {255,255,000},   % 10=yellow
              {255,255,255}    % 11=white
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
    %% return palette of 32 colors
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
              {255,255,255},   % 16=white
              {000,255,255},   % 17=cyan
              {000,000,255},   % 18=blue
              {255,000,000},   % 19=red
              {000,255,000},   % 20=green
              {255,255,000},   % 21=yellow
              {210,180,140},   % 22=tan
              {240,128,128},   % 23=lightcoral
              {255,165,000},   % 24=orange
              {128,128,000},   % 25=olive
              {255,000,255},   % 26=fuschia
              {000,000,255},   % 27=blue
              {000,255,255},   % 28=cyan
              {255,000,000},   % 29=red
              {255,255,255},   % 30=white
              {000,000,000}    % 31=black
              ] };

makeColorPalette(ColorAlg) 
        when ColorAlg == blue32 ->
    %% return palette of 32 colors with blue theme
    {rgb, 8, [{255,255,255},   % 0=white
              {255,000,255},   % 1=fuschia
              {001,067,071},   % 2=velvet ocean
              {007,009,102},   % 3=skyblue midnight
              {001,093,099},   % 4=dark ocean
              {012,015,102},   % 5=moonview skyblue
              {001,114,122},   % 6=deep sea dreams
              {011,016,140},   % 7=evening skyblue
              {001,135,145},   % 8=turquoise waves
              {014,078,173},   % 9=afternoon skyblue
              {002,150,161},   % 10=lulled by waves
              {016,127,201},   % 11=high skyblue
              {100,217,232},   % 12=the right time
              {035,176,219},   % 13=true noon
              {041,205,255},   % 14=almost teher
              {087,216,255},   % 15=opti-mist
              {143,229,255},   % 16=morning blue
              {204,243,255},   % 17=in your eyes
              {003,073,128},   % 18=blu1
              {005,093,161},   % 19=moving away
              {027,112,224},   % 20=almight blue
              {055,134,237},   % 21=another side
              {074,149,247},   % 22=anticipating
              {063,017,171},   % 23=llara's flight
              {128,185,242},   % 24=true joy
              {145,194,242},   % 25=spring sky
              {174,208,242},   % 26=smiling sky
              {184,213,242},   % 27=perfection
              {210,228,252},   % 28=lovliest
              {000,255,255},   % 29=cyan
              {255,255,255},   % 30=white
              {000,000,000}    % 31=black
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

