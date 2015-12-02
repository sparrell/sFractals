%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

%%        when ColorAlg == simplest ->
    %% return palette of 12 colors
-define(SIMPLEST_PALETTE_A, {rgb, 8, [{255,255,255},   % 0=white
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
                                ] } ).
