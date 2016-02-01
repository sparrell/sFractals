-module(make_png).
-author("Duncan Sparrell").
-license("Apache 2.0").
%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
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

%% given fractal data (in memory or in file),
%% turn it into a fractal image using png

%% public API
-export([
          make_png_from_data/2,         % create Png from block of fractal data
          make_png_from_file/1          % create Png from file of fractal data
          ]).

%% expose functions for test
-export([ ]).

%% public API for making fractal
make_png_from_file(#{dataFileName := DataFileName } = ConfigMap) ->
    %% get the data
    lager:debug("change make_png_from_file from consult to binary"),
    {ok, Rows} = file:consult( dataFileName ),

    %% make the png
    make_png_from_data(Rows, ConfigMap).


%% create a Png from block of fractal data
make_png_from_data(Rows, ConfigMap) ->
    %% initialize the png
    ThisPng = imagelib:startPng( ConfigMap ),

    %% add all the rows, one at a time, to the image
    addRows(ThisPng, Rows),

    %% finalize the png
    imagelib:finishPng( ThisPng ),

    ok.

addRows(_ThisPng, []) ->
    %% no rows left so done
    ok;

addRows(ThisPng, [ThisRow | RestOfRows ] ) ->
    %% add row to png
    imagelib:addRow( ThisRow, ThisPng ),

    %%recurse
    addRows(ThisPng, RestOfRows).
