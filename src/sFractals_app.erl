-module('sFractals_app').
%%%-------------------------------------------------------------------
%% @doc sFractals public API
%% @end
%%%-------------------------------------------------------------------

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


-behaviour(application).

%% Application callbacks
-export([start/2
        , start/0
        , stop/1]).

%%====================================================================
%% API
%%====================================================================

-spec start(_, _) -> { 'ok', pid() }.
start(_StartType, _StartArgs) ->
    true = cxy_ctl:init([{cfp, unlimited, 1000, 100000}]),
    %% had separate server but it got depreciated.
    %%   leaving in for future reinstantiation
    %%{ok, SfPid} = 'sFractals_sup':start_link(),
    WebServerReturn = start_webserver(),
    %%{ok, SfPid}.
    {ok, WebServerReturn}.

%%--------------------------------------------------------------------
-spec stop(_) -> 'ok'.
stop(_State) ->
    ok.

%%--------------------------------------------------------------------
-spec start() -> {'error', {atom(), _}} | {'ok', [atom()]}.
start() ->
    application:ensure_all_started(sFractals).
%%====================================================================
%% Internal functions
%%====================================================================

-spec start_webserver() -> 'ok'.
start_webserver() ->
    lager:info("cowboy webserver about to start"),
    Port = 8080,  % get this from config when clean this up
    ListenerCount = 5, % how many parralell listeners (don't need many)
    %%MaxConnections = 100, % how many can use at same tim
    IndexPage = {priv_file, sFractals, "index.html"},
    Routes = [
               {
                 '_'  %virtual hostname (any host name)
               , [ {"/status", status_handler, []}
                 , {"/static_assets/[...]"
                   , cowboy_static, { priv_dir
                                    , sFractals
                                    , "static/assets"
                                    }
                   }
                 , {"/images/[...]"
                   , cowboy_static, { priv_dir
                                    , sFractals
                                    , "images"
                                    }
                   }
                 , {"/", cowboy_static, IndexPage}
                 , {"/sFractal", fractal_handler, [] }
                 , {"/inputform", form_handler, [] }
                 ]
               }
             ],
    Dispatch = cowboy_router:compile(Routes),
    {ok, CowboyReturn} = cowboy:start_http( http
                               , ListenerCount
                               , [{port, Port}]
                               , [ {env, [{dispatch, Dispatch}]} ]
                               ),
    lager:debug("Cowboy starting: ~p", [CowboyReturn] ),
    CowboyReturn.




