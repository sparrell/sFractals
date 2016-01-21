%%%-------------------------------------------------------------------
%% @doc sFractals public API
%% @end
%%%-------------------------------------------------------------------

-module('sFractals_app').

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
-spec start() -> {'error',{atom(),_}} | {'ok',[atom()]}.
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




