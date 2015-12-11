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

start(_StartType, _StartArgs) ->
    {ok, SfPid} = 'sFractals_sup':start_link(),
    start_webserver(),
    {ok, SfPid}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%--------------------------------------------------------------------
start() ->
    application:ensure_all_started(sFractals).
%%====================================================================
%% Internal functions
%%====================================================================

start_webserver() ->
    lager:info("cowboy webserver about to start"),
    Port = 8080,  % get this from config when clean this up
    ListenerCount = 5, % how many parralell listeners (don't need many)
    MaxConnections = 100, % how many can use at same tim
    StaticPages = [ { directory, {priv_dir, sFractals, []} }
                  , {mimetypes, [{<<".html">>, [<<"text/html">>]}]}
                  ],
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
                 , {"/", cowboy_static, IndexPage}
                 , {"/sFractal", fractal_handler, [] }
                 ]
               }
             ],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_http( http
                               , ListenerCount
                               , [{port, Port}]
                               , [ {env, [{dispatch, Dispatch}]} ]
                               ),
    ok.




