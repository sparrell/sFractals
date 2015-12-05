%%%-------------------------------------------------------------------
%% @doc sFractals top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('sFractals_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{
                 strategy => one_for_all
                 , intensity => 0
                 , period => 1
                },
    FractalSvrSpec = #{
                     id => fractal_server
                     , start => {fractal_server, start_link, []}
                     , restart => permanent   % make sure always there
                     , shutdown => brutal_kill
                     , type => worker
                     , module => [fractal_server]
                     },
    ChildSpecs = [FractalSvrSpec],
    {ok, { SupFlags, ChildSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================
