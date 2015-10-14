%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-module(dataFileSvr).
-author("Duncan Sparrell").
%% server for gathering fractal data and storing it in file
%% later maybe retrieving (and caching) it
%% one shot for now, maybe later multiple at once

%% gen_server design pattern
-behavior(gen_server).

%% client public API
-export([ 
          start/1,     % start the server with config info
          stop/0,      % stop
          addARow/1,   % add a row of data
          rowStatus/0  % print status of where at
          ]).

%% server public API
%% expose functions for test
-export([ init/1,             % init
          handle_call/3,      % sync requests
          handle_cast/2,      % async requests 
          terminate/2,        % cleanup
          handle_info/2,      % for later
          code_change/3       % for later
          ]).
 
%% client functions

start(ConfigMap) ->
    gen_server:start_link( {local, ?MODULE}, ?MODULE, ConfigMap, [] ).

stop() ->
    gen_server:cast( ?MODULE, stop ).

addARow( {RowNumber, Row} ) ->
    %% add a row to stored row data at position given
    %% fix later once structure works
    io:format("addARow: ~s, ~s~n", [RowNumber, Row] ),
    gen_server:cast(?MODULE, {addARow, {RowNumber, Row} }). 

rowStatus() ->
    %% print status of where at
    %% fix later once structure works
    io:format("rowStatus: ~n" ),
    gen_server:call(?MODULE, {rowStatus, junkForNow }). 

%% server functions
init(ConfigMap) ->     % initialization
    NumRowsLeft = 4000,                   % start with hardcoded (clean up later)  Number of Rows left to be received
    RowData = [],                         % start with empty row list
    {NumRowsLeft, RowData, ConfigMap }.   % return tuple for LoopData

%% sync requests
handle_call( {rowStatus, junkForNow }, _From, LoopData ) ->
    %% fix later
    io:format("rowStatus: got here~n"),
    {reply, "got to rowStatus", LoopData}.

%% async requests 
handle_cast(addARow, LoopData) ->
    io:format("handleCast:addARow: got here~n"),
    {noreply, LoopData};
handle_cast(stop, LoopData) ->
    io:format("handleCast:stop: got here~n"),
    {stop, normal, LoopData}.

terminate(Reason, _LoopData) -> 
    io:format("terminating: ~s ~n", [Reason] ).

handle_info(_Message, LoopData) -> {noreply, LoopData}.      % for later

code_change(_oldversion,LoopData,_Extra) -> {ok,LoopData}.   % for later
