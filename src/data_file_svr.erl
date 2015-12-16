%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%%
%%%-------------------------------------------------------------------

-module(data_file_svr).
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
          rowStatus/0,  % print status of where at
          writeDataFile/0  % write file
          ]).

%% server public API
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
    lager:debug("stopping dataFileSvr ~n" ),
    gen_server:cast( ?MODULE, stop ).

addARow( {RowNumber, Row} ) ->
    %% add a row to stored row data at position given
    lager:debug("addARow client: ~p, ~p~n", [RowNumber, Row] ),
    gen_server:cast(?MODULE, {addARow, {RowNumber, Row} }).

rowStatus() ->
    %% print status of where at
    %% fix later once structure works
    lager:debug("rowStatus client: ~n" ),
    gen_server:call(?MODULE, {rowStatus, junkForNow }).

writeDataFile() ->
    %% called once all data reaches server
    %% should svr figure when to write out itself?
    %% validate enough rows?
    %% wait for more if not? timeout?
    gen_server:cast(?MODULE, writeDataFile).

%% server functions
init(ConfigMap) ->     % initialization
    lager:debug("starting dataFileSvr ~n" ),

    %% Number of Rows left to be received starts at height
    NumRowsLeft = maps:get(height, ConfigMap),

    RowData = [],                              % start with empty row list

    %% add NumRowsLeft and RowData and ConfigMap to make new map for status
    InterimStatusMap = maps:update(num_rows_left,NumRowsLeft,ConfigMap),
    StatusMap = maps:update(row_data,RowData,InterimStatusMap),

    { ok, StatusMap }. % return tuple for LoopData

%% sync requests
handle_call( {whatcall, StatusMap}
           , _From
           , {NumRowsLeft, RowData, ConfigMap}
           ) ->
    #{ num_rows_left := NumRowsLeft, RowData := RowData} = StatusMap,
    %% fix later
    lager:debug("rowStatus svr: ~p, ~p~n", [NumRowsLeft, RowData] ),
    {reply, "reply to rowStatus", StatusMap}.

%% async requests
handle_cast( {addARow, {RowNumber, RowToAdd}}
           , {NumRowsLeft , RowData , ConfigMap}
           )
        when NumRowsLeft > 0 ->
    %% this case when crunching thru the rows and not at last one yet
    lager:debug("handleCast svr:addARow: NumRowsLeft=~p~n",[NumRowsLeft]),
    NewNumRowsLeft = NumRowsLeft - 1,
    NewRowData = [ {RowNumber, RowToAdd} | RowData ],
    {noreply, {NewNumRowsLeft, NewRowData, ConfigMap}};
handle_cast(writeDataFile, {NumRowsLeft, RowData, ConfigMap}) ->
    %% this case when have everything to write
    lager:debug("CastSvr:writeDataFile: got here~n"),
    lager:debug("    NumRowsLeft = ~p~n",[NumRowsLeft]),
    lager:debug("    RowData = ~p~n",[RowData]),
    %% sort and make list of rows
    %%    RowData is unsorted list of tuples of form RowNumber,Row
    %%    Sort on RowNumbner
    SortedRowTuples = lists:sort(RowData),
    lager:debug("CastSvr:writeDataFile:SortedRowTuples=~p~n",[SortedRowTuples]),
    %% remove the RowNumber
    SortedRowData = [ ThisRow || {_, ThisRow} <- SortedRowTuples],
    lager:debug("CastSvr:writeDataFile: SortedRowData=~p~n",[SortedRowData]),

    %% get config and open file
    DataFileName = maps:get(dataFileName, ConfigMap),  % where to write datafile
    {ok, DataFile} = file:open(DataFileName, [write]),

    %% write to file until out of rows
    lists:foreach( fun(ThisRow) ->
          io:format(DataFile, "~w.~n",[ThisRow]) end, SortedRowData),

    %% close file
    file:close(DataFile),

    %% return? stop?
    {noreply, {NumRowsLeft, RowData, ConfigMap}};
handle_cast(stop, LoopData) ->
    lager:debug("handleCast:stop: got here~n"),
    {stop, normal, LoopData}.

terminate(Reason, _LoopData) ->
    lager:debug("terminating: ~p ~n", [Reason] ).

handle_info(_Message, LoopData) -> {noreply, LoopData}.      % for later

code_change(_OldVersion, LoopData, _Extra) -> {ok, LoopData}.   % for later
