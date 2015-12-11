%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%%
%%%-------------------------------------------------------------------

-module(other_compute).
-author("Duncan Sparrell").
%% routines for making fractals

%% public API
-export([
          make_png/1,                    % create fractal data and make a png
          make_png_using_pool/1,           % create fractal data and make a png
          collect_rows/4,                % process to catch fractal data
          create_fractal_workers/5,       % process to make workers
          fractal_worker/5,              % worker to create one row of data
          compute_row/4,    % create one row of fractal data
          compute_rows/1 % create one row of fractal data
          ]).

%% expose functions for test
-export([ compute_xlist/1, compute_ylist/1
          ]).

%%%%%%%%
%% make_png
%%     ConfigMap - contains config info
%%               add parameters here to explain api
%%%%%%%%

-spec make_png(ConfigMap::map()) -> ok.
make_png(ConfigMap) ->
    FractalAlg = maps:get(fractalAlg, ConfigMap),
    XList = compute_xlist(ConfigMap),
    YList = compute_ylist(ConfigMap),

    %% initialize the png
    ThisPng = imagelib:startPng( ConfigMap ),

    %% add all the rows, one at a time, to the image
    addRowsToPng(FractalAlg, ThisPng, XList, YList, ConfigMap),

    %% finalize the png
    imagelib:finishPng( ThisPng ),

    ok.

%%%%%%%%
%% make_png_using_pool - using worker_pool for calculting fractal data
%%     ConfigMap - contains config info
%%               add parameters here to explain api
%%%%%%%%

make_png_using_pool(ConfigMap) ->
    %% get config and do some setup
    FractalAlg = maps:get(fractalAlg, ConfigMap),
    XList = compute_xlist(ConfigMap),
    YList = compute_ylist(ConfigMap),

    %% initialize the png
    ThisPng = imagelib:startPng( ConfigMap ),

    %% add all the rows, one at a time, to the image
    addRowsToPngUsingPool(FractalAlg, ThisPng, XList, YList, ConfigMap),

    %% finalize the png
    imagelib:finishPng( ThisPng ),

    ok.

%%%%%%%%
%% compute_rows/1 API
%%        ConfigMap    - config info
%%               add parameters here to explain api
%%        returns Rows
%%           (list of rows where each row is list of counts, 1 per pixel)
%%               add output format here
%%%%%%%%
compute_rows(ConfigMap) ->
    FractalAlg = maps:get(fractalAlg, ConfigMap),
    XList = compute_xlist(ConfigMap),
    YList = compute_ylist(ConfigMap),
    FractalData = [ compute_row( FractalAlg
                                           , {PixelY, ImgY}
                                           , XList
                                           , ConfigMap
                                           ) || {PixelY, ImgY} <- YList ],
    FractalData.

%%%%%%%%%%%%%%%%%%%%%%%%%
%% compute_row computes one row of fractal data
%%        inputs:
%%               FractalAlg - which algorithm eg julian, mandelbrot, ...
%%               {PixelY, ImgY} - the Y cordinate in pixels (an integer),
%%                                and number (imaginary value of Z or C)
%%               XList - the list of x coords to be computer over
%%               ConfigMap - configuration data, values needed for:
%%                           fractalAlg (must be same as FractalAlg)
%%%%%%%%%%%%%%%%%%%%%%%%%

compute_row(FractalAlg, {PixelY, ImgY}, XList, ConfigMap) ->
    %% given Y value for the row, and given Xlist (the x values in the row),
    %% compute the fractal values
    compute_row(FractalAlg, {PixelY, ImgY}, XList, [], ConfigMap).

compute_row( _FractalAlg
                       , {PixelY, ImgY}
                       , XList
                       , RowOfFractalData
                       , _ConfigMap
                       )
        when XList == [] ->
    %% XList empty so done, return RowOfFractalData
    %%   sorted by pixel value (ie 1 first)
    %%     and include Y value for use when pooling
    %%     (ie if they arrive out of order)
    { {PixelY, ImgY}, lists:sort(RowOfFractalData) };

compute_row( FractalAlg
                       , {PixelY, ImgY}
                       , XList
                       , RowOfFractalData
                       , ConfigMap
                       )
            when FractalAlg == julian ->
    %% otherwise pop off one x value, compute data,
    %%      insert answer in RowOfFractalData, and recurse

    %% pop off first x value, remainder is used for next iteration
    [ {PixelX, RealX} | NewXList ] = XList,
    %% compute fractal value for x, y
    IterCount = compute_points:compute_iteration_value(FractalAlg,
                                     %% since julian, C remains constant
                                     maps:get(cReal, ConfigMap),
                                     maps:get(cImaginary, ConfigMap),
                                     RealX,    %ZReal since julian
                                     ImgY,     %ZImg since julian
                                     0,        %IterCount = 0 to start
                                     maps:get(maxIterationThreshold, ConfigMap),
                                     maps:get(bailoutThreshold, ConfigMap)
                                     ),
    %% push new value onto RowOfFractalData list
    NewRowOfFractalData = [{PixelX, RealX, IterCount} | RowOfFractalData ],

    %% recurse
    compute_row( FractalAlg
                           , {PixelY, ImgY}
                           , NewXList
                           , NewRowOfFractalData
                           , ConfigMap
                           ).

%%%%%%%%%%%%%%%%%%%%%%%%%
%% ComputeXlist and ComputeYList set up x/y pixel/number data
%%%%%%%%%%%%%%%%%%%%%%%%%


%% ComputeXlist creates a row (list) of X, ZsubR, CsubR for a given fractal alg
%%    This list, along with the Y list, create the 'box'
%%        where each dot (x, y) fractal value is computed
compute_xlist(ConfigMap) ->
     %% get parameters
    Width      = maps:get(<<"width">>, ConfigMap),
    %% each pixel has a corresonding complex number defined
    %%         by corners of the box
    %%   note whether Y is dependent on Z or C is algorithm dependent
    %%     and taken into account in config file
    XRealRight      = maps:get(xRealRight, ConfigMap),
    XRealLeft       = maps:get(xRealLeft, ConfigMap),
    %% box is bounded on left by x > XRealLeft
    %%    and bounded on right by x < XRealRight
    %% box is width pixels wide and height pixels high

    %% step is floating range divided by number of pixels
    %% note -1 in next statement since one less intervals than points
    DeltaX = (XRealRight - XRealLeft) / (Width-1),

    %% iteratively compute the values in the list
    %%    starting at right (x=width, xreal=XRealRight) and decrementing down
    %% the returned list is returned by this function
    %%    for use in fractal computations
    compute_xlist([], Width, XRealRight, DeltaX).

compute_xlist(Row, PixelX, _RealX, _DeltaX)
         when PixelX =< 0 ->
    %% since PixelX decremented to end, the Row is now complete, so return Row
    Row;

compute_xlist(Row, PixelX, RealX, DeltaX) ->
    %% otherwise add another point and recurse
    NewRow = [ {PixelX, RealX} | Row ],
    compute_xlist(NewRow, PixelX-1, RealX-DeltaX, DeltaX).

%% ComputeYlist creates a column (list) of Y, ZsubI, CsubI for a
%%    given fracal alg
%%  This list, along with the X list, create the 'box' where
%%    each dot (x, y) fractal value is computed
compute_ylist(ConfigMap) ->
     %% get parameters
    Height          = maps:get(height, ConfigMap),
    %% each pixel has a corresonding complex number defined by corners
    %%    of the box
    %%  note whether Y is dependent on Z or C is algorithm dependent
    %%     and taken into account in config file
    YImaginaryLow   = maps:get(yImaginaryLow, ConfigMap),
    YImaginaryHigh  = maps:get(yImaginaryHigh, ConfigMap),
    %% box is bounded on top by y > YImaginaryHigh
    %%    and bounded on bottom by y > YImaginaryLow
    %% box is width pixels wide and height pixels high
    %% note -1 since one less intervals than points
    DeltaY = (YImaginaryHigh - YImaginaryLow) / (Height-1),

    %% iteratively compute the values in the list
    %%    starting at top (y=height, yImg=YImgTop) and decrementing down
    %% the returned list is returned by this function
    %%    for use in fractal computations
    compute_ylist([], Height, YImaginaryHigh, DeltaY).

compute_ylist(Column, PixelY, _ImgY, _DeltaY)
         when PixelY =< 0 ->
    %% since PixelY decremented to end, the Column is now complete,
    %%    so return Column
    Column;

compute_ylist(Column, PixelY, ImgY, DeltaY) ->
    %% otherwise add another point and recurse
    NewColumn = [ {PixelY, ImgY} | Column ],
    compute_ylist(NewColumn, PixelY-1, ImgY-DeltaY, DeltaY).

%% addRowsToPng computes fractal data one row at a time and adds to png
addRowsToPng(_FractalAlg, _ThisPng, _XList, YList, _ConfigMap)
        when YList == [] ->
    %% no rows left to compute so complete
    ok;

addRowsToPng(FractalAlg, ThisPng, XList, YList, ConfigMap) ->
    %% otherwise pop off a row, compute fractal data, add to png, recurse

    %% pop off a row
    [ {PixelY, ImgY} | NewYList ] = YList,

    %% compute row of fractal data
    { {_Yp, _Yi}, RowOfFractalData } =
            compute_row( FractalAlg
                                   , {PixelY, ImgY}
                                   , XList
                                   , ConfigMap
                                   ),
    ThisRowDataOnly = [ C || {_P, _I, C} <- RowOfFractalData ],

    %% add to png
    imagelib:addRow( ThisRowDataOnly, ThisPng ),

    %% recurse
    addRowsToPng(FractalAlg, ThisPng, XList, NewYList, ConfigMap).

%%%%%%%%%%
%% addRowsToPngUsingPool - using worker_pool for calculating
%%      fractal data to try to do more in parrallel
%%
%%      add params here
%%
%% this routine spawns a process to get the pool calculating
%%   all the rows (which will be messaged back)
%% then it spawns a process to receive all the rows,
%%   and add them to the image in the right order
%%
%%%%%%%%%%
addRowsToPngUsingPool(FractalAlg, ThisPng, XList, YList, ConfigMap) ->
    %% start worker pool application
    ok = application:start(worker_pool),

    %% start a worker pool for handling fractal computations
    %% use default worker and use default number (100) in pool
    {ok, _PoolPid} = wpool:start_sup_pool(fractal_pool, []),

    %% spawn a process to collect rows and write to png in correct order.
    %%     this process started first so pid is known
    %%     self pid is included so collector can message when finished
    %%     nextRowId is started at 1
    %%        (ie the next row to be written is the y=1 row)
    CollectorPid = spawn( ?MODULE
                        , collect_rows
                        , [ self()
                          , 1
                          , maps:get(height, ConfigMap)
                          , ThisPng
                          ]
                        ),

    %% spawn a process to use one worker per row to calculate fractal data
    %%     rowcollector pid is passed so workers respond correctly
    spawn( ?MODULE
         , create_fractal_workers
         , [CollectorPid, FractalAlg, XList, YList, ConfigMap]
         ),


    %% wait for message from collector that all rows are collected and written
    receive
           finished ->
               ok
    after maps:get(timeout, ConfigMap) ->
               lager:error("~ncollect_rows Timeout~n"),
               timeout
    end,

    ok.

%%%%%%%%%%
%% collect_rows collects rows of fractal data (messaged from workerpool)
%%   and writes to png
%%      collect_rows messgages it's calling program when finished
%% Parameters:
%%    Calling Pid - calling programs pid so collect_rows can message when done
%%    NextRowId   - messages may arrive out of order
%%                   so only write png when 'next' row arrives
%%    Height      - done when this many rows are written
%%
%%    ThisPng     - png to be written to
%%%%%%%%%%
collect_rows(CallingPid, NextRowId, Height, _ThisPng)
        when NextRowId > Height ->
    %% finished so let calling program know and end
    CallingPid ! finished;
collect_rows(CallingPid, NextRowId, Height, ThisPng) ->
    receive
        %% match when next row is to be written
        %%   (otherwise leave messages in queue until their turn)
        { {NextRowId, _ImgY}, RowOfFractalData } ->
           %% strip out just the data
           ThisRowDataOnly = [ C || {_P, _I, C} <- RowOfFractalData ],
           %% write data to png
           imagelib:addRow( ThisRowDataOnly, ThisPng ),

            %% go back for more messages
            collect_rows(CallingPid, NextRowId+1, Height, ThisPng)
    end.

%%%%%%%%%%
%% create_fractal_workers assigns fractal data creation to worker_pool
%% parameters:
%%            CollectorPid - process to message data to
%%            FractalAlg   - which alg to use to compute fractal data
%%                             (eg julian, mandelbrot, ...)
%%            XList        - list of x values
%%            YList        - list of y values
%%            ConfigMap    - all the config data
%%%%%%%%%%
create_fractal_workers(_CollectorPid, _FractalAlg, _XList, YList, _ConfigMap)
        %% done if nothing left in YList
        when YList == [] ->
    ok;
create_fractal_workers(CollectorPid, FractalAlg, XList, YList, ConfigMap) ->
    % for each rowi (value in YList), kick off a worker to compute data
    %%    cast used, output returned via a message to CollectorPid
    %%    pop a y value to define this row, then recurse thru rest of list
    [ {PixelY, ImgY} | NewYList ] = YList,
    wpool:cast( fractal_pool
              , { ?MODULE
                , fractal_worker
                , [CollectorPid, FractalAlg, XList, {PixelY, ImgY}, ConfigMap]
                }
              ),
    create_fractal_workers( CollectorPid
                          , FractalAlg
                          , XList
                          , NewYList
                          , ConfigMap
                          ).

%%%%%%%%%%
%% fractal_worker is run by worker_pool to calculate one row of data
%%          and message it to collector
%%   parameters:
%%              CollectorPid
%%              FractalAlg
%%              XList
%%              YP
%%              YR
%%              ConfigMap
%%%%%%%%%%
fractal_worker(CollectorPid, FractalAlg, XList, {PixelY, ImgY}, ConfigMap) ->
     %% calculate a row of fractal data
     Row = compute_row( FractalAlg
                                  , {PixelY, ImgY}
                                  , XList
                                  , ConfigMap
                                  ),
     %% message data to collector
     %%   (note the Y value, ie row id, is first field in Row)
     CollectorPid ! Row.
