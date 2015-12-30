%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%%
%%%-------------------------------------------------------------------

-module(sf_controller).
-author("Duncan Sparrell").
%% control the flow of making a fractal
%% use epocxy to spawn actual computations

%% public API
-export([ make_data/1 % create a block of fractal data
        , make_data2/1
        , compute_row/8 % so spawned process can run
        , data2file/2  % write fractal data to file
        , data2svr/2  % write fractal data to svr
        , point/5     % worker
          ]).

%% expose functions for test
%% none?

make_data2( #{ height := Height
             , yImaginaryLow := YImaginaryLow
             , yImaginaryHigh := YImaginaryHigh
             , width := Width
             , xRealRight := XRealRight
             , xRealLeft := XRealLeft
             } = ConfigMap
          ) when is_integer(Height)
               , is_float(YImaginaryLow)
               , is_float(YImaginaryHigh)
               , is_integer(Width)
               , is_float(XRealRight)
             ->

  %% create ETS table to hold data
  FractalEts = ets:new(fractal_ets, [set, public, {write_concurrency, true}]),

  %% compute once
  DeltaY = (YImaginaryHigh - YImaginaryLow) / (Height-1),
  DeltaX = (XRealRight - XRealLeft) / (Width-1),

  %% compute each row initializing with height and width
  lager:debug("starting spawning rows"),
  compute_rows( FractalEts
              , Height
              , YImaginaryHigh
              , DeltaY
              , Width
              , XRealRight
              , DeltaX
              , ConfigMap
              ),
  lager:debug("finished spawning rows, starting wait"),
  wait_for_rows2( Height ), % wait for top row
  lager:debug("compute rows workers finished"),
  EtsInfo = ets:info(FractalEts),
  lager:debug("ets info: ~p", [EtsInfo]),
  lager:debug("clean up ets?"),

  %% convert ets table data into rowdata for png
  RowData = ets_to_rowdata(FractalEts, Height, []),

  %% create image from rowdata
  make_png:make_png_from_data(RowData, ConfigMap).

%% convert ets table into fractal row data suitable for png
ets_to_rowdata(_FractalEts, 0, RowData) ->
  %% Height = zero  so done, return RowData
  %%     later feature - put save to file here if desired
  lager:debug("later feature - put save to file here if desired"),
  %%     later feature - clean up ets here
  lager:debug("later feature - clean up ets here"),
  RowData;
ets_to_rowdata( FractalEts, RowNum, RowData )
               when is_integer(RowNum), RowNum > 0 ->
  %% pull the data for Row numbered RowNum
  NewRow = [0,0,0], %for now

  %% add to RowData
  NewRowData = [ NewRow | RowData ],

  %% recurse to next row
  NewRowNum = RowNum - 1,
  ets_to_rowdata( FractalEts, NewRowNum, NewRowData ).

wait_for_rows2(0) ->
  %% Row=0 so done
  ok;

wait_for_rows2(Row) when is_integer(Row), Row > 0 ->
  %% wait for any row to be written
  receive
    %% match when get a message that a row is done
    {did_a_row, Row} -> ok
  after 5000 ->  %hardcoded 5s timeout for next point
      lager:error("wait_for_row timeout. Row = ~p",[Row]),
      erlang:error("wait_for_row timeout")
  end,
  lager:debug("fix hardcoded row timeout"),

  lager:debug("got Row = ~p",[Row]),

  %% iterate for next row
  NewRow = Row - 1,
  wait_for_rows2( NewRow).

compute_rows( _FractalEts
            , 0
            , _YI
            , _DeltaY
            , _XP
            , _XR
            , _DeltaX
            , _ConfigMap
            ) ->
  %% if Y=0 then done going thru all rows
  ok;

compute_rows(FractalEts
            , YP
            , YI
            , DeltaY
            , XP
            , XR
            , DeltaX
            , ConfigMap
            ) when is_integer(YP)
                 , is_float(YI)
                 , is_float(DeltaY)
                 , is_integer(XP)
                 , is_float(XR)
                 , is_float(DeltaX)
                 , YP > 0
            ->
  cxy_ctl:execute_task( cfp
                      , ?MODULE
                      , compute_row
                      , [ self()
                        , FractalEts
                        , ConfigMap
                        , YP
                        , YI
                        , XP
                        , XR
                        , DeltaX
                        ]
                      ),

  %% now compute new values
  NewYP = YP -1,
  NewYI = YI - DeltaY,

  %% recurse to next row
  compute_rows(FractalEts, NewYP, NewYI, DeltaY, XP, XR, DeltaX, ConfigMap).

compute_row( CallingPid, FractalEts, ConfigMap, YP, YI, XP, XR, DeltaX ) ->
  %% this is spawned worker process
  %%  initialize row data to empty and kickoff recursion thru all the points in the row
  compute_row( CallingPid, FractalEts, ConfigMap, YP, YI, XP, XR, DeltaX, [] ).

compute_row( CallingPid, FractalEts, _ConfigMap, YP, _YI, XP, _XR, _DeltaX, RowData )
        when is_integer(YP), is_integer(XP), XP =< 0 ->
  %% done with row
  %%  save to ETS
  ets:insert(FractalEts, { YP, RowData}),
  %%  message did row
  CallingPid ! {did_a_row, YP};

compute_row( CallingPid
           , FractalEts
           , #{ fractalAlg := FractalAlg
              , cReal := CReal
              , cImaginary := CImaginary
              , maxIterationThreshold := MaxIterationThreshold
              , bailoutThreshold := BailoutThreshold
              } = ConfigMap
           , YP
           , YI
           , XP
           , XR
           , DeltaX
           , RowData
           )
        when is_integer(YP)
           , is_float(YI)
           , is_integer(XP)
           , is_float(XR)
           , is_float(DeltaX)
           , is_float(CReal)
           , is_float(CImaginary)
           , is_integer(MaxIterationThreshold)
           , is_float(BailoutThreshold)
        ->
  %% compute iteration count for point
  Iter = compute_points:compute_iteration_value( FractalAlg
                                               , CReal
                                               , CImaginary
                                               , XR
                                               , YI
                                               , 0 % init iteration to zero
                                               , MaxIterationThreshold
                                               , BailoutThreshold ),

  NewRowData = [ Iter | RowData ],

  %% iterate for next point
  NewXP = XP - 1,
  NewXR = XR - DeltaX,
  compute_row( CallingPid
             , FractalEts
             , ConfigMap
             , YP
             , YI
             , NewXP
             , NewXR
             , DeltaX
             , NewRowData
             ).

make_data( ConfigMap ) ->

  %% compute the box (ie list of x's and y's)
  XList = databox:compute_xlist(ConfigMap),
  YList = databox:compute_ylist(ConfigMap),

  %% create ETS table to hold data
  FractalEts = ets:new(fractal_ets, [set, public, {write_concurrency, true}]),

  %% spawn processes to populate ETS with fractal data
  %%    initialize epocxy concurrency initialized in supervisor
  %%    cfp = concurrent fractal processing

  %% spawn the workers for each point
  lager:debug("make_data spawning workers"),
  make_rows( cfp, XList, YList, ConfigMap, FractalEts ),

  %% wait for all rows of data to finish
  lager:debug("make_data waiting for workers to finish"),
  wait_for_rows( XList, YList),
  lager:debug("make_data workers finished"),
  EtsInfo = ets:info(FractalEts),
  lager:debug("ets info: ~p", [EtsInfo]),

  %% transform data

  %% return data
  ok.

wait_for_rows( _XList, []) ->
  %% if YList empty then you are done
  ok;
wait_for_rows( XList, [Y | RestY] ) ->
  %% wait for next row
  wait_for_row( XList, Y ),
  %% recurse thru rest of rows
  wait_for_rows( XList, RestY ).

wait_for_row( [], _Y) ->
  %% if XList empty then you are done with this row
  ok;
wait_for_row( [ X | RestX ], Y ) ->
  %% wait for next point in row
  receive
    %% match when get a message that a point is done
    %% note it might not be this XY
    %%   doesn't matter - just counting got right number back
    did_a_point ->
      ok
  after 5000 ->  %hardcoded 5s timeout for next point
      lager:error("wait_for_row timeout. XY = ~p, ~p",[X,Y]),
      erlang:error("wait_for_row timeout")
  end,

  %% recurse thru rest of points in row
  wait_for_row( RestX, Y).


make_rows( _Pool, _XList, [], _ConfigMap, _FractalEts ) ->
  %% done when YList empty
  ok;
make_rows( Pool, XList, [FirstY | RestY], ConfigMap, FractalEts ) ->
  %% make another row of data
  make_a_row( Pool, XList, FirstY, ConfigMap, FractalEts),
  %% recurse thru rest of rows
  make_rows( Pool, XList, RestY, ConfigMap, FractalEts ).

make_a_row( _Pool, [], _Y, _ConfigMap, _FractalEts) ->
  %% done when XList empty
  ok;
make_a_row( Pool, [X | RestX], Y, ConfigMap, FractalEts) ->
  %% make a point
  cxy_ctl:execute_task(cfp, ?MODULE, point, [ X, Y, ConfigMap, FractalEts, self()]),
  %% recurse thru rest of row
  make_a_row( Pool, RestX, Y, ConfigMap, FractalEts).

data2file( _Data, _ConfigMap) ->
  lager:debug("need to implement data2file using binary"),
  ok.

data2svr( _Data, _ConfigMap) ->
  lager:debug("need to implement data2svr"),
  ok.

%%%%%%%
%% point is the worker to compute one data point
%%     inputs: {PixelX, RealX}, {PixelY, ImgY}, ConfigMap
%%              PixelX = integer x pixel co-ordinate
%%              RealX  = real floating point number corresponding to X
%%              PixelY = integer y pixel co-ordinate
%%              ImgY   = imaginary floating point number corresponding to Y
%%              ConfigMap = configuration data
point( {PixelX, RealX}
     , {PixelY, ImgY}
     , #{ fractalAlg := julian
        , cReal := CReal
        , cImaginary := CImaginary
        , maxIterationThreshold := MaxIterationThreshold
        , bailoutThreshold := BailoutThreshold
        }
     , FractalEts
     , ControllerPid
     )
    when is_integer(PixelX)
       , is_integer(PixelY)
       , is_float(RealX)
       , is_float(ImgY)
       , is_float(CReal)
       , is_float(CImaginary)
       , is_integer(MaxIterationThreshold)
       , is_float(BailoutThreshold)
    ->
  %% calculate iterations
  Iter = compute_points:compute_iteration_value( julian
                                               , CReal
                                               , CImaginary
                                               , RealX
                                               , ImgY
                                               , 0
                                               , MaxIterationThreshold
                                               , BailoutThreshold ),

  %% store in ets table at x,y
  ets:insert(FractalEts, {{PixelX ,PixelY}, Iter}),

  %% message that data stored
  ControllerPid ! did_a_point,

  %% done
  ok.
