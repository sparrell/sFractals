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
-export([ make_data/1
        , compute_row/8 % so spawned process can run
        , data2file/2  % write fractal data to file
        , data2svr/2  % write fractal data to svr
          ]).

%% expose functions for test
%% none?

make_data( #{ height := Height
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

  %% convert ets table data into rowdata for png
  RowData = ets_to_rowdata(FractalEts, Height, []),

  %% create image from rowdata
  make_png:make_png_from_data(RowData, ConfigMap).

%% convert ets table into fractal row data suitable for png
ets_to_rowdata(_FractalEts, 0, RowData) ->
  %% Height = zero  so done, return RowData
  lager:debug("clean up hardcoded timeout in wait_for_rows(2?)"),
  %%     later feature - put save to file here if desired
  lager:debug("later feature - put save to file here if desired"),
  %%     later feature - clean up ets here
  lager:debug("later feature - clean up ets here"),
  RowData;
ets_to_rowdata( FractalEts, RowNum, RowData )
               when is_integer(RowNum), RowNum > 0 ->
  %% pull the data for Row numbered RowNum
  [{RowNum, NewRow}] = ets:lookup(FractalEts, RowNum),

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
  after 5000 ->  %hardcoded 5s timeout for next row
      lager:error("wait_for_row timeout. Row = ~p",[Row]),
      erlang:error("wait_for_row timeout")
  end,

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

data2file( _Data, _ConfigMap) ->
  lager:debug("need to implement data2file using binary"),
  ok.

data2svr( _Data, _ConfigMap) ->
  lager:debug("need to implement data2svr"),
  ok.
