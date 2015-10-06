%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-module(simpleFractal).
-author("Duncan Sparrell").
%% try to make simple fractal data and then turn into color data and then make image

%% public API
-export([ createPointData/3, 
          makeFractalPng/1,
          computeFractalData/1,           % create a block of fractal data
          computeFractalDataIntoFile/1,   % create a block of fractal data and write to file
          makePngFromData/2,              % create a Png from block of fractal data
          makePngFromDataFile/1           % create a Png from block of fractal data in a file
          ]).

%% expose functions for test
-export([ newImaginaryC/1,newRealC/1,newImaginaryZ/1,newRealZ/1,
          computeIterationValue/8, addOnePoint/7, makePoints/8,
          computeFractalData/11,computeFractalDataIntoFile/11 ]).
 
%% public API for making fractal
makePngFromDataFile(ConfigMap) ->       % create a Png from file of fractal data
    %% get the data
    {ok, Rows} = file:consult( maps:get( dataFileName,ConfigMap) ),

    %% make the png
    makePngFromData(Rows,ConfigMap).


makePngFromData(Rows,ConfigMap) ->       % create a Png from block of fractal data
    %% initialize the png
    ThisPng = imagelib:startPng( ConfigMap ),

    %% add all the rows, one at a time, to the image
    addRows(ThisPng,Rows),

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

%% explain ConfigMap params needed here
%% note palette must have values for all counts (eg set max iter according to size of palette or vice versa)
%%
makeFractalPng(ConfigMap) ->

    %% create height rows of width columns of pixels
    Width           = maps:get(width,ConfigMap),
    Height          = maps:get(height,ConfigMap),
    %% each pixel has a corresonding complex number defined by corners of the box
    XRealRight      = maps:get(xRealRight,ConfigMap),
    XRealLeft       = maps:get(xRealLeft,ConfigMap),
    YImaginaryLow   = maps:get(yImaginaryLow,ConfigMap),
    YImaginaryHigh  = maps:get(yImaginaryHigh,ConfigMap),
    %% box is bounded on left by x > XRealLeft and bounded on right by x < XRealRight
    %% box is bounded on top by y > YImaginaryHigh and bounded on bottom by y > YImaginaryLow
    %% box is width pixels wide and height pixels high

    %% step is floating range divided by number of pixels
    DeltaX = (XRealRight - XRealLeft) / Width,
    DeltaY = (YImaginaryHigh - YImaginaryLow) / Height,

    %% initialize the png
    ThisPng = imagelib:startPng( ConfigMap ),

    %% recurse thru the rows
    ok = computeRowAtATime( [],    % start with empty row data
                        ThisPng,                          % png object
                        Width, XRealRight, DeltaX, Width, % start at right hand side (point 1 at head)
                        1, YImaginaryLow, DeltaY,Height,   %% start on first row
                        ConfigMap),

    %% finalize the png
    imagelib:finishPng( ThisPng ),

    ok.

%%%%%%%%
%% computeFractalData/1 API
%%        ConfigMap    - config info
%%        returns Rows (list of rows where each row is list of counts, 1 per pixel)
%%%%%%%%
computeFractalData(ConfigMap) ->
    %% get config needed
    %% create height rows of width columns of pixels
    Width           = maps:get(width,ConfigMap),
    Height          = maps:get(height,ConfigMap),
    %% each pixel has a corresonding complex number defined by corners of the box
    XRealRight      = maps:get(xRealRight,ConfigMap),
    XRealLeft       = maps:get(xRealLeft,ConfigMap),
    YImaginaryLow   = maps:get(yImaginaryLow,ConfigMap),
    YImaginaryHigh  = maps:get(yImaginaryHigh,ConfigMap),
    %% box is bounded on left by x > XRealLeft and bounded on right by x < XRealRight
    %% box is bounded on top by y > YImaginaryHigh and bounded on bottom by y > YImaginaryLow
    %% box is width pixels wide and height pixels high

    %% step is floating range divided by number of pixels
    DeltaX = (XRealRight - XRealLeft) / Width,
    DeltaY = (YImaginaryHigh - YImaginaryLow) / Height,

    %% call computeFractalData/11
    % return Rows from computeFractalData/11
    computeFractalData( [],             % Rows starts empty
                        [],             % ThisRow starts empty
                        Width,          % XPix starts width and decrements to 1
                        XRealRight,     %% XR starts at right and decrements to by deltaX
                        DeltaX,         
                        Width,          % width
                        Height,         % YPix starts height and decrements to 1
                        YImaginaryHigh, % YI starts at top and decrements by delta y
                        DeltaY,         
                        Height,         % height
                        ConfigMap).    % map of parameters



%%%%%%%%
%% computeFractalData/11 API
%%        Rows         - a list of the rows(lines) of data, starts empty and builds until height reached, built top down
%%        ThisRow      - a list of the points(count value) in a row/line, starts empty and builds R->L until width reached
%%        XPix         - the integer X value of the pixel 
%%        XR           - the real component of the floating point number for computing fractal for this XPix
%%        DeltaX       - for each pixel, XR increases by this amount
%%        Width        - width of fractal in pixels
%%        YPix         - the integer Y value of the pixel
%%        YI           - the imaginary component of the floating point number for computing fractal for this YPix
%%        DeltaY       - for each pixel, YI increases by this amount
%%        Height       - height(intger) of image = number of rows
%%        ConfigMap    - config info
%%%%%%%%
% clause when height is reached, return the rows of data
computeFractalData( Rows, _ThisRow,
               _XPix, _XR, _DeltaX, _Width,  
               YPix, _YI, _DeltaY, _Height,  % only height matters
               _ConfigMap)
        when YPix =< 0 ->
               
    %% pixels all made already so done
    Rows;

% clause when row is complete  but height not reached - process row and recurse
computeFractalData( Rows, ThisRow,        % row data computed so far
               XPix, _XR, DeltaX, Width,  % info for points in a row
               YPix, YI, DeltaY, Height,  % info for rows
               ConfigMap)
        when XPix =< 0, YPix >= 0 ->

    % add row
    NewRows = [ ThisRow | Rows ],

    % reset to begining of next row 
    NewRowData = [],                                 % reset data for row to empty
    NewXPix    = Width,                              % reset to end of line
    NewXR      = maps:get(xRealRight,ConfigMap),     % reset to end of line
    NewYPix    = YPix - 1,                           % increment row
    NewYI      = YI - DeltaY,                          % increment row
    computeFractalData( NewRows, NewRowData,
                   NewXPix, NewXR, DeltaX, Width,
                   NewYPix, NewYI, DeltaY,Height,
                   ConfigMap);

computeFractalData( Rows, RowData,       % row data computed so far
               XPix, XR, DeltaX, Width,   % info for points in a row
               YPix, YI, DeltaY, Height,  % info for rows
               ConfigMap)
        when XPix > 0, YPix > 0 ->

    %% get iteration count for this point
    NewPoint = computeIterationValue( maps:get(fractalAlg,ConfigMap),
                                      maps:get(cReal,ConfigMap),
                                      maps:get(cImaginary,ConfigMap),
                                      XR,
                                      YI,
                                      0,          %iteration count starts at zero
                                      maps:get(maxIterationThreshold,ConfigMap),
                                      maps:get(bailoutThreshold,ConfigMap)
                                      ),
                           
    NewRowData = [ NewPoint | RowData ],
    NewXPix    = XPix - 1,                           % decrement moving left building row
    NewXR      = XR - DeltaX,                        % decrease XR to the left
    computeFractalData( Rows, NewRowData,
                   NewXPix, NewXR, DeltaX, Width,
                   YPix, YI, DeltaY,Height,
                   ConfigMap).


%%%%%%%%
%% computeFractalDataIntoFile/1 API
%%        ConfigMap    - config info
%%        creates file with list of rows (where each row is list of counts, 1 per pixel)
%%                     suitable to be read with file:consult
%%%%%%%%
computeFractalDataIntoFile(ConfigMap) ->
    %% get config needed
    DataFileName  = maps:get(dataFileName,ConfigMap),
    %% create height rows of width columns of pixels
    Width           = maps:get(width,ConfigMap),
    Height          = maps:get(height,ConfigMap),
    %% each pixel has a corresonding complex number defined by corners of the box
    XRealRight      = maps:get(xRealRight,ConfigMap),
    XRealLeft       = maps:get(xRealLeft,ConfigMap),
    YImaginaryLow   = maps:get(yImaginaryLow,ConfigMap),
    YImaginaryHigh  = maps:get(yImaginaryHigh,ConfigMap),
    %% box is bounded on left by x > XRealLeft and bounded on right by x < XRealRight
    %% box is bounded on top by y > YImaginaryHigh and bounded on bottom by y > YImaginaryLow
    %% box is width pixels wide and height pixels high

    %% step is floating range divided by number of pixels
    DeltaX = (XRealRight - XRealLeft) / Width,
    DeltaY = (YImaginaryHigh - YImaginaryLow) / Height,

    %% open file
    {ok,DataFile} = file:open(DataFileName, [write]),

    %% call computeFractalDataIntoFile/11
    %% writing rows one at a time
    computeFractalDataIntoFile( DataFile,       % file where data is written
                        [],             % ThisRow starts empty
                        Width,          % XPix starts width and decrements to 1
                        XRealRight,     %% XR starts at right and decrements to by deltaX
                        DeltaX,         
                        Width,          % width
                        Height,         % YPix starts height and decrements to 1
                        YImaginaryHigh, % YI starts at top and decrements by delta y
                        DeltaY,         
                        Height,         % height
                        ConfigMap).    % map of parameters



%%%%%%%%
%% computeFractalDataIntoFile/11 API
%%        DataFile     - file already opened for writing
%%        ThisRow      - a list of the points(count value) in a row/line, starts empty and builds R->L until width reached
%%        XPix         - the integer X value of the pixel 
%%        XR           - the real component of the floating point number for computing fractal for this XPix
%%        DeltaX       - for each pixel, XR increases by this amount
%%        Width        - width of fractal in pixels
%%        YPix         - the integer Y value of the pixel
%%        YI           - the imaginary component of the floating point number for computing fractal for this YPix
%%        DeltaY       - for each pixel, YI increases by this amount
%%        Height       - height(intger) of image = number of rows
%%        ConfigMap    - config info
%%%%%%%%
% clause when height is reached, return the rows of data
computeFractalDataIntoFile( DataFile, _ThisRow,
               _XPix, _XR, _DeltaX, _Width,  
               YPix, _YI, _DeltaY, _Height,  % only height matters
               _ConfigMap)
        when YPix =< 0 ->
               
    %% pixels all made already, therefore work is finished so close file and end
    file:close(DataFile);

% clause when row is complete  but height not reached - process row and recurse
computeFractalDataIntoFile( DataFile, ThisRow,        % row data computed so far
               XPix, _XR, DeltaX, Width,  % info for points in a row
               YPix, YI, DeltaY, Height,  % info for rows
               ConfigMap)
        when XPix =< 0, YPix >= 0 ->

    % add row to file
    io:format(DataFile,"~p.~n",[ThisRow]),

    % reset to begining of next row 
    NewRowData = [],                                 % reset data for row to empty
    NewXPix    = Width,                              % reset to end of line
    NewXR      = maps:get(xRealRight,ConfigMap),     % reset to end of line
    NewYPix    = YPix - 1,                           % increment row
    NewYI      = YI - DeltaY,                          % increment row
    computeFractalDataIntoFile( DataFile, NewRowData,
                   NewXPix, NewXR, DeltaX, Width,
                   NewYPix, NewYI, DeltaY,Height,
                   ConfigMap);

computeFractalDataIntoFile( DataFile, RowData,       % row data computed so far
               XPix, XR, DeltaX, Width,   % info for points in a row
               YPix, YI, DeltaY, Height,  % info for rows
               ConfigMap)
        when XPix > 0, YPix > 0 ->

    %% get iteration count for this point
    NewPoint = computeIterationValue( maps:get(fractalAlg,ConfigMap),
                                      maps:get(cReal,ConfigMap),
                                      maps:get(cImaginary,ConfigMap),
                                      XR,
                                      YI,
                                      0,          %iteration count starts at zero
                                      maps:get(maxIterationThreshold,ConfigMap),
                                      maps:get(bailoutThreshold,ConfigMap)
                                      ),
                           
    NewRowData = [ NewPoint | RowData ],
    NewXPix    = XPix - 1,                           % decrement moving left building row
    NewXR      = XR - DeltaX,                        % decrease XR to the left
    computeFractalDataIntoFile( DataFile, NewRowData,
                   NewXPix, NewXR, DeltaX, Width,
                   YPix, YI, DeltaY,Height,
                   ConfigMap).




%%%%%%%%%%%%%%%%%%%%%
% clause when height is reached 
computeRowAtATime( _RowData,
               #{size := {_, Height}},       % when PNG is height high
               _XPix, _XR, _DeltaX, _Width,  
               YPix, _YI, _DeltaY, Height,  % only height matters
               _ConfigMap)
        when YPix > Height ->
               
    %% pixels all made already so done
    ok;

% clause when row is complete (xpix = -1) but height not reached - process row and recurse
computeRowAtATime( RowData,                   % row data computed so far
               ThisPng,                   % png object
               XPix, _XR, DeltaX, Width,   % info for points in a row
               YPix, YI, DeltaY, Height,  % info for rows
               ConfigMap)
        when XPix =< 0, YPix =< Height ->

    %% add row to png
    imagelib:addRow( RowData, ThisPng ),

    %% recurse
    NewRowData = [],                                 % reset data for row to empty
    NewXPix    = Width,                              % reset to end of line
    NewXR      = maps:get(xRealRight,ConfigMap),     % reset to end of line
    NewYPix    = YPix + 1,                           % increment row
    NewYI      = YI+DeltaY,                          % increment row
    computeRowAtATime( NewRowData,
                   ThisPng,                          % png object
                   NewXPix, NewXR, DeltaX, Width,
                   NewYPix, NewYI, DeltaY,Height,
                   ConfigMap);

% clause when row is incomplete - add another point and recurse
computeRowAtATime( RowData,                   % row data computed so far
               ThisPng,                   % png object
               XPix, XR, DeltaX, Width,   % info for points in a row
               YPix, YI, DeltaY, Height,  % info for rows
               ConfigMap)
        when XPix >= 0, YPix =< Height ->

    %% get iteration count for this point
    NewPoint = computeIterationValue( maps:get(fractalAlg,ConfigMap),
                                      maps:get(cReal,ConfigMap),
                                      maps:get(cImaginary,ConfigMap),
                                      XR,
                                      YI,
                                      0,          %iteration count starts at zero
                                      maps:get(maxIterationThreshold,ConfigMap),
                                      maps:get(bailoutThreshold,ConfigMap)
                                      ),
                           
    NewRowData = [ NewPoint | RowData ],
    NewXPix    = XPix - 1,                           % decrement moving left building row
    NewXR      = XR - DeltaX,                        % decrease XR to the left
    computeRowAtATime( NewRowData,
                   ThisPng,                          % png object
                   NewXPix, NewXR, DeltaX, Width,
                   YPix, YI, DeltaY,Height,
                   ConfigMap).



createPointData( {XRealLeft,DeltaX,Width}, {YImaginaryLow,DeltaY,Height}, ConfigMap)
        when DeltaX > 0, DeltaY > 0 ->

    CurrentPixelY = 1,   % start at bottom and work up (so head of list will be top row)
    IterCounts = [],     % start with empty data box
    %% makeAllRows returns InterCount, which is then returned by createPointData 
    makeAllRows( {XRealLeft,DeltaX,Width}, 
                 {YImaginaryLow,DeltaY,Height,CurrentPixelY},
                 IterCounts, 
                 ConfigMap).

%% make one row at a time, starting at bottom and working up 
%%  note builds list from tail so this yeilds highest Y nearest front 
%%  which is useful in case later we might to build file on fly that way 'from top'

% function head for done
%new
makeAllRows( {_XRealLeft,_DeltaX,_Width}, 
                 {_CurrentImaginaryY,_DeltaY,Height,CurrentPixelY},
                 IterCounts, 
                 _ConfigMap)
        when CurrentPixelY > Height ->  
    IterCounts;   %reached bottom so return the iteration count data

% function head for not-done so recurse
makeAllRows( {XRealLeft,DeltaX,Width}, 
                 {CurrentImaginaryY,DeltaY,Height,CurrentPixelY},
                 IterCounts, 
                 ConfigMap) ->

    % reached here so need to make another row
	% IterCounts is updated with the new row calculated by makePoints and is the returned value
    NewIterCounts = makePoints(Width,
                               1,                   % set CurrentPixelX to 1
                               CurrentPixelY,
                               XRealLeft, 
                               CurrentImaginaryY,
                               DeltaX,
                               IterCounts,
                               ConfigMap),
    makeAllRows( {XRealLeft,DeltaX,Width}, 
                 {CurrentImaginaryY+DeltaY,DeltaY,Height,CurrentPixelY+1},   % increment float & pixel Y's
                 NewIterCounts,                                   % new counts
                 ConfigMap).
    
% function head for done-with-row
makePoints(Width,
           CurrentPixelX,
           _CurrentPixelY,
           _CurrentRealX,
           _CurrentImaginaryY,
           _DeltaX,
           IterCounts,
           _ConfigMap)
        when CurrentPixelX > Width ->  
    IterCounts;   %reached end of line so return the iteration count data

% function head for not-done so recurse
makePoints(Width,
           CurrentPixelX,
           CurrentPixelY,
           CurrentRealX,
           CurrentImaginaryY,
           DeltaX,
           IterCounts,
           ConfigMap) ->   
    %reached here so need to add another pixel
    % CurrentPixelX starts at 1 increases by 1 each recurse
    % CurrentRealX starts at XRealLeft and increases by DeltaX each recurse
    % IterCounts has point {X,Y,Iter} added by function addOnePoint
    FractalAlg= maps:get(fractalAlg,ConfigMap),

    %% note addOnePoint returns the new value of IterCounts into the recursive call
    NewIterCounts = addOnePoint(CurrentPixelX,
                                CurrentPixelY,
                                CurrentRealX,
                                CurrentImaginaryY,
                                FractalAlg,
                                IterCounts,
                                ConfigMap),

    makePoints(Width,
               CurrentPixelX+1,
               CurrentPixelY,
               CurrentRealX+DeltaX,
               CurrentImaginaryY,
               DeltaX, 
               NewIterCounts, 
               ConfigMap ).

%% Compute fractal (bounded or unbounded)
    %% FractalAlg is which algorithm (only one at moment)
% this function clause for creating julian fractals. eventually will generalize
addOnePoint(CurrentPixelX,
            CurrentPixelY,
            Xreal,
            Yimaginary,
            FractalAlg,
            IterCounts,
            ConfigMap) 
    when FractalAlg == julian ->

    CReal = maps:get(cReal,ConfigMap),
    CImaginary = maps:get(cImaginary,ConfigMap),
    MaxIterationThreshold = maps:get(maxIterationThreshold,ConfigMap),
    BailoutThreshold = maps:get(bailoutThreshold,ConfigMap),
    %% return IterCounts with new point added. Note 3rd item in tuple is Count, and 4th item in compute call is initial iteration
    Count = computeIterationValue(FractalAlg,
                                  CReal,
                                  CImaginary,
                                  Xreal,
                                  Yimaginary,
                                  0,                     % initial iteration count
                                  MaxIterationThreshold, 
                                  BailoutThreshold ),

    [ {CurrentPixelX, CurrentPixelY, Count } | IterCounts ].

%% computeIterationValue computes fractal value and returns iteration count
%% function clause for exceeding iteration count
computeIterationValue(_FractalAlg,
                      _CReal, 
                      _CImaginary, 
                      _ZReal,
                      _ZImaginary, 
                      IterCount,                     %for this clause only need IterCount and Max
                      MaxIterationThreshold, 
                      _BailoutThreshold )
        when IterCount >= MaxIterationThreshold ->   % reached iteration limit so return count=limit
    MaxIterationThreshold;

%% function clause for exceeding bound
computeIterationValue(_FractalALg,
                      _CReal,
                      _CImaginary, 
                      ZReal,
                      ZImaginary, 
                      IterCount, 
                      _MaxIterationThreshold, 
                      BailoutThreshold ) 
        when ((ZReal*ZReal)+(ZImaginary*ZImaginary)) > BailoutThreshold -> 
    %bailout exceeded so return iterCount
    IterCount;

%% function clause for recursing further
computeIterationValue(FractalAlg,
                      CReal,
                      CImaginary, 
                      ZReal,
                      ZImaginary, 
                      IterCount, 
                      MaxIterationThreshold, 
                      BailoutThreshold ) ->

    % compute new Z and C based on fractal algorithm used
    ZCParams      = {FractalAlg,CReal,CImaginary, ZReal,ZImaginary},
    NewZReal      = newRealZ(ZCParams),
    NewZImaginary = newImaginaryZ(ZCParams),
    NewCReal      = newRealC(ZCParams),
    NewCImaginary = newImaginaryC(ZCParams),

    computeIterationValue(FractalAlg,
                          NewCReal,
                          NewCImaginary, 
                          NewZReal, 
                          NewZImaginary, 
                          IterCount+1, 
                          MaxIterationThreshold, 
                          BailoutThreshold ).

% function for creating new real value for Julian algorithm
newRealZ({FractalAlg,CReal,_CImaginary, ZReal,ZImaginary}) 
        when FractalAlg == julian ->
    (ZReal*ZReal) - (ZImaginary*ZImaginary) + CReal.

% function for creating new imaginary value for Julian algorithm
newImaginaryZ({FractalAlg,_CReal,CImaginary, ZReal,ZImaginary}) 
        when FractalAlg == julian ->
    (2 * ZReal * ZImaginary) + CImaginary.

% function for creating new real value for Julian Algorithm (ie remains unchanged)
newRealC({FractalAlg,CReal,_CImaginary, _ZReal,_ZImaginary}) 
        when FractalAlg == julian ->
    CReal.

% function for creating new imaginary value for Julian Algorithm (ie remains unchanged)
newImaginaryC({FractalAlg,_CReal,CImaginary, _ZReal,_ZImaginary}) 
        when FractalAlg == julian ->
    CImaginary.

