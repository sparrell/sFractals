%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%%
%%%-------------------------------------------------------------------

-module(simpleFractal).
-author("Duncan Sparrell").
%% try to make simple fractal data
%%   and then turn into color data and then make image

%% public API
-export([
          computeFractalData/1,           % create a block of fractal data
          computeFractalDataIntoFile/1,   % create a file of fractal data
          computeFractalDataIntoFile2/1,   % create a file of fractal data
          makePngFromData/2,              % create a Png from data
          makePngFromDataFile/1           % create a Png from file
          ]).

%% expose functions for test
-export([ computeFractalData/11 ]).

%% public API for making fractal
makePngFromDataFile(ConfigMap) ->       % create a Png from file of fractal data
    %% get the data
    {ok, Rows} = file:consult( maps:get( dataFileName, ConfigMap) ),

    %% make the png
    makePngFromData(Rows, ConfigMap).


makePngFromData(Rows, ConfigMap) ->   % create a Png from block of fractal data
    %% initialize the png
    ThisPng = imagelib:startPng( ConfigMap ),

    %% add all the rows, one at a time, to the image
    addRows(ThisPng, Rows),

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

%%%%%%%%
%% computeFractalData/1 API
%%        ConfigMap    - config info
%%        returns Rows
%%           (list of rows where each row is list of counts, 1 per pixel)
%%%%%%%%
computeFractalData(ConfigMap) ->
    %% get config needed
    %% create height rows of width columns of pixels
    Width           = maps:get(width, ConfigMap),
    Height          = maps:get(height, ConfigMap),
    %% each pixel has a corresonding complex number defined by corners
    %%     of the box
    XRealRight      = maps:get(xRealRight, ConfigMap),
    XRealLeft       = maps:get(xRealLeft, ConfigMap),
    YImaginaryLow   = maps:get(yImaginaryLow, ConfigMap),
    YImaginaryHigh  = maps:get(yImaginaryHigh, ConfigMap),
    %% box is bounded on left by x > XRealLeft
    %%    and bounded on right by x < XRealRight
    %% box is bounded on top by y > YImaginaryHigh
    %%    and bounded on bottom by y > YImaginaryLow
    %% box is width pixels wide and height pixels high

    %% step is floating range divided by number of pixels
    DeltaX = (XRealRight - XRealLeft) / Width,
    DeltaY = (YImaginaryHigh - YImaginaryLow) / Height,

    %% call computeFractalData/11
    % return Rows from computeFractalData/11
    computeFractalData( [],             % Rows starts empty
                        [],             % ThisRow starts empty
                        Width,          % XPix starts width and decrements to 1
                        XRealRight,     %% XR starts at right
                                        %%   and decrements to by deltaX
                        DeltaX,
                        Width,          % width
                        Height,         % YPix starts height and decrements to 1
                        YImaginaryHigh, % YI starts at top
                                        %%   and decrements by delta y
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
    NewXR      = maps:get(xRealRight, ConfigMap),     % reset to end of line
    NewYPix    = YPix - 1,                           % increment row
    NewYI      = YI - DeltaY,                          % increment row
    computeFractalData( NewRows, NewRowData,
                   NewXPix, NewXR, DeltaX, Width,
                   NewYPix, NewYI, DeltaY, Height,
                   ConfigMap);

computeFractalData( Rows, RowData,       % row data computed so far
               XPix, XR, DeltaX, Width,   % info for points in a row
               YPix, YI, DeltaY, Height,  % info for rows
               ConfigMap)
        when XPix > 0, YPix > 0 ->

    %% get iteration count for this point
    NewPoint = compute_points:compute_iteration_value( maps:get(fractalAlg, ConfigMap),
                                      maps:get(cReal, ConfigMap),
                                      maps:get(cImaginary, ConfigMap),
                                      XR,
                                      YI,
                                      0,          %iteration count starts at zero
                                      maps:get(maxIterationThreshold, ConfigMap),
                                      maps:get(bailoutThreshold, ConfigMap)
                                      ),

    NewRowData = [ NewPoint | RowData ],
    NewXPix    = XPix - 1,                           % decrement moving left building row
    NewXR      = XR - DeltaX,                        % decrease XR to the left
    computeFractalData( Rows, NewRowData,
                   NewXPix, NewXR, DeltaX, Width,
                   YPix, YI, DeltaY, Height,
                   ConfigMap).


%%%%%%%%
%% computeFractalDataIntoFile/1 API
%%        ConfigMap    - config info
%%        creates file with list of rows
%%              (where each row is list of counts, 1 per pixel)
%%                     suitable to be read with file:consult
%%%%%%%%
computeFractalDataIntoFile(ConfigMap) ->
    %% get config needed
    DataFileName  = maps:get(dataFileName, ConfigMap),
    %% create height rows of width columns of pixels
    Width           = maps:get(width, ConfigMap),
    Height          = maps:get(height, ConfigMap),
    %% each pixel has a corresonding complex number defined by corners
    %%   of the box
    XRealRight      = maps:get(xRealRight, ConfigMap),
    XRealLeft       = maps:get(xRealLeft, ConfigMap),
    YImaginaryLow   = maps:get(yImaginaryLow, ConfigMap),
    YImaginaryHigh  = maps:get(yImaginaryHigh, ConfigMap),
    %% box is bounded on left by x > XRealLeft and
    %%     bounded on right by x < XRealRight
    %% box is bounded on top by y > YImaginaryHigh
    %%     and bounded on bottom by y > YImaginaryLow
    %% box is width pixels wide and height pixels high

    %% step is floating range divided by number of pixels
    DeltaX = (XRealRight - XRealLeft) / Width,
    DeltaY = (YImaginaryHigh - YImaginaryLow) / Height,

    %% open file
    {ok, DataFile} = file:open(DataFileName, [write]),

    %% call computeFractalDataIntoFile/11
    %% writing rows one at a time
    computeFractalDataIntoFile( DataFile,       % file where data is written
                        [],             % ThisRow starts empty
                        Width,          % XPix starts width and decrements to 1
                        XRealRight,     % XR starts at righti
                                        %  and decrements to by deltaX
                        DeltaX,
                        Width,          % width
                        Height,         % YPix starts height and decrements to 1
                        YImaginaryHigh, % YI starts at top
                                        %  and decrements by delta y
                        DeltaY,
                        Height,         % height
                        ConfigMap).    % map of parameters



%%%%%%%%
%% computeFractalDataIntoFile/11 API
%%        DataFile     - file already opened for writing
%%        ThisRow      - a list of the points(count value) in a row/line,
%%                       starts empty and builds R->L until width reached
%%        XPix         - the integer X value of the pixel
%%        XR           - the real component of the floating point number
%%                       for computing fractal for this XPix
%%        DeltaX       - for each pixel, XR increases by this amount
%%        Width        - width of fractal in pixels
%%        YPix         - the integer Y value of the pixel
%%        YI           - the imaginary component of the floating point number
%%                       for computing fractal for this YPix
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
    io:format(DataFile,"~w.~n",[ThisRow]),

    % reset to begining of next row
    NewRowData = [],                             % reset data for row to empty
    NewXPix    = Width,                          % reset to end of line
    NewXR      = maps:get(xRealRight, ConfigMap),% reset to end of line
    NewYPix    = YPix - 1,                       % increment row
    NewYI      = YI - DeltaY,                    % increment row
    computeFractalDataIntoFile( DataFile, NewRowData,
                   NewXPix, NewXR, DeltaX, Width,
                   NewYPix, NewYI, DeltaY, Height,
                   ConfigMap);

computeFractalDataIntoFile( DataFile, RowData,       % row data computed so far
               XPix, XR, DeltaX, Width,   % info for points in a row
               YPix, YI, DeltaY, Height,  % info for rows
               ConfigMap)
        when XPix > 0, YPix > 0 ->

    %% get iteration count for this point
    NewPoint = compute_points:compute_iteration_value(
                                    maps:get(fractalAlg, ConfigMap),
                                    maps:get(cReal, ConfigMap),
                                    maps:get(cImaginary, ConfigMap),
                                    XR,
                                    YI,
                                    0,          %iteration count starts at zero
                                    maps:get(maxIterationThreshold, ConfigMap),
                                    maps:get(bailoutThreshold, ConfigMap)
                                    ),

    NewRowData = [ NewPoint | RowData ],
    NewXPix    = XPix - 1,                  % decrement moving left building row
    NewXR      = XR - DeltaX,               % decrease XR to the left
    computeFractalDataIntoFile( DataFile, NewRowData,
                   NewXPix, NewXR, DeltaX, Width,
                   YPix, YI, DeltaY, Height,
                   ConfigMap).

%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%
%% computeFractalDataIntoFile2/1 API
%%        ConfigMap    - config info
%%        creates file with list of rows
%%              (where each row is list of counts, 1 per pixel)
%%                     suitable to be read with file:consult
%%%%%%%%
computeFractalDataIntoFile2(ConfigMap) ->
    %% get config needed
    %% create height rows of width columns of pixels
    Width           = maps:get(width, ConfigMap),
    Height          = maps:get(height, ConfigMap),
    %% each pixel has a complex number defined by corners of the box
    XRealRight      = maps:get(xRealRight, ConfigMap),
    XRealLeft       = maps:get(xRealLeft, ConfigMap),
    YImaginaryLow   = maps:get(yImaginaryLow, ConfigMap),
    YImaginaryHigh  = maps:get(yImaginaryHigh, ConfigMap),
    %% box is bounded on left by x > XRealLeft
    %%  and bounded on right by x < XRealRight
    %% box is bounded on top by y > YImaginaryHigh
    %%  and bounded on bottom by y > YImaginaryLow
    %% box is width pixels wide and height pixels high

    %% step is floating range divided by number of pixels
    DeltaX = (XRealRight - XRealLeft) / Width,
    DeltaY = (YImaginaryHigh - YImaginaryLow) / Height,

    %% start server
    {ok, Pid} = dataFileSvr:start(ConfigMap),

    %% call computeFractalDataIntoFile2/11
    %% writing rows one at a time
    computeFractalDataIntoFile2(
                        [],             % ThisRow starts empty
                        Width,          % XPix starts width and decrements to 1
                        XRealRight,     % XR starts at right
                                        %  and decrements to by deltaX
                        DeltaX,
                        Width,          % width
                        Height,         % YPix starts height and decrements to 1
                        YImaginaryHigh, % YI starts at top
                                        %  and decrements by delta y
                        DeltaY,
                        Height,         % height
                        ConfigMap).    % map of parameters



%%%%%%%%
%% computeFractalDataIntoFile2/11 API
%%        ThisRow      - a list of the points(count value) in a row/line,
%%                       starts empty and builds R->L until width reached
%%        XPix         - the integer X value of the pixel
%%        XR           - the real component of the floating point number
%%                       for computing fractal for this XPix
%%        DeltaX       - for each pixel, XR increases by this amount
%%        Width        - width of fractal in pixels
%%        YPix         - the integer Y value of the pixel
%%        YI           - the imaginary component of the floating point number
%%                       for computing fractal for this YPix
%%        DeltaY       - for each pixel, YI increases by this amount
%%        Height       - height(intger) of image = number of rows
%%        ConfigMap    - config info
%%%%%%%%
% clause when height is reached, return the rows of data
computeFractalDataIntoFile2(  _ThisRow,
               _XPix, _XR, _DeltaX, _Width,
               YPix, _YI, _DeltaY, _Height,  % only height matters
               _ConfigMap)
        when YPix =< 0 ->

    %% pixels all made already, therefore work is finished so close file and end
    %% more work needed here to actually write file. for now just show status
    io:format('finishing computeFractalDataIntoFile2 - more work needed~n'),
    dataFileSvr:rowStatus(),
    dataFileSvr:writeDataFile(),
    % temp fix until message when complete
    timer:sleep(3000),
    dataFileSvr:rowStatus(),
    io:format('still more work needed~n'),
    ok;

% clause when row is complete  but height not reached - process row and recurse
computeFractalDataIntoFile2( ThisRow,        % row data computed so far
               XPix, _XR, DeltaX, Width,  % info for points in a row
               YPix, YI, DeltaY, Height,  % info for rows
               ConfigMap)
        when XPix =< 0, YPix >= 0 ->

    % sent row#, row to file svr
    dataFileSvr:addARow( {YPix, ThisRow} ),

    % reset to begining of next row
    NewRowData = [],                             % reset data for row to empty
    NewXPix    = Width,                          % reset to end of line
    NewXR      = maps:get(xRealRight, ConfigMap), % reset to end of line
    NewYPix    = YPix - 1,                       % increment row
    NewYI      = YI - DeltaY,                    % increment row
    computeFractalDataIntoFile2( NewRowData,
                   NewXPix, NewXR, DeltaX, Width,
                   NewYPix, NewYI, DeltaY, Height,
                   ConfigMap);

computeFractalDataIntoFile2( RowData,       % row data computed so far
               XPix, XR, DeltaX, Width,   % info for points in a row
               YPix, YI, DeltaY, Height,  % info for rows
               ConfigMap)
        when XPix > 0, YPix > 0 ->

    %% get iteration count for this point
    NewPoint = compute_points:compute_iteration_value(
                                    maps:get(fractalAlg, ConfigMap),
                                    maps:get(cReal, ConfigMap),
                                    maps:get(cImaginary, ConfigMap),
                                    XR,
                                    YI,
                                    0,  %iteration count starts at zero
                                    maps:get(maxIterationThreshold, ConfigMap),
                                    maps:get(bailoutThreshold, ConfigMap)
                                    ),

    NewRowData = [ NewPoint | RowData ],
    NewXPix    = XPix - 1,          % decrement moving left building row
    NewXR      = XR - DeltaX,       % decrease XR to the left
    computeFractalDataIntoFile2( NewRowData,
                   NewXPix, NewXR, DeltaX, Width,
                   YPix, YI, DeltaY, Height,
                   ConfigMap).
