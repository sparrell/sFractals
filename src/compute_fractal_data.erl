%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%%
%%%-------------------------------------------------------------------

-module(compute_fractal_data).
-author("Duncan Sparrell").
%% try to make simple fractal data
%% and then turn into color data
%% and then make image

%% public API
-export([
          compute_fractal_data/1,       % create a block of fractal data
          compute_fractal_data_file/1,  % create fractal data and write to file
          compute_fractal_data_file2/1, % create fractal data and write to file
          make_png_from_data/2,         % create Png from block of fractal data
          make_png_from_file/1          % create Png from file of fractal data
          ]).

%% expose functions for test
-export([ compute_fractal_data/11 ]).

%% public API for making fractal
make_png_from_file(ConfigMap) ->       % create a Png from file of fractal data
    %% get the data
    {ok, Rows} = file:consult( maps:get( dataFileName, ConfigMap) ),

    %% make the png
    make_png_from_data(Rows, ConfigMap).


%% create a Png from block of fractal data
make_png_from_data(Rows, ConfigMap) ->
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
%% compute_fractal_data/1 API
%%        ConfigMap    - config info
%%        returns Rows
%%          list of rows where each row is list of counts, 1 per pixel)
%%%%%%%%
compute_fractal_data(ConfigMap) ->
    %% get config needed
    %% create height rows of width columns of pixels
    Width           = maps:get(<<"width">>, ConfigMap),
    Height          = maps:get(height, ConfigMap),
    %% each pixel is corresonding complex number defined by corners of the box
    XRealRight      = maps:get(xRealRight, ConfigMap),
    XRealLeft       = maps:get(xRealLeft, ConfigMap),
    YImaginaryLow   = maps:get(yImaginaryLow, ConfigMap),
    YImaginaryHigh  = maps:get(yImaginaryHigh, ConfigMap),
    %% box is bounded on left by x > XRealLeft
    %%      and bounded on right by x < XRealRight
    %% box is bounded on top by y > YImaginaryHigh
    %%      and bounded on bottom by y > YImaginaryLow
    %% box is width pixels wide and height pixels high

    %% step is floating range divided by number of pixels
    DeltaX = (XRealRight - XRealLeft) / Width,
    DeltaY = (YImaginaryHigh - YImaginaryLow) / Height,

    %% call compute_fractal_data/11
    % return Rows from compute_fractal_data/11
    compute_fractal_data( [],             % Rows starts empty
                        [],             % ThisRow starts empty
                        Width,          % XPix starts width and decrements to 1
                        XRealRight,     % XR starts at right
                                        %     and decrements to by deltaX
                        DeltaX,
                        Width,          % width
                        Height,         % YPix starts height and decrements to 1
                        YImaginaryHigh, % YI starts at top
                                        %     and decrements by delta y
                        DeltaY,
                        Height,         % height
                        ConfigMap).    % map of parameters



%%%%%%%%
%% compute_fractal_data/11 API
%%        Rows         - a list of the rows(lines) of data, starts empty
%%                         and builds until height reached, built top down
%%        ThisRow      - a list of the points(count value) in a row/line,
%%                         starts empty and builds R->L until width reached
%%        XPix         - the integer X value of the pixel
%%        XR           - the real component of the floating point number
%%                         for computing fractal for this XPix
%%        DeltaX       - for each pixel, XR increases by this amount
%%        Width        - width of fractal in pixels
%%        YPix         - the integer Y value of the pixel
%%        YI           - the imaginary component of the floating point number
%%                         for computing fractal for this YPix
%%        DeltaY       - for each pixel, YI increases by this amount
%%        Height       - height(intger) of image = number of rows
%%        ConfigMap    - config info
%%%%%%%%
% clause when height is reached, return the rows of data
compute_fractal_data( Rows, _ThisRow,
               _XPix, _XR, _DeltaX, _Width,
               YPix, _YI, _DeltaY, _Height,  % only height matters
               _ConfigMap)
        when YPix =< 0 ->

    %% pixels all made already so done
    Rows;

% clause when row is complete  but height not reached - process row and recurse
compute_fractal_data( Rows, ThisRow,        % row data computed so far
               XPix, _XR, DeltaX, Width,  % info for points in a row
               YPix, YI, DeltaY, Height,  % info for rows
               ConfigMap)
        when XPix =< 0, YPix >= 0 ->

    % add row
    NewRows = [ ThisRow | Rows ],

    % reset to begining of next row
    NewRowData = [],                               % reset data for row to empty
    NewXPix    = Width,                            % reset to end of line
    NewXR      = maps:get(xRealRight, ConfigMap),  % reset to end of line
    NewYPix    = YPix - 1,                         % increment row
    NewYI      = YI - DeltaY,                      % increment row
    compute_fractal_data( NewRows, NewRowData,
                   NewXPix, NewXR, DeltaX, Width,
                   NewYPix, NewYI, DeltaY, Height,
                   ConfigMap);

compute_fractal_data( Rows, RowData,       % row data computed so far
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
                                   0,      %iteration count starts at zero
                                   maps:get(maxIterationThreshold, ConfigMap),
                                   maps:get(bailoutThreshold, ConfigMap)
                                   ),

    NewRowData = [ NewPoint | RowData ],
    %% decrement moving left building row
    NewXPix    = XPix - 1,
    %% decrease XR to the left
    NewXR      = XR - DeltaX,
    compute_fractal_data( Rows, NewRowData,
                   NewXPix, NewXR, DeltaX, Width,
                   YPix, YI, DeltaY, Height,
                   ConfigMap).


%%%%%%%%
%% compute_fractal_data_file/1 API
%%        ConfigMap    - config info
%%        creates file with list of rows
%%           (where each row is list of counts, 1 per pixel)
%%        suitable to be read with file:consult
%%%%%%%%
compute_fractal_data_file(ConfigMap) ->
    %% get config needed
    DataFileName  = maps:get(dataFileName, ConfigMap),
    %% create height rows of width columns of pixels
    Width           = maps:get(<<"width">>, ConfigMap),
    Height          = maps:get(height, ConfigMap),
    %% each pixel has a corresonding complex number
    %%     defined by corners of the box
    XRealRight      = maps:get(xRealRight, ConfigMap),
    XRealLeft       = maps:get(xRealLeft, ConfigMap),
    YImaginaryLow   = maps:get(yImaginaryLow, ConfigMap),
    YImaginaryHigh  = maps:get(yImaginaryHigh, ConfigMap),
    %% box is bounded on left by x > XRealLeft
    %%   and bounded on right by x < XRealRight
    %% box is bounded on top by y > YImaginaryHigh
    %%   and bounded on bottom by y > YImaginaryLow
    %% box is width pixels wide and height pixels high

    %% step is floating range divided by number of pixels
    DeltaX = (XRealRight - XRealLeft) / Width,
    DeltaY = (YImaginaryHigh - YImaginaryLow) / Height,

    %% open file
    {ok, DataFile} = file:open(DataFileName, [write]),

    %% call compute_fractal_data_file/11
    %% writing rows one at a time
    compute_fractal_data_file( DataFile,       % file where data is written
                        [],             % ThisRow starts empty
                        Width,          % XPix starts width and decrements to 1
                        XRealRight,     %% XR starts at right
                                        %%    and decrements to by deltaX
                        DeltaX,
                        Width,          % width
                        Height,         % YPix starts height
                                        %%    and decrements to 1
                        YImaginaryHigh, % YI starts at top
                                        %%    and decrements by delta y
                        DeltaY,
                        Height,         % height
                        ConfigMap).    % map of parameters



%%%%%%%%
%% compute_fractal_data_file/11 API
%%        DataFile     - file already opened for writing
%%        ThisRow      - a list of the points(count value)
%%                       in a row/line, starts empty
%%                       and builds R->L until width reached
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
compute_fractal_data_file( DataFile, _ThisRow,
               _XPix, _XR, _DeltaX, _Width,
               YPix, _YI, _DeltaY, _Height,  % only height matters
               _ConfigMap)
        when YPix =< 0 ->

    %% pixels all made already, therefore work is finished so close file and end
    file:close(DataFile);

% clause when row is complete  but height not reached - process row and recurse
compute_fractal_data_file( DataFile, ThisRow,        % row data computed so far
               XPix, _XR, DeltaX, Width,  % info for points in a row
               YPix, YI, DeltaY, Height,  % info for rows
               ConfigMap)
        when XPix =< 0, YPix >= 0 ->

    % add row to file
    io:format(DataFile, "~w.~n", [ThisRow]),

    % reset to begining of next row
    NewRowData = [],                               % reset data for row to empty
    NewXPix    = Width,                            % reset to end of line
    NewXR      = maps:get(xRealRight, ConfigMap),  % reset to end of line
    NewYPix    = YPix - 1,                         % increment row
    NewYI      = YI - DeltaY,                      % increment row
    compute_fractal_data_file( DataFile, NewRowData,
                   NewXPix, NewXR, DeltaX, Width,
                   NewYPix, NewYI, DeltaY, Height,
                   ConfigMap);

compute_fractal_data_file( DataFile, RowData,       % row data computed so far
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
                                     0,   %iteration count starts at zero
                                     maps:get(maxIterationThreshold, ConfigMap),
                                     maps:get(bailoutThreshold, ConfigMap)
                                     ),

    NewRowData = [ NewPoint | RowData ],
    NewXPix    = XPix - 1,           % decrement moving left building row
    NewXR      = XR - DeltaX,        % decrease XR to the left
    compute_fractal_data_file( DataFile, NewRowData,
                   NewXPix, NewXR, DeltaX, Width,
                   YPix, YI, DeltaY, Height,
                   ConfigMap).

%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%
%% compute_fractal_data_file2/1 API
%%        ConfigMap    - config info
%%        creates file with list of rows
%%             (where each row is list of counts, 1 per pixel)
%%        suitable to be read with file:consult
%%%%%%%%
compute_fractal_data_file2(ConfigMap) ->
    %% get config needed
    %% create height rows of width columns of pixels
    Width           = maps:get(<<"width">>, ConfigMap),
    Height          = maps:get(height, ConfigMap),
    %% each pixel has a corresonding complex number
    %%    defined by corners of the box
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

    %% start server
    {ok, Pid} = data_file_svr:start(ConfigMap),

    %% call compute_fractal_data_file2/11
    %% writing rows one at a time
    compute_fractal_data_file2(
                        [],             % ThisRow starts empty
                        Width,          % XPix starts width and decrements to 1
                        XRealRight,     %% XR starts at right
                                        %%    and decrements to by deltaX
                        DeltaX,
                        Width,          % width
                        Height,         % YPix starts height
                                        %%    and decrements to 1
                        YImaginaryHigh, % YI starts at top
                                        %%    and decrements by delta y
                        DeltaY,
                        Height,         % height
                        ConfigMap).    % map of parameters



%%%%%%%%
%% compute_fractal_data_file2/11 API
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
compute_fractal_data_file2(  _ThisRow,
               _XPix, _XR, _DeltaX, _Width,
               YPix, _YI, _DeltaY, _Height,  % only height matters
               _ConfigMap)
        when YPix =< 0 ->

    %% pixels all made already, therefore work is finished so close file and end
    %% more work needed here to actually write file. for now just show status
    lager:debug('finishing compute_fractal_data_file2 - more work needed~n'),
    data_file_svr:rowStatus(),
    data_file_svr:writeDataFile(),
    % temp fix until message when complete
    timer:sleep(3000),
    data_file_svr:rowStatus(),
    lager:debug('really finishing compute_fractal_data_file2~n'),
    lager:debug('still more work needed~n'),
    ok;

% clause when row is complete  but height not reached - process row and recurse
compute_fractal_data_file2( ThisRow,        % row data computed so far
               XPix, _XR, DeltaX, Width,  % info for points in a row
               YPix, YI, DeltaY, Height,  % info for rows
               ConfigMap)
        when XPix =< 0, YPix >= 0 ->

    % sent row#, row to file svr
    data_file_svr:addARow( {YPix, ThisRow} ),

    % reset to begining of next row
    NewRowData = [],                              % reset data for row to empty
    NewXPix    = Width,                           % reset to end of line
    NewXR      = maps:get(xRealRight, ConfigMap), % reset to end of line
    NewYPix    = YPix - 1,                        % increment row
    NewYI      = YI - DeltaY,                     % increment row
    compute_fractal_data_file2( NewRowData,
                   NewXPix, NewXR, DeltaX, Width,
                   NewYPix, NewYI, DeltaY, Height,
                   ConfigMap);

compute_fractal_data_file2( RowData,       % row data computed so far
               XPix, XR, DeltaX, Width,   % info for points in a row
               YPix, YI, DeltaY, Height,  % info for rows
               ConfigMap)
        when XPix > 0, YPix > 0 ->

    %% get iteration count for this point
    #{fractalAlg := FractalAlg, 
      cReal := CReal } = ConfigMap, 
    NewPoint = compute_points:compute_iteration_value(
                                     FractalAlg,
                                     CReal, 
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
    compute_fractal_data_file2( NewRowData,
                   NewXPix, NewXR, DeltaX, Width,
                   YPix, YI, DeltaY, Height,
                   ConfigMap).
