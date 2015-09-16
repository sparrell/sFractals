%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-module(simpleFractal).
-author("Duncan Sparrell").
%% try to make simple fractal data and then turn into color data and then make image

%% API
-export([ simplFrac/1,testConfig1/0,testOne/0 ]).
 
%% test config 1 - eventually make parameterized and move this to test dir
testConfig1() ->
    % return map of config parameters
    TC = #{ fractalAlg => julian,  % Fractal Algorithm is julian
       fractalImageFileName => "./julian10.10.dot5.dot5.4.100.png",  %image file created
       width => 10, % width=10
       height => 10, % height=10
       cReal => 0.5, % real portion of C0
       cImaginary => -0.5, % imaginary portion of C0
       zReal => -0.1, %real portion of Z0 (don't care for Julian)
       zImaginary => -0.1, %imaginary portion of Z0 (don't care for Julian)
       xRealRight => 3.0,
       xRealLeft => -3.0,
       yImaginaryLow => -3.0,
       yImaginaryHigh => 3.0,
       bailoutThreshold => 4,
       maxIterationThreshold => 100 },

    % return dict of config parameters

    %% put list of key/values into dict and return it
    dict:from_list( [ {fractalAlg, julian},  % Fractal Algorithm is julian
      { fractalImageFileName, "./julian10.10.dot5.dot5.4.100.png"},  %image file created
      { width, 10}, % width=10
      { height, 10}, % height=10
      { cReal, 0.5}, % real portion of C0
      { cImaginary, -0.5}, % imaginary portion of C0
      { zReal, -0.1}, %real portion of Z0 (don't care for Julian)
      { zImaginary, -0.1}, %imaginary portion of Z0 (don't care for Julian)
      { xRealRight, 3.0}, 
      { xRealLeft, -3.0},
      { yImaginaryLow, -3.0},
      { yImaginaryHigh, 3.0},
      { bailoutThreshold, 4},
      { maxIterationThreshold, 100} ] ).

    
%% test1 - first simple fractal
testOne() ->
    simplFrac(testConfig1()).



% create a simple fractal
simplFrac(Config) ->

    %% FractalConfig = {FractalAlg,CReal,CImaginary, ZReal,ZImaginary,BailoutThreshold,MaxIterationThreshold}
    %% simple config
    FractalConfig = {julian,0.5,-0.5, 0,0,4,100},
    Width = 10,
    Height = 10,

    %% first create julian data set with some hardcoded configs (add configing later)
        %% XRealRight = 3.0 
        %% XRealLeft = -3.0
        %% YImaginaryLow = -3.0
        %% YImaginaryHigh = 3.0
    IterationCountData = createPointData(Width,Height,-3.0, 3.0, -3.0, 3.0, FractalConfig,Config),

    %% print some stats on distribution of counts
    %% need to do

    %% colorize the data using simplest alg
    ColorData = imagelib:colorizeData(IterationCountData,simplest,{}),

    %% take the x,y,color data and creates an image
    imagelib:makeImageFromData( ColorData, Config ),

    ok.

createPointData(Width,Height, XRealLeft, XRealRight, YImaginaryLow, YImaginaryHigh, FractalConfig,Config) 
    when XRealLeft < XRealRight, YImaginaryLow < YImaginaryHigh ->
    %% box is bounded on left by x > XRealLeft and bounded on right by x < XRealRight
    %% box is bounded on top by y > YImaginaryHigh and bounded on bottom by y > YImaginaryLow
    %% box is width pixels wide and height pixels high

    %% step is floating range divided by number of pixels
    DeltaX = (XRealRight - XRealLeft) / Width,
    DeltaY = (YImaginaryHigh - YImaginaryLow) / Height,

    makeAllRows(Width,Height,1,XRealLeft,YImaginaryLow,DeltaX,DeltaY,FractalConfig,[],Config).
        %% note the 3rd parameter is "CurrentPixelY" which starts at bottom
        %% note the 9th parameter is "IterCounts" which starts as empty list
		%% makeAllRows returns InterCount, which is then returned by createPointData 

%% make one row at a time, starting at bottom and working up 
%%  note builds list from tail so this yeilds highest Y nearest front 
%%  which is useful in case later we might to build file on fly that way 'from top'

% function head for done
makeAllRows(_Width,Height, CurrentPixelY,_XRealLeft,_CurrentImaginaryY,_DeltaX,_DeltaY,_FractalConfig,IterCounts,Config)
        when CurrentPixelY > Height ->  
    IterCounts;   %reached bottom so return the iteration count data

% function head for not-done so recurse
makeAllRows(Width,Height,CurrentPixelY,XRealLeft,CurrentImaginaryY,DeltaX,DeltaY,FractalConfig,IterCounts,Config) ->
    % reached here so need to make another row
    % note Y-1 one since doing top down
    % similarly Imaginary& went down by delta
	% IterCounts is updated with the new row calculated by makePoints and is the returned value
    makeAllRows(Width,Height,CurrentPixelY+1,XRealLeft,CurrentImaginaryY+DeltaY,DeltaX,DeltaY,FractalConfig,
        makePoints(Width,1,CurrentPixelY,XRealLeft, CurrentImaginaryY,DeltaX,FractalConfig,IterCounts,Config),Config ).
    
% function head for done-with-row
makePoints(Width,CurrentPixelX,_CurrentPixelY,_CurrentRealX,_CurrentImaginaryY,_DeltaX,_FractalConfig,IterCounts,Config)
        when CurrentPixelX > Width ->  
    IterCounts;   %reached end of line so return the iteration count data

% function head for not-done so recurse
makePoints(Width,CurrentPixelX,CurrentPixelY,CurrentRealX,CurrentImaginaryY,DeltaX,FractalConfig,IterCounts,Config) ->   
    %reached here so need to add another pixel
    % CurrentPixelX starts at 1 increases by 1 each recurse
    % CurrentRealX starts at XRealLeft and increases by DeltaX each recurse
    % IterCounts has point {X,Y,Iter} added by function addOnePoint
    FractalAlg= maps:get(fractalAlg,ConfigMap),
    makePoints(Width,CurrentPixelX+1,CurrentPixelY,CurrentRealX+DeltaX,CurrentImaginaryY,DeltaX,FractalConfig,
        addOnePoint(CurrentPixelX,
                    CurrentPixelY,
                    CurrentRealX,
                    CurrentImaginaryY,
                    FractalAlg,
                    IterCounts,
                    Config),
         Config ).
        %% note addOnePoint returns the new value of IterCounts into the recursive call

%% Compute fractal (bounded or unbounded)
    %% FractalConfig = {FractalAlg,CReal,CImaginary, ZReal,ZImaginary,BailoutThreshold,MaxIterationThreshold}
    %% FractalAlg is which algorithm (only one at moment)
% this function clause for creating julian fractals. eventually will generalize
addOnePoint(CurrentPixelX,
            CurrentPixelY,
            Xreal,
            Yimaginary,
            FractalAlg,
            IterCounts,
            Config) 
    when FractalAlg == julian ->

    CReal = dict:fetch(cReal,Config),
    CImaginary = dict:fetch(cImaginary,Config),
    MaxIterationThreshold = dict:fetch(maxIterationThreshold,Config),
    BailoutThreshold = dict:fetch(bailoutThreshold,Config),
    %% return IterCounts with new point added. Note 3rd item in tuple is Count, and 4th item in compute call is initial iteration
    Count = computeIterationValue(CReal,
                                  CImaginary,
                                  Xreal,
                                  Yimaginary,
                                  0,                     % initial iteration count
                                  MaxIterationThreshold, 
                                  BailoutThreshold ),

    [ {CurrentPixelX, CurrentPixelY, Count } | IterCounts ].

%% computeIterationValue computes fractal value and returns iteration count
%% function clause for exceeding iteration count
computeIterationValue(_CReal, _CImaginary, _ZReal,_ZImaginary, 
            IterCount, MaxIterationThreshold, _BailoutThreshold )
        when IterCount >= MaxIterationThreshold ->   % reached iteration limit so return count=limit
    MaxIterationThreshold;
%% function clause for exceeding bound
computeIterationValue(_CReal,_CImaginary, ZReal,ZImaginary, IterCount, 
            _MaxIterationThreshold, BailoutThreshold ) 
        when ((ZReal*ZReal)+(ZImaginary*ZImaginary)) > BailoutThreshold -> %bailout exceeded so return iterCount
    IterCount;
%% function clause for recursing further
computeIterationValue(CReal,CImaginary, ZReal,ZImaginary, IterCount, 
        MaxIterationThreshold, BailoutThreshold ) ->
    computeIterationValue(CReal,CImaginary, (ZReal*ZReal) - (ZImaginary*ZImaginary) + CReal, (2 * ZReal * ZImaginary) + CImaginary, IterCount+1, MaxIterationThreshold, BailoutThreshold ).

