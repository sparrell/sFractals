%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%% 
%%%-------------------------------------------------------------------

-module(compute_points).
-author("Duncan Sparrell").
%% routines for making fractals

%% public API
-export([ 
          compute_iteration_value/8 
          ]).

%% expose functions for test
-export([ 
          new_imaginary_c/1
        , new_real_c/1
        , newImaginaryZ/1
        , new_real_z/1
        ]).
 
%%%%%%%%%%
%% compute_iteration_value computes fractal value and returns iteration count
%%%%%%%%%%
compute_iteration_value(_FractalAlg,
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
compute_iteration_value(_FractalALg,
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
compute_iteration_value(FractalAlg,
                      CReal,
                      CImaginary, 
                      ZReal,
                      ZImaginary, 
                      IterCount, 
                      MaxIterationThreshold, 
                      BailoutThreshold ) ->

    % compute new Z and C based on fractal algorithm used
    ZCParams      = {FractalAlg,CReal,CImaginary, ZReal,ZImaginary},
    NewZReal      = new_real_z(ZCParams),
    NewZImaginary = newImaginaryZ(ZCParams),
    NewCReal      = new_real_c(ZCParams),
    NewCImaginary = new_imaginary_c(ZCParams),

    compute_iteration_value(FractalAlg,
                          NewCReal,
                          NewCImaginary, 
                          NewZReal, 
                          NewZImaginary, 
                          IterCount+1, 
                          MaxIterationThreshold, 
                          BailoutThreshold ).

% function for creating new real value for Julian algorithm
new_real_z({FractalAlg,CReal,_CImaginary, ZReal,ZImaginary}) 
        when FractalAlg == julian ->
    (ZReal*ZReal) - (ZImaginary*ZImaginary) + CReal.

% function for creating new imaginary value for Julian algorithm
newImaginaryZ({FractalAlg,_CReal,CImaginary, ZReal,ZImaginary}) 
        when FractalAlg == julian ->
    (2 * ZReal * ZImaginary) + CImaginary.

% function for creating new real value for Julian Algorithm (ie remains unchanged)
new_real_c({FractalAlg,CReal,_CImaginary, _ZReal,_ZImaginary}) 
        when FractalAlg == julian ->
    CReal.

% function for creating new imaginary value for Julian Algorithm (ie remains unchanged)
new_imaginary_c({FractalAlg,_CReal,CImaginary, _ZReal,_ZImaginary}) 
        when FractalAlg == julian ->
    CImaginary.

