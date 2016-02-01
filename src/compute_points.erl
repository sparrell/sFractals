%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%%
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are
%%% met:
%%%
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%%
%%% * The names of its contributors may not be used to endorse or promote
%%%   products derived from this software without specific prior written
%%%   permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------

-module(compute_points).
-author("Duncan Sparrell").
-license("Apache 2.0").
%% routines for making fractals

%% public API
-export([
          compute_iteration_value/8
          ]).

%% expose functions for test
-export([
          new_imaginary_c/1
        , new_real_c/1
        , new_imaginary_z/1
        , new_real_z/1
        ]).

%%%%%%%%%%
%% compute_iteration_value computes fractal value and returns iteration count
%%%%%%%%%%
compute_iteration_value( _FractalAlg
                       , _CReal
                       , _CImaginary
                       , _ZReal
                       , _ZImaginary
                       , IterCount
                       , MaxIterationThreshold
                       , _BailoutThreshold )
        when is_integer(IterCount)
           , is_integer(MaxIterationThreshold)
           , IterCount >= MaxIterationThreshold
        ->
    %% reached iteration limit so return count=limit
    MaxIterationThreshold;

%% function clause for exceeding bound
compute_iteration_value( _FractalAlg
                      , _CReal
                      , _CImaginary
                      , ZReal
                      , ZImaginary
                      , IterCount
                      , _MaxIterationThreshold
                      , BailoutThreshold )
        when is_integer(IterCount)
           , is_float(ZReal)
           , is_float(ZImaginary)
           , ((ZReal*ZReal)+(ZImaginary*ZImaginary)) > BailoutThreshold
        ->
    %bailout exceeded so return iterCount
    IterCount;

%% function clause for recursing further
compute_iteration_value( FractalAlg
                       , CReal
                       , CImaginary
                       , ZReal
                       , ZImaginary
                       , IterCount
                       , MaxIterationThreshold
                       , BailoutThreshold )
        when is_integer(IterCount)
           , is_float(CReal)
           , is_float(CImaginary)
           , is_float(ZReal)
           , is_float(ZImaginary)
           , is_integer(MaxIterationThreshold)
        ->

    % compute new Z and C based on fractal algorithm used
    ZCParams      = {FractalAlg, CReal, CImaginary, ZReal, ZImaginary},
    NewZReal      = new_real_z(ZCParams),
    NewZImaginary = new_imaginary_z(ZCParams),
    NewCReal      = new_real_c(ZCParams),
    NewCImaginary = new_imaginary_c(ZCParams),

    compute_iteration_value( FractalAlg
                           , NewCReal
                           , NewCImaginary
                           , NewZReal
                           , NewZImaginary
                           , IterCount+1
                           , MaxIterationThreshold
                           , BailoutThreshold ).

%% function for creating new real value for Julian algorithm
new_real_z({julian, CReal, _CImaginary, ZReal, ZImaginary})
        when is_float(CReal)
           , is_float(ZReal)
           , is_float(ZImaginary)
           ->
    (ZReal*ZReal) - (ZImaginary*ZImaginary) + CReal.

%% function for creating new imaginary value for Julian algorithm
new_imaginary_z({julian, _CReal, CImaginary, ZReal, ZImaginary})
        when is_float(CImaginary)
           , is_float(ZReal)
           , is_float(ZImaginary)
           ->
    (2 * ZReal * ZImaginary) + CImaginary.

%% function for creating new real value for Julian Algorithm
%%     (ie remains unchanged)
new_real_c({julian, CReal, _CImaginary, _ZReal, _ZImaginary})
        when is_float(CReal) ->
    CReal.

%% function for creating new imaginary value for Julian Algorithm
%%     (ie remains unchanged)
new_imaginary_c({julian, _CReal, CImaginary, _ZReal, _ZImaginary})
        when is_float(CImaginary) ->
    CImaginary.

