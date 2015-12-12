-module(config_utils).

-export([ jason2atom/1
        ]).

jason2atom(BinaryMap) ->
  Width  = maps:get(<<"width">>, BinaryMap),

  Height = get_height(BinaryMap),
  FractalAlg = get_fractal_alg(BinaryMap),
  
  ImageFile = maps:get(<<"imageFileName">>, BinaryMap),
  ColorAlg = maps:get(<<"colorAlg">>, BinaryMap),
  CReal = maps:get(<<"cReal">>, BinaryMap),
  CImaginary = maps:get(<<"cImaginary">>, BinaryMap),
  ZReal = maps:get(<<"zReal">>, BinaryMap),
  ZImaginary = maps:get(<<"zImaginary">>, BinaryMap),
  XRealRight = maps:get(<<"xRealRight">>, BinaryMap),
  XRealLeft = maps:get(<<"xRealLeft">>, BinaryMap),
  YImaginaryLow = maps:get(<<"yImaginaryLow">>, BinaryMap),
  YImaginaryHigh = maps:get(<<"yImaginaryHigh">>, BinaryMap),
  Bailout = maps:get(<<"bailoutThreshold">>, BinaryMap),
  MaxIter = maps:get(<<"maxIterationThreshold">>, BinaryMap),

  %% need to add checks on input here
  %% need to add optional parameters here (or default on gets above)

  JsonConfigMap = #{ <<"width">> => Width
                   , height => Height
                   , fractalAlg => FractalAlg
                   , imageFile => ImageFile
                   , colorAlg => ColorAlg
                   , cReal => CReal
                   , cImaginary => CImaginary
                   , zReal => ZReal
                   , zImaginary => ZImaginary
                   , xRealRight => XRealRight
                   , xRealLeft => XRealLeft
                   , yImaginaryLow => YImaginaryLow
                   , yImaginaryHigh => YImaginaryHigh
                   , bailoutThreshold => Bailout
                   , maxIterationThreshold => MaxIter
                   },
  %% return the converted map
  JsonConfigMap.

get_height(BinaryMap) ->
  %% mandatory parameter
  Height = maps:get(<<"height">>, BinaryMap),
  check_height(Height).

check_height(Height)
        when is_integer(Height),
             Height > 0,
             Height < 10000
        ->
  %% passed checks so return Height
  Height;
check_height(Height) ->
  %% failed checks so fail with atom indicating issue
  Height = height_must_be_integer_1_to_9999.

get_fractal_alg(BinaryMap) ->
  %% mandatory parameter
  FractalAlg = maps:get(<<"fractalAlg">>, BinaryMap),
  check_fractal_alg(FractalAlg).

check_fractal_alg(FractalAlg)
        when FractalAlg == <<"julian">> ->
  %% valid so return atom
  julian;
check_fractal_alg(FractalAlg)
        when FractalAlg == julian ->
  %% valid so return atom
  julian;
%% add other valid fractal algorithms here as they are created
check_fractal_alg(FractalAlg) ->
  FractalAlg = this_fractal_algorithm_not_implemented.






