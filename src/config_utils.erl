-module(config_utils).

-export([ jason2atom/1
        ]).

jason2atom(BinaryMap) ->
  Width  = maps:get(<<"width">>, BinaryMap),
  lager:debug("Width(fix later) ~p", [Width]),
  Height = get_height(BinaryMap),
  lager:debug("Height ~p", [Height]),
  FractalAlg = get_fractal_alg(BinaryMap),
  lager:debug("FractalAlg ~p", [FractalAlg]),
  ImageFile = get_image_file(BinaryMap),
  lager:debug("ImageFile ~p", [ImageFile]),
  ColorAlg = get_color_alg(BinaryMap),
  lager:debug("ColorAlg ~p", [ColorAlg]),
  
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
check_height(_Height) ->
  %% failed checks so fail with atom indicating issue
  erlang:error(height_must_be_integer_1_to_9999).

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
check_fractal_alg(_FractalAlg) ->
  erlang:error(this_fractal_algorithm_not_implemented).

%% get and validate image file name
get_image_file(BinaryMap) ->
  ImageFile = maps:get(<<"imageFileName">>, BinaryMap),
  check_image_file(ImageFile).

check_image_file(ImageFile) when is_binary(ImageFile) ->
  %% convert binary to normal string list
  lager:debug('ImageFile Binary ~p', [ImageFile]),
  StringList = binary_to_list(ImageFile),
  lager:debug('ImageFile String from bin ~p', [StringList]),
  %% now run rest of checks
  check_image_file(StringList);
check_image_file(ImageFile) when is_list(ImageFile) ->
  lager:debug('got to ImageFile String ~p', [ImageFile]),
  check_illegal_filename_chars(ImageFile),
  %% all good so return string
  ImageFile;
check_image_file(_ImageFile) ->
  erlang:error(imageFile_is_not_binary_nor_list).

check_illegal_filename_chars( [] ) ->
  %% done and all fine if got this far
  ok;
check_illegal_filename_chars( [H | T] ) ->
  %% check if first char is legal
  is_good_char(H),
  %% recurse thru rest of char
  check_illegal_filename_chars( T ).

is_good_char(C)
        when not is_integer(C) ->
  erlang:error(input_has_bad_char);
is_good_char(C)
        when C < 45 ->
  erlang:error(input_has_bad_char);
is_good_char(C)
        when C =:= 47 ->
  erlang:error(input_has_bad_slash_char);
is_good_char(C)
        when C > 57, C < 65 ->
  erlang:error(input_has_bad_char);
is_good_char(C)
        when C > 90, C < 95 ->
  erlang:error(input_has_bad_char);
is_good_char(C)
        when C =:= 96 ->
  erlang:error(input_has_bad_acc_char);
is_good_char(C)
        when C > 122 ->
  erlang:error(input_has_bad_char);
is_good_char(_C) ->
  %% this char ok
  ok.

get_color_alg(BinaryMap) ->
  InitalColorAlg  = maps:get(<<"colorAlg">>, BinaryMap),
  %% check validity and return if valid
  check_color_alg(InitalColorAlg).

check_color_alg(InitialColorAlg) when InitialColorAlg =:= "simplest" ->
  simplest;
check_color_alg(InitialColorAlg) when InitialColorAlg =:= <<"simplest">> ->
  simplest;
check_color_alg(InitialColorAlg) when InitialColorAlg =:= <<"simplest2">> ->
  simplest2;
check_color_alg(InitialColorAlg) when InitialColorAlg =:= "simplest2" ->
  simplest2;
check_color_alg(InitialColorAlg) when InitialColorAlg =:= <<"simple16">> ->
  simple16;
check_color_alg(InitialColorAlg) when InitialColorAlg =:= "simple16" ->
  simple16;
check_color_alg(InitialColorAlg) when InitialColorAlg =:= <<"simple32">> ->
  simple32;
check_color_alg(InitialColorAlg) when InitialColorAlg =:= "simple32" ->
  simple32;
check_color_alg(InitialColorAlg) when InitialColorAlg =:= <<"blue32">> ->
  blue32;
check_color_alg(InitialColorAlg) when InitialColorAlg =:= "blue32" ->
  blue32;
check_color_alg(InitialColorAlg) when InitialColorAlg =:= <<"simple64">> ->
  simple64;
check_color_alg(InitialColorAlg) when InitialColorAlg =:= "simple64" ->
  simple64;
check_color_alg(_) ->
  erlang:error(unknown_color_alg).





