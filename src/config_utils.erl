-module(config_utils).

-export([ jason2atom/1
        ]).

%% for test
- export( [ is_good_char/1 ] ).

jason2atom(BinaryMap) ->
  Width  = get_width(BinaryMap),
  Height = get_height(BinaryMap),
  FractalAlg = get_fractal_alg(BinaryMap),
  ImageFile = get_image_file(BinaryMap),
  ColorAlg = get_color_alg(BinaryMap),
  CReal = get_c_real(BinaryMap),
  CImaginary = get_c_imaginary(BinaryMap),

  CImaginary = maps:get(<<"cImaginary">>, BinaryMap),
  ZReal = maps:get(<<"zReal">>, BinaryMap),
  ZImaginary = maps:get(<<"zImaginary">>, BinaryMap),
  XRealRight = maps:get(<<"xRealRight">>, BinaryMap),
  XRealLeft = maps:get(<<"xRealLeft">>, BinaryMap),
  YImaginaryLow = maps:get(<<"yImaginaryLow">>, BinaryMap),
  YImaginaryHigh = maps:get(<<"yImaginaryHigh">>, BinaryMap),
  Bailout = maps:get(<<"bailoutThreshold">>, BinaryMap),
  MaxIter = maps:get(<<"maxIterationThreshold">>, BinaryMap),
  lager:debug("need more checks on input in config_utils:jason2atom"),

  %% need to add checks on input here
  %% need to add optional parameters here (or default on gets above)

  JsonConfigMap = #{ width => Width
                   , height => Height
                   , fractalAlg => FractalAlg
                   , imageFileName => ImageFile
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

get_width(BinaryMap) ->
  Width  = maps:get(<<"width">>, BinaryMap),
  check_width(Width).

check_width(Width)
        when is_integer(Width),
             Width > 0,
             Width < 10000
        ->
  %% passed checks so return Width
  Width;
check_width(_Width) ->
  %% failed checks so fail with atom indicating issue
  erlang:error(width_must_be_integer_1_to_9999).

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
  StringList = binary_to_list(ImageFile),
  %% now run rest of checks
  check_image_file(StringList);
check_image_file(ImageFile) when is_list(ImageFile) ->
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

%% is_good_char checks only valid characters used
%%    prevents hack to get at other parts of filesystem/webserver
%%    allowed characters
%%       - ("dash" - ascii 45)
%%       . ("dot" - ascii 46)
%%       0-9 (ascii 48-57)
%%       A-Z (ascii 65-90)
%%       _ ("underscrore" - ascii 95)
%%       a-z (ascii 97-122)
is_good_char(C)
        when not is_integer(C) ->
  erlang:error(input_has_bad_char);
is_good_char(C)
        when C =:= 45 % dash
           ; C =:= 46 % dot
           ; ( C >= 48 andalso C =< 57 ) % 0-9
           ; ( C >= 65 andalso C =< 90 ) % A-Z
           ; C =:= 95 % underscrore
           ; ( C >= 97 andalso C =< 122 ) % a-z
        ->
  ok; % valid characters
is_good_char(_C) ->
  %% anything else is bad
  erlang:error(input_has_bad_char).

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

check_is_float(ParamName, Num) when not is_float(Num) ->
  erlang:error("~p/~p must be floating point number", [ParamName, Num] );
check_is_float(ParamName, Num) ->
  Num.

get_c_real(BinaryMap) ->
  CReal = maps:get(<<"cReal">>, BinaryMap),
  check_is_float(c_real, CReal).

get_c_imaginary(BinaryMap) ->
  CImaginary = maps:get(<<"cImaginary">>, BinaryMap),
  check_is_float(c_imaginary, CImaginary).





