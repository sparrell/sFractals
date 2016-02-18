%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2015, sFractal Consulting LLC
%%%
%%%-------------------------------------------------------------------

-module(config_utils_SUITE).
-author("Duncan Sparrell").

%% for test
-compile(export_all).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% tests to run
all() ->
    [ test_good_json
    , test_bad_char_json
    , bad_width
    , bad_height
    , bad_fractal_alg
    , bad_color_alg
    , bad_cr
    , bad_ci
    , bad_zr
    , bad_zi
    , bad_xr
    , bad_yi
    , bad_bailout
    , bad_max_iter
    ].

%% timeout if no reply in a minute
suite() ->
    [ {timetrap,{minutes,2}}
    , {userdata, [ {json_map, #{ <<"bailoutThreshold">> => 4.0
                                , <<"cImaginary">> => -0.5
                                , <<"cReal">> => 0.5
                                , <<"colorAlg">> => <<"simple64">>
                                , <<"fractalAlg">> => <<"julian">>
                                , <<"height">> => 2000
                                , <<"imageFileName">> => <<"example10.png">>
                                , <<"maxIterationThreshold">> => 63
                                , <<"width">> => 2000
                                , <<"xRealLeft">> => -1.0
                                , <<"xRealRight">> => 1.0
                                , <<"yImaginaryHigh">> => 2.0
                                , <<"yImaginaryLow">> => 0.0
                                , <<"zImaginary">> => -0.1
                                , <<"zReal">> => -0.1
                                } 
                   } 
                 ]
      }
    ].

test_good_json(_Config) ->
  test_good_param( <<"imageFileName">>
                 , imageFileName
                 , <<"abc-def_ghijklmnopqrstuvwxyz.png">>
                 , "abc-def_ghijklmnopqrstuvwxyz.png"),
  test_good_param( <<"imageFileName">>
                 , imageFileName
                 , <<"ABCDEFGHIJKLMNOPQRSTUVWXYZ.png">>
                 , "ABCDEFGHIJKLMNOPQRSTUVWXYZ.png"),
  test_good_param( <<"imageFileName">>
                 , imageFileName
                 , <<"0123456789">>
                 , "0123456789"),
  test_good_param( <<"fractalAlg">>
                 , fractalAlg
                 , <<"julian">>
                 , julian),
  test_good_param( <<"colorAlg">>
                 , colorAlg
                 , "simplest"
                 , simplest),
  test_good_param( <<"colorAlg">>
                 , colorAlg
                 , "simplest2"
                 , simplest2),
  test_good_param( <<"colorAlg">>
                 , colorAlg
                 , "simple16"
                 , simple16),
  test_good_param( <<"colorAlg">>
                 , colorAlg
                 , "simple32"
                 , simple32),
  test_good_param( <<"colorAlg">>
                 , colorAlg
                 , "blue32"
                 , blue32),
  test_good_param( <<"colorAlg">>
                 , colorAlg
                 , "simple64"
                 , simple64),
  test_good_param( <<"colorAlg">>
                 , colorAlg
                 , <<"simplest">>
                 , simplest),
  test_good_param( <<"colorAlg">>
                 , colorAlg
                 , <<"simplest2">>
                 , simplest2),
  test_good_param( <<"colorAlg">>
                 , colorAlg
                 , <<"simple16">>
                 , simple16),
  test_good_param( <<"colorAlg">>
                 , colorAlg
                 , <<"simple32">>
                 , simple32),
  test_good_param( <<"colorAlg">>
                 , colorAlg
                 , <<"blue32">>
                 , blue32),
  test_good_param( <<"colorAlg">>
                 , colorAlg
                 , <<"simple64">>
                 , simple64),
  ok.

test_good_param(InParam, OutParam, InValue, OutValue) ->
  %% get inital config
  JsonMap = get_base_config(),
  JsonMap2 = maps:update(InParam, InValue , JsonMap),

  Expected = #{ bailoutThreshold => 4.0
              , cImaginary => -0.5
              , cReal => 0.5
              , colorAlg => simple64
              , fractalAlg => julian
              , height => 2000
              , imageFileName => "example10.png"
              , maxIterationThreshold => 63
              , width => 2000
              , xRealLeft => -1.0
              , xRealRight => 1.0
              , yImaginaryHigh => 2.0
              , yImaginaryLow => 0.0
              , zImaginary => -0.1
              , zReal => -0.1},
  Expected2 = maps:update(OutParam, OutValue , Expected),

  Expected2 = config_utils:json2atom(JsonMap2),
  ok.

get_base_config() ->
  %% get inital config
  SuiteData = ?MODULE:suite(),
  {userdata, UserData} =  lists:keyfind(userdata, 1, SuiteData),
  {json_map, JsonMap} =  lists:keyfind(json_map, 1, UserData),
  JsonMap.


bad_color_alg(_Config) ->
  %% use alg not implemented yet to JsonMap and test
  test_bad_param( <<"colorAlg">>
                , <<"unheardofalg">>
                , unknown_color_alg
                , unknown_color_alg
                ).

bad_fractal_alg(_Config) ->
  %% use alg not implemented yet to JsonMap and test
  test_bad_param( <<"fractalAlg">>
                , <<"mandelbrot">>
                , this_fractal_algorithm_not_implemented
                , this_fractal_algorithm_not_implemented
                ),
  test_bad_param( <<"fractalAlg">>
                , <<"kleinian">>
                , this_fractal_algorithm_not_implemented
                , this_fractal_algorithm_not_implemented
                ),
  test_bad_param( <<"fractalAlg">>
                , <<"garbage">>
                , this_fractal_algorithm_not_implemented
                , this_fractal_algorithm_not_implemented
                ),
  ok.

test_bad_char_json(_Config) ->

  %% add bad char to JsonMap and test
  test_bad_param( <<"imageFileName">>
                , <<"./example10.png">>
                , input_has_bad_char
                , failed_to_catch_bad_char
                ),
  %% try another bad character
  test_bad_param( <<"imageFileName">>
                , <<"example?10.png">>
                , input_has_bad_char
                , failed_to_catch_bad_char
                ),
  %% try another bad character
  test_bad_param( <<"imageFileName">>
                , <<3>>
                , input_has_bad_char
                , failed_to_catch_bad_char
                ),

  %% try different type
  test_bad_param( <<"imageFileName">>
                , atom_not_bin
                , imageFile_is_not_binary_nor_list
                , failed_to_catch_bad_char
                ).

bad_width(_Config) ->

  %% make a bad width
  test_bad_param( <<"width">>
                , -2
                , width_must_be_integer_1_to_9999
                , failed_to_catch_bad_width
                ),

  test_bad_param( <<"width">>
                , 10000
                , width_must_be_integer_1_to_9999
                , failed_to_catch_bad_width
                ),

  test_bad_param( <<"width">>
                , 10.5
                , width_must_be_integer_1_to_9999
                , failed_to_catch_bad_width
                ),
  ok.

bad_cr(_Config) ->
  test_bad_param( <<"cReal">>
                , 10
                , must_be_floating_point_number
                , failed_to_catch_bad_cr
                ),
  ok.

bad_ci(_Config) ->
  test_bad_param( <<"cImaginary">>
                , 10
                , must_be_floating_point_number
                , failed_to_catch_bad_ci
                ),
  ok.

bad_zr(_Config) ->
  test_bad_param( <<"zReal">>
                , 10
                , must_be_floating_point_number
                , failed_to_catch_bad_zr
                ),
  ok.

bad_zi(_Config) ->
  test_bad_param( <<"zImaginary">>
                , 10
                , must_be_floating_point_number
                , failed_to_catch_bad_zi
                ),
  ok.

bad_xr(_Config) ->
  test_bad_param( <<"xRealRight">>
                , 10
                , must_be_floating_point_number
                , failed_to_catch_bad_xrr
                ),
  test_bad_param( <<"xRealLeft">>
                , 10
                , must_be_floating_point_number
                , failed_to_catch_bad_xrr
                ),
  %% test left is greater then right. 
  %%   note right in base config is 1.0
  test_bad_param( <<"xRealLeft">>
                , 2.0
                , xreals_must_be_floats_and_right_must_be_greater_than_left
                , failed_to_catch_bad_xrr
                ),
  %%   note left in base config is -1.0
  test_bad_param( <<"xRealRight">>
                , -2.0
                , xreals_must_be_floats_and_right_must_be_greater_than_left
                , failed_to_catch_bad_xrr
                ),
  ok.

bad_bailout(_Config) ->
  test_bad_param( <<"bailoutThreshold">>
                , 10
                , must_be_positive_floating_point_number
                , failed_to_catch_bad_bailout
                ),
  test_bad_param( <<"bailoutThreshold">>
                , -10.5
                , must_be_positive_floating_point_number
                , failed_to_catch_bad_bailout
                ),
  ok.

bad_max_iter(_Config) ->
  test_bad_param( <<"maxIterationThreshold">>
                , 10.5
                , must_be_positive_integer
                , failed_to_catch_bad_bailout
                ),
  test_bad_param( <<"maxIterationThreshold">>
                , -10
                , must_be_positive_integer
                , failed_to_catch_bad_bailout
                ),
  ok.

bad_yi(_Config) ->
  test_bad_param( <<"yImaginaryLow">>
                , 10
                , must_be_floating_point_number
                , failed_to_catch_bad_xrr
                ),
  test_bad_param( <<"yImaginaryHigh">>
                , 10
                , must_be_floating_point_number
                , failed_to_catch_bad_xrr
                ),
  %% test high is greater then low. 
  %%   note high in base config is 2.0
  test_bad_param( <<"yImaginaryLow">>
                , 2.5
                , yi_must_be_floats_and_high_must_be_greater_than_low
                , failed_to_catch_bad_yil
                ),
  %%   note low in base config is 0.0
  test_bad_param( <<"yImaginaryHigh">>
                , -2.0
                , yi_must_be_floats_and_high_must_be_greater_than_low
                , failed_to_catch_bad_yih
                ),
  ok.

bad_height(_Config) ->

  %% make a bad height
  test_bad_param( <<"height">>
                , -2
                , height_must_be_integer_1_to_9999
                , failed_to_catch_bad_height
                ),

  test_bad_param( <<"height">>
                , 10000
                , height_must_be_integer_1_to_9999
                , failed_to_catch_bad_height
                ),

  test_bad_param( <<"height">>
                , 10.5
                , height_must_be_integer_1_to_9999
                , failed_to_catch_bad_height
                ),
  ok.

test_bad_param(Param, Value, Error, ErrorMsg)
     when is_atom(Error)
        , is_atom(ErrorMsg)
     ->
  JsonMap = get_base_config(),
  JsonMap2 = maps:update(Param, Value , JsonMap),
  try config_utils:json2atom(JsonMap2) of
    _ -> ct:fail(ErrorMsg)
  catch
    error:Error -> ok;
    throw:Error -> ok
  end.
