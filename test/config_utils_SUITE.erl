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
    ].

%% timeout if no reply in a minute
suite() ->
    [{timetrap,{minutes,2}}].

test_good_json(_Config) ->
  JsonConfigMap = #{ <<"bailoutThreshold">> => 4.0
                    ,<<"cImaginary">> => -0.5
                    ,<<"cReal">> => 0.5
                    ,<<"colorAlg">> => <<"simple64">>
                    ,<<"fractalAlg">> => <<"julian">>
                    ,<<"height">> => 2000
                    ,<<"imageFileName">> => <<"example10.png">>
                    ,<<"maxIterationThreshold">> => 63
                    ,<<"width">> => 2000
                    ,<<"xRealLeft">> => -1.0
                    ,<<"xRealRight">> => 1.0
                    ,<<"yImaginaryHigh">> => 2.0
                    ,<<"yImaginaryLow">> => 0.0
                    ,<<"zImaginary">> => -0.1
                    ,<<"zReal">> => -0.1
                    },
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

  Expected = config_utils:jason2atom(JsonConfigMap),
  ok.

test_bad_char_json(_Config) ->
  JsonConfigMap = #{ <<"bailoutThreshold">> => 4.0
                    ,<<"cImaginary">> => -0.5
                    ,<<"cReal">> => 0.5
                    ,<<"colorAlg">> => <<"simple64">>
                    ,<<"fractalAlg">> => <<"julian">>
                    ,<<"height">> => 2000
                    ,<<"imageFileName">> => <<"./example10.png">>
                    ,<<"maxIterationThreshold">> => 63
                    ,<<"width">> => 2000
                    ,<<"xRealLeft">> => -1.0
                    ,<<"xRealRight">> => 1.0
                    ,<<"yImaginaryHigh">> => 2.0
                    ,<<"yImaginaryLow">> => 0.0
                    ,<<"zImaginary">> => -0.1
                    ,<<"zReal">> => -0.1
                    },
  try config_utils:jason2atom(JsonConfigMap) of
    _ -> ct:fail(failed_to_catch_bad_char)
  catch
    error:input_has_bad_char -> ok
  end,
  %% try another bad character
  JsonConfigMap2 = maps:update(<<"imageFileName">>, <<"example?10.png">>, JsonConfigMap),
  try config_utils:jason2atom(JsonConfigMap2) of
    _ -> ct:fail(failed_to_catch_bad_char)
  catch
    error:input_has_bad_char -> ok
  end,
  %% try another bad character
  JsonConfigMap3 = maps:update(<<"imageFileName">>, <<3>>, JsonConfigMap),
  try config_utils:jason2atom(JsonConfigMap3) of
    _ -> ct:fail(failed_to_catch_bad_char)
  catch
    error:input_has_bad_char -> ok
  end,
  %% try different type
  JsonConfigMap4 = maps:update(<<"imageFileName">>, atom_not_bin, JsonConfigMap),
  try config_utils:jason2atom(JsonConfigMap4) of
    _ -> ct:fail(failed_to_catch_bad_char)
  catch
    error:imageFile_is_not_binary_nor_list -> ok
  end,
  ok.

bad_width(_Config) ->
  JsonConfigMap = #{ <<"bailoutThreshold">> => 4.0
                    ,<<"cImaginary">> => -0.5
                    ,<<"cReal">> => 0.5
                    ,<<"colorAlg">> => <<"simple64">>
                    ,<<"fractalAlg">> => <<"julian">>
                    ,<<"height">> => 2000
                    ,<<"imageFileName">> => <<"example10.png">>
                    ,<<"maxIterationThreshold">> => 63
                    ,<<"width">> => -2000
                    ,<<"xRealLeft">> => -1.0
                    ,<<"xRealRight">> => 1.0
                    ,<<"yImaginaryHigh">> => 2.0
                    ,<<"yImaginaryLow">> => 0.0
                    ,<<"zImaginary">> => -0.1
                    ,<<"zReal">> => -0.1
                    },
  try config_utils:jason2atom(JsonConfigMap) of
    _ -> ct:fail(failed_to_catch_bad_width)
  catch
    error:width_must_be_integer_1_to_9999 -> ok
  end,
  JsonConfigMap2 = maps:update(<<"width">>, 0 , JsonConfigMap),
  try config_utils:jason2atom(JsonConfigMap2) of
    _ -> ct:fail(failed_to_catch_bad_width)
  catch
    error:width_must_be_integer_1_to_9999 -> ok
  end,
  JsonConfigMap3 = maps:update(<<"width">>, 10000 , JsonConfigMap),
  try config_utils:jason2atom(JsonConfigMap3) of
    _ -> ct:fail(failed_to_catch_bad_width)
  catch
    error:width_must_be_integer_1_to_9999 -> ok
  end,
  JsonConfigMap4 = maps:update(<<"width">>, 10.5 , JsonConfigMap),
  try config_utils:jason2atom(JsonConfigMap4) of
    _ -> ct:fail(failed_to_catch_bad_width)
  catch
    error:width_must_be_integer_1_to_9999 -> ok
  end,
  ok.
