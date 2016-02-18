-module(form_handler).
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

-author("Duncan Sparrell").
-license("Apache 2.0").

-export([init/3
        , rest_init/2
        , allowed_methods/2
        , resource_exists/2
        , content_types_accepted/2
        , handle_parms/2
        ]).

init( {tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {Method, Req1} = cowboy_req:method(Req),
    {URL, Req2} = cowboy_req:url(Req1),
    lager:debug("~s ~s", [Method, URL]),
    %%{ok, Body, Req3} = cowboy_req:body(Req2),
    %%lager:debug("Body: ~p ", [Body]),
    %%{ok, Req3, #{}}.
    {ok, Req2, #{}}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

resource_exists(Req, State) ->
    %% returning false since only method allowed is post
    %%    and this routine creates new resource
    {false, Req, State}.

content_types_accepted(Req, State) ->
    %% header has content =application/json/whatever
    { [ { { <<"application">>, <<"x-www-form-urlencoded">>, '*'}, handle_parms}
      ], Req, State}.

handle_parms(Req, State) ->
    %% put stuff here for actually making fractal and returning it
    %%{ ok, Body, Req1} = cowboy_req:body(Req),
    %%lager:debug("body ~p", [Body] ),
    {ok, KeyValues, Req1} = cowboy_req:body_qs(Req),
    lager:debug("parms ~p", [KeyValues] ),
    ConfigMap = maps:from_list(KeyValues),
    lager:debug("configmap ~p", [ConfigMap] ),

    %% set value if no exceptions or set to exception reason
    JsonConfigMap = try config_utils:json2atom(ConfigMap)
    catch
      throw:Reason ->
        %% caught an input error
        lager:error("Input Error: ~p", [Reason]),
        Reason;
      error:ErrReason ->
        %% caught unanticipted error
        lager:error("Unknown Error: ~p", [ErrReason]),
        ErrReason
    after
      lager:debug("got to after, need to figure what to do")
    end,
    lager:debug("jsonconfigmap ~p", [JsonConfigMap] ),
    %%WhereRunning = code:priv_dir(sFractals),
    %%UserFileName = maps:get(imageFileName, ConfigMap),
    UserFileName = "tempUserFileName",
    %%SysFileName = filename:join( [WhereRunning, "images", UserFileName] ),
    %%SysConfigMap = maps:update(imageFileName, SysFileName, ConfigMap),

    %% decide what to return
    {Host, Req2} = cowboy_req:header(<<"host">>, Req1),
    BinUserFileName = list_to_binary(UserFileName),
    Path = <<"/images/", BinUserFileName/binary>>,
    Location = <<"http://", Host/binary, Path/binary>>,
    %{ {true, Location}, Req2, State}.
    RespBody = io_lib:format("~p", [KeyValues] ),
    {ok, Req3} = cowboy_req:reply(200
                                 , [ {<<"content-type">>
                                     , <<"text/plain">>
                                     }
                                   ]
                                 , RespBody
                                 , Req2
                                 ),
    { true, Req3, State}.

