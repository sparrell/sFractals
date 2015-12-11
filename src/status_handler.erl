-module(status_handler).

-export([init/3, rest_init/2, to_html/2]).

init( {tcp, http}, Req, Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
    {Method, Req1} = cowboy_req:method(Req),
    {URL, Req2} = cowboy_req:url(Req1),
    lager:debug("~s ~s", [Method, URL]),
    {ok, Req2, #{}}.

to_html(Req, State) ->
    Body = <<"<html><body>Status Works - needs more later</body></html>">>,
    {Body, Req, State}.
