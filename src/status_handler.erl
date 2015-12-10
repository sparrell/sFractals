-module(status_handler).

-export([init/3]).

init( {tcp,http}, Req, Opts) ->
    Req2 = cowboy_req:reply( 200
                           , [ {<<"content-type">>, <<"text/plain">>} ]
                           , <<"Got Status. Now what?">>
                           , Req
                           ),
	{ok, Req2, Opts}.
