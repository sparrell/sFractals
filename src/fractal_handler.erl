-module(fractal_handler).

-export([init/3
        , rest_init/2
        , allowed_methods/2
        , resource_exists/2
        , content_types_accepted/2
        , handle_json/2
        ]).

init( {tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {Method, Req1} = cowboy_req:method(Req),
    {URL, Req2} = cowboy_req:url(Req1),
    lager:debug("~s ~s", [Method, URL]),
    {ok, Req2, #{}}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

resource_exists(Req, State) ->
    %% returning false since only method allowed is post
    %%    and this routine creates new resource
    {false, Req, State}.

content_types_accepted(Req, State) ->
    %% header has content =application/jason/whatever
    { [{ { <<"application">>, <<"json">>, '*'} , handle_json}], Req, State}.

handle_json(Req, State) ->
    %% put stuff here for actually making fractal and returning it
    { ok, Body, Req1} = cowboy_req:body(Req),
    JsonConfigMap = jiffy:decode(Body, [return_maps]),
    ConfigMap = config_utils:jason2atom(JsonConfigMap),
    %% concat WhereRunning, images, filename
    WhereRunning = code:priv_dir(sFractals),
    UserFileName = maps:get(imageFileName, ConfigMap),
    SysFileName = filename:join( [WhereRunning, "images", UserFileName] ),
    SysConfigMap = maps:update(imageFileName, SysFileName, ConfigMap),

    %% calculate the fractal data
    lager:info("Starting data calc"),
    Rows = compute_fractal_data:compute_fractal_data( SysConfigMap ),

    %% make the image
    lager:info("Making Png"),
    compute_fractal_data:make_png_from_data(Rows, SysConfigMap),
    lager:info("Image created at: ~p", [SysFileName]),

    %% decide what to return
    { true, Req1, State}.


