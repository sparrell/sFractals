-module(fractal_server).
-behaviour(gen_server).

%% gen svr exports
-export( [init/1
        , terminate/2
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , code_change/3
        ] ).

%% api
-export( [start_link/0, clientComputeFractal/1] ).

-type state()::map().

-spec init(_Args::term()) -> { ok, state() }.
init(_) ->
  lager:info("starting init"),
  { ok,  #{} }.

-spec terminate(Reason::term(), State::state()) -> ok.
terminate(Reason, State) ->
    lager:debug("terminating due to ~p with state=~p", [Reason, State]),
    ok.

-spec handle_call(Request::term(), From::{pid(), term()}, State::state()) ->
        {reply, Reply::term(), NewState::state()}.
handle_call(makeFractal, From, State) ->
    lager:debug("got a makeFractal call from ~p", [From]),
    Reply = figureThisOutLater,
    NewState = State,
    {reply, Reply, NewState};
handle_call(Request, _From, State) ->
    Reply = figureThisOutLater,
    lager:debug("got a call of ~p", [Request]),
    NewState = State,
    {reply, Reply, NewState}.

-spec handle_cast(Request::term(), State::state()) ->
        {noreply, NewState::state()}.
handle_cast(Request, State) ->
    lager:debug("got a cast of ~p", [Request]),
    NewState = State,
    {noreply, NewState}.

-spec handle_info(Request::term(), State::state()) ->
        {noreply, NewState::state()}.
handle_info(Request, State) ->
    lager:debug("got a handle_info of ~p", [Request]),
    NewState = State,
    {noreply, NewState}.

-spec code_change(OldVersion::term(), State::state(), Extra::term()) ->
        {ok, NewState::state()}.
code_change(_OldVersion, State, _Extra) ->
    NewState = State,
    {ok, NewState}.


-spec clientComputeFractal(ConfigMap::map()) -> Filename::string().
clientComputeFractal(ConfigMap) ->
    Timeout = maps:get(timeout, ConfigMap, 30) * 1000, % default timeout of 30s
    gen_server:call(?MODULE, {makeFractalPng, ConfigMap}, Timeout).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



