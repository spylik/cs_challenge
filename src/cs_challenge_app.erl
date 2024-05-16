-module(cs_challenge_app).
-behaviour(application).

-export([start/2, stop/1]).

-spec start(Type, Args) -> Result when
    Type    :: application:start_type(),
    Args    :: term(),
    Result  :: {ok, Pid} | {ok, Pid, State} | {error, term()},
    Pid     :: pid(),
    State   :: term().

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/:reply_mode", job_processor_handler, []}
        ]}
    ]),
    cowboy:start_clear(http, [{port, 4000}], #{
        env => #{dispatch => Dispatch}
    }).

-spec stop(State) -> Result when
    State   :: term(),
    Result  :: ok.

stop(_State) ->
    ok = cowboy:stop_listener(http).
