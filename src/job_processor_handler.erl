-module(job_processor_handler).

-export([init/2]).

-define(STANDARD_ERR, <<"{\"error\":\"Something wrong\"}">>).

-spec init(Req, State) -> Result when
    Req     :: cowboy_req:req(),
    State   :: [],
    Result  :: {ok, cowboy_req:req(), State}.

init(Req, State) ->
    {ok, Body, _Req} = cowboy_req:read_body(Req),
    ReplyMode = binary_to_existing_atom(maps:get(reply_mode, cowboy_req:bindings(Req), <<"sorted">>)),
    case job_processor:process_tasks(Body, ReplyMode) of
        {ok, Result} ->
            Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Result, Req),
            {ok, Req2, State};
        {error, _Reason} ->
            Req2 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, ?STANDARD_ERR, Req),
            {ok, Req2, State}
    end.
