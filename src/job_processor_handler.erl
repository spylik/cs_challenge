-module(job_processor_handler).

-export([init/2]).

-define(STANDARD_ERR, <<"{\"error\":\"Something wrong\"}">>).

-type reply_mode() ::   job_processor:reply_mode().

-spec init(Req, State) -> Result when
    Req     :: cowboy_req:req(),
    State   :: [],
    Result  :: {ok, cowboy_req:req(), State}.

init(Req, State) ->
    {ok, Body, _Req} = cowboy_req:read_body(Req),
    ReplyMode = reply_mode(Req),
    case job_processor:process_tasks(Body, ReplyMode) of
        {ok, Result} ->
            Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Result, Req),
            {ok, Req2, State};
        {error, _Reason} ->
            Req2 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, ?STANDARD_ERR, Req),
            {ok, Req2, State}
    end.

% =========================== Private helpers ==================================

-spec reply_mode(Req) -> Result when
    Req     :: cowboy_req:req(),
    Result  :: reply_mode().

reply_mode(Req) ->
    case maps:get(reply_mode, cowboy_req:bindings(Req), <<"sorted">>) of
        <<"sorted">> -> sorted;
        <<"bash_script">> -> bash_script
    end.

% ------------------------ end of Private helpers ------------------------------
