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
            Req2 = cowboy_req:reply(200, #{<<"content-type">> => pick_mine_type(ReplyMode)}, Result, Req),
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

-spec pick_mine_type(ReplyMode) -> Result when
    ReplyMode   :: reply_mode(),
    Result      :: binary().

pick_mine_type(sorted) -> <<"application/json">>;
pick_mine_type(bash_script) -> <<"text/x-shellscript">>.

% ------------------------ end of Private helpers ------------------------------
