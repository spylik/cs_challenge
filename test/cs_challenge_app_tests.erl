-module(cs_challenge_app_tests).

-include_lib("eunit/include/eunit.hrl").
-define(BASE_URL, "http://127.0.0.1:4000").
-define(SORTED_URL, ?BASE_URL ++ "/sorted").
-define(BASH_SCRIPT_URL, ?BASE_URL ++ "/bash_script").

sort_tasks_test_() ->
    {setup,
        fun() ->
            application:ensure_all_started(inets),
            application:ensure_all_started(cs_challenge)
        end,
        {inparallel,
            [
                {<<"Examples from challenge definition">>,
                    fun() ->
                        Headers = [{"Content-Type", "application/json"}],

                        InputJson = "{\"tasks\":[{\"command\":\"touch /tmp/file1\",\"name\":\"task-1\"},{\"command\":\"cat /tmp/file1\",\"name\":\"task-2\",\"requires\":[\"task-3\"]},{\"command\":\"echo 'Hello World!' > /tmp/file1\",\"name\":\"task-3\",\"requires\":[\"task-1\"]},{\"command\":\"rm /tmp/file1\",\"name\":\"task-4\",\"requires\":[\"task-2\",\"task-3\"]}]}",

                        ExpectedJsonReply = "{\"tasks\":[{\"command\":\"touch /tmp/file1\",\"name\":\"task-1\"},{\"command\":\"echo 'Hello World!' > /tmp/file1\",\"name\":\"task-3\"},{\"command\":\"cat /tmp/file1\",\"name\":\"task-2\"},{\"command\":\"rm /tmp/file1\",\"name\":\"task-4\"}]}",
                        ExpectedBashScript = "#!/usr/bin/env bash\ntouch /tmp/file1\necho 'Hello World!' > /tmp/file1\ncat /tmp/file1\nrm /tmp/file1\n",
                        {ok, {{"HTTP/1.1",200,"OK"}, HeadersSorted, ResponseForSorted}} = httpc:request(post, {?SORTED_URL, Headers, "application/json", InputJson}, [], []),
                        ?assert(lists:member({"content-type","application/json"}, HeadersSorted)),
                        ?assertEqual(ExpectedJsonReply, ResponseForSorted),


                        {ok, {{"HTTP/1.1",200,"OK"}, HeadersBash, ResponseForBash}} = httpc:request(post, {?BASH_SCRIPT_URL, Headers, "application/json", InputJson}, [], []),
                        ?assert(lists:member({"content-type","text/x-shellscript"}, HeadersBash)),
                        ?assertEqual(ExpectedBashScript, ResponseForBash)
                    end
                }
            ]
        }
    }.
