-module(job_processor_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TESTMODULE, job_processor).

sort_tasks_test_() ->
    {setup,
        fun() -> error_logger:tty(true) end,
        {inparallel,
            [
                {<<"Examples from challenge definition">>,
                    fun() ->
                        Expected =
                            [
                                #{name => <<"task-1">>, command => <<"touch /tmp/file1">>},
                                #{name => <<"task-3">>, command => <<"echo 'Hello World!' > /tmp/file1">>},
                                #{name => <<"task-2">>, command => <<"cat /tmp/file1">>},
                                #{name => <<"task-4">>, command => <<"rm /tmp/file1">>}
                            ],

                        ?assertEqual(
                            Expected,
                            ?TESTMODULE:sort_tasks(
                                [
                                    #{name => <<"task-1">>, command => <<"touch /tmp/file1">>},
                                    #{name => <<"task-2">>, command => <<"cat /tmp/file1">>, requires => [<<"task-3">>]},
                                    #{name => <<"task-3">>, command => <<"echo 'Hello World!' > /tmp/file1">>, requires => [<<"task-1">>]},
                                    #{name => <<"task-4">>, command => <<"rm /tmp/file1">>, requires => [<<"task-2">>, <<"task-3">>]}
                                ]
                            )
                        ),

                        ?assertEqual(
                            Expected,
                            ?TESTMODULE:sort_tasks(
                                [
                                    #{name => <<"task-3">>, command => <<"echo 'Hello World!' > /tmp/file1">>, requires => [<<"task-1">>]},
                                    #{name => <<"task-1">>, command => <<"touch /tmp/file1">>},
                                    #{name => <<"task-2">>, command => <<"cat /tmp/file1">>, requires => [<<"task-3">>]},
                                    #{name => <<"task-4">>, command => <<"rm /tmp/file1">>, requires => [<<"task-2">>, <<"task-3">>]}
                                ]
                            )
                        ),

                        ?assertEqual(
                            Expected,
                            ?TESTMODULE:sort_tasks(
                                [
                                    #{name => <<"task-2">>, command => <<"cat /tmp/file1">>, requires => [<<"task-3">>]},
                                    #{name => <<"task-3">>, command => <<"echo 'Hello World!' > /tmp/file1">>, requires => [<<"task-1">>]},
                                    #{name => <<"task-1">>, command => <<"touch /tmp/file1">>},
                                    #{name => <<"task-4">>, command => <<"rm /tmp/file1">>, requires => [<<"task-2">>, <<"task-3">>]}
                                ]
                            )
                        )
                    end
                }
            ]
        }
    }.

process_tasks_test_() ->
    {setup,
        fun() -> error_logger:tty(true) end,
        {inparallel,
            [
                {<<"Examples from challenge definition">>,
                    fun() ->
                        InputJson = <<"{\"tasks\":[{\"command\":\"touch /tmp/file1\",\"name\":\"task-1\"},{\"command\":\"cat /tmp/file1\",\"name\":\"task-2\",\"requires\":[\"task-3\"]},{\"command\":\"echo 'Hello World!' > /tmp/file1\",\"name\":\"task-3\",\"requires\":[\"task-1\"]},{\"command\":\"rm /tmp/file1\",\"name\":\"task-4\",\"requires\":[\"task-2\",\"task-3\"]}]}">>,
                        ExpectedJson = <<"{\"tasks\":[{\"command\":\"touch /tmp/file1\",\"name\":\"task-1\"},{\"command\":\"echo 'Hello World!' > /tmp/file1\",\"name\":\"task-3\"},{\"command\":\"cat /tmp/file1\",\"name\":\"task-2\"},{\"command\":\"rm /tmp/file1\",\"name\":\"task-4\"}]}">>,
                        ExpectedJsonBashScript = <<"#!/usr/bin/env bash\ntouch /tmp/file1\necho 'Hello World!' > /tmp/file1\ncat /tmp/file1\nrm /tmp/file1\n">>,
                        ?assertEqual({ok, ExpectedJson}, ?TESTMODULE:process_tasks(InputJson, sorted)),
                        ?assertEqual({ok, ExpectedJsonBashScript}, ?TESTMODULE:process_tasks(InputJson, bash_script))
                    end
                }
            ]
        }
    }.

