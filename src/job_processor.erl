-module(job_processor).

-define(NOTEST, true).
-ifdef(TEST).
    -compile(export_all).
    -compile(nowarn_export_all).
-endif.

-type task_name()   :: binary().
-type reply_mode()  :: sorted |  bash_script.

-type task()        :: #{
                        name := task_name(),
                        command := binary(),
                        requires => [task_name()]
                    }.

-export([process_tasks/2]).

-export_type([reply_mode/0]).

-spec process_tasks(JsonAsBinary, Mode) -> Result when
    JsonAsBinary    :: binary(),
    Mode            :: reply_mode(),
    Result          :: {ok, binary()} | {error, term()}.

process_tasks(JsonAsBinary, Mode) ->
    case thoas:decode(JsonAsBinary, #{keys => to_existing_atom}) of
        {ok, #{tasks := Tasks}} ->
            produce_output(Mode, sort_tasks(Tasks));
        {error, Reason} ->
            {error, Reason}
    end.

% =========================== Private helpers ==================================

-spec produce_output(Mode, SortedTasks) -> Result when
    Mode        :: reply_mode(),
    SortedTasks :: [task()],
    Result      :: {ok, binary()} | {error, term()}.

produce_output(sorted, SortedTasks) when is_list(SortedTasks) ->
    {ok, thoas:encode(#{tasks => SortedTasks})};

produce_output(bash_script, SortedTasks) when is_list(SortedTasks) ->
    {ok, lists:foldl(fun(#{command := Command}, Acc) ->
        <<Acc/binary, Command/binary, "\n">>
    end, <<"#!/usr/bin/env bash\n">>, SortedTasks)};

produce_output(_ReplyMode, {error, _Reason} = Error) -> Error.

-spec sort_tasks(Tasks) -> Result when
    Tasks       :: [task()],
    Result      :: [task()] | {error, term()}.

sort_tasks(Tasks) ->
    KeyedTasks = keyed(Tasks),
    try
        {Sorted, _} = lists:foldl(fun
            (T, {Acc, Seen}) ->
                sort(T, KeyedTasks, Acc, Seen, [])
            end, {[], []}, Tasks),
        lists:reverse(Sorted)
    catch
        throw:Descr ->
            {error, Descr}
    end.

-spec keyed(Tasks) -> Result when
    Tasks       :: [task()],
    Result      :: #{task_name() => task()}.

keyed(Tasks) ->
    lists:foldl(fun (#{name := Name} = Task, Acc) ->
        maps:put(Name, Task, Acc)
    end, #{}, Tasks).

-spec sort(Task, KeyedMap, Acc, Seen, CurrentPath) -> Result when
    Task        :: task(),
    KeyedMap    :: #{task_name() => task()},
    Acc         :: [task()],
    Seen        :: [task_name()],
    CurrentPath :: [task_name()],
    Result      :: {task_name(), [task_name()]}.

sort(#{name := Name} = Task, KeyedMap, Acc, Seen, CurrentPath) ->
    case lists:member(Name, CurrentPath) of
        true ->
            throw({circular_dependency, Name});
        false ->
            case lists:member(Name, Seen) of
                true ->
                    {Acc, Seen};
                false ->
                    {NewAcc, NewSeen} = lists:foldl(fun(Dep, {DAcc, DSeen}) ->
                        case maps:get(Dep, KeyedMap, undefined) of
                            undefined ->
                                throw({wrong_reference_in_required, Dep});
                            DepTask ->
                                sort(DepTask, KeyedMap, DAcc, DSeen, [Name | CurrentPath])
                        end
                    end, {Acc, [Name | Seen]}, maps:get(requires, Task, [])),
                    {[maps:without([requires], Task) | NewAcc], NewSeen}
            end
    end.

% ------------------------ end of Private helpers ------------------------------
