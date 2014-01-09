%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Script generation utilities
%%%
%%% @end
%%% Created : 26 Jul 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(tracerl_gen_util).

-include("tracerl_util.hrl").

-compile(export_all).

sep(Args, Sep) ->
    sep_f(Args, Sep, fun(Arg) -> Arg end).

sep_t(Tag, Args, Sep) ->
    sep_f(Args, Sep, fun(Arg) -> {Tag, Arg} end).

sep_t(BeforeTag, AfterTag, Args, Sep) ->
    sep_f(Args, Sep, fun(Arg) -> {BeforeTag, AfterTag, Arg} end).

tag(Tag, Args) ->
    [{Tag, Arg} || Arg <- Args].

tag(PreTag, PostTag, Args) ->
    [{PreTag, PostTag, Arg} || Arg <- Args].

sep_f([A, B | T], Sep, F) -> [F(A), Sep | sep_f([B|T], Sep, F)];
sep_f([H], _Sep, F)       -> [F(H)];
sep_f([], _Sep, _F)       -> [].

insert_args(Point, State) ->
    State#gen_state{args = orddict:from_list(args(Point))}.

args("process-unscheduled") ->
    [{pid, {arg_str, 1}}];
args(Point) when Point == "process-spawn";
                 Point == "process-scheduled";
                 Point == "process-hibernate" ->
    [{pid, {arg_str, 1}},
     {mfa, {arg_str, 2}}];
args("process-exit") ->
    [{pid, {arg_str, 1}},
     {reason, {arg_str, 2}}];
args("process-exit_signal") ->
    [{sender_pid, {arg_str, 1}},
     {receiver_pid, {arg_str, 2}},
     {reason, {arg_str, 3}}];
args("process-exit_signal-remote") ->
    [{sender_pid, {arg_str, 1}},
     {node, {arg_str, 2}},
     {receiver_pid, {arg_str, 3}},
     {reason, {arg_str, 4}}];
args("message-send") ->
    [{sender_pid, {arg_str, 1}},
     {receiver_pid, {arg_str, 2}},
     {size, {arg, 3}}];
args("message-send-remote") ->
    [{sender_pid, {arg_str, 1}},
     {node, {arg_str, 2}},
     {receiver_pid, {arg_str, 3}},
     {size, {arg, 4}}];
args(Point) when Point == "message-queued";
                 Point == "message-receive" ->
    [{pid, {arg_str, 1}},
     {size, {arg, 2}},
     {queue_length, {arg, 3}}];
args("copy-struct") ->
    [{size, {arg, 1}}];
args("copy-object") ->
    [{pid, {arg_str, 1}},
     {size, {arg, 2}}];
args(Point) when Point == "local-function-entry";
                 Point == "global-function-entry";
                 Point == "function-return" ->
    [{pid, {arg_str, 1}},
     {mfa, {arg_str, 2}},
     {depth, {arg, 3}}];
args(Point) when Point == "bif-entry";
                 Point == "bif-return" ->
    [{pid, {arg_str, 1}},
     {mfa, {arg_str, 2}}];
args("driver-init") ->
    [{name, {arg_str, 1}},
     {major, {arg, 2}},
     {minor, {arg, 3}},
     {flags, {arg, 4}}];
args(Point) when Point == "driver-start";
                 Point == "driver-stop" ->
    [{pid, {arg_str, 1}},
     {name, {arg_str, 2}},
     {port, {arg_str, 3}}];
args(Point) when Point == "driver-finish";
                 Point == "driver-stop_select" ->
    [{name, {arg_str, 1}}];
args(Point) when Point == "driver-flush";
                 Point == "driver-event";
                 Point == "driver-ready_input";
                 Point == "driver-ready_output";
                 Point == "driver-timeout";
                 Point == "driver-ready_async";
                 Point == "driver-process_exit" ->
    [{pid, {arg_str, 1}},
     {port, {arg_str, 2}},
     {name, {arg_str, 3}}];
args(Point) when Point == "driver-output";
                 Point == "driver-outputv" ->
    [{pid, {arg_str, 1}},
     {port, {arg_str, 2}},
     {name, {arg_str, 3}},
     {bytes, {arg, 4}}];
args(Point) when Point == "driver-control";
                 Point == "driver-call" ->
    [{pid, {arg_str, 1}},
     {port, {arg_str, 2}},
     {name, {arg_str, 3}},
     {command, {arg, 4}},
     {bytes, {arg, 5}}];
args("user_trace-" ++ _) ->
    [{pid, {arg_str, 1}},
     {tag, {arg_str, 2, ""}},
     {i1, {arg, 3}},
     {i2, {arg, 4}},
     {i3, {arg, 5}},
     {i4, {arg, 6}},
     {s1, {arg_str, 7, ""}},
     {s2, {arg_str, 8, ""}},
     {s3, {arg_str, 9, ""}},
     {s4, {arg_str, 10, ""}}];
args(_) ->
    [].
