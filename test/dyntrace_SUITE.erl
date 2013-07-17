%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Tests for dynamic tracing.
%%%      The test cases check:
%%%        * VM probes correctness,
%%%        * Generating simple scripts for dtrace/systemtap.
%%%
%%% @end
%%% Created : 4 Jul 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(dyntrace_SUITE).
-include_lib("test_server/include/test_server.hrl").

-compile(export_all).

-define(msg1, "message").
-define(msg2, "other message").
-define(msize(Msg), integer_to_list(erts_debug:flat_size(Msg))).
-define(p2l(Pid), pid_to_list(Pid)).

suite() ->
    [{timetrap, {minutes, 1}}].

all() ->
    case erlang:system_info(dynamic_trace) of
        none ->
            {skip, "No dynamic trace in this run-time system"};
        _ ->
            [{group, local},
             {group, dist}]
    end.

groups() ->
    [{local, [],
      [process_spawn_exit_test,
       process_scheduling_test,
       message_test,
       message_self_test]},
     {dist, [],
      [message_dist_test]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(dist, Config) ->
    {ok, Host} = inet:gethostname(),
    {ok, Slave} = slave:start(list_to_atom(Host), ct_slave),
    [{slave, Slave} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(dist, Config) ->
    Slave = ?config(slave, Config),
    ok = slave:stop(Slave);
end_per_group(_, _Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%%%-------------------------------------------------------------------
%%% Test cases
%%%-------------------------------------------------------------------

process_spawn_exit_test(_Config) ->
    {Pid, Output} = dyntrace_util:trace(process_spawn_exit_script(),
                                        fun process_spawn_exit_scenario/0),
    TermOutput = [termify_line(L) || L <- Output],
    PidStr = ?p2l(Pid),
    FiltOutput = [L || L <- TermOutput, element(2, L) =:= PidStr],
    ct:log("trace output:~n~p~n", [FiltOutput]),
    [{spawn, PidStr, "erlang:apply/2"},
     {exit, PidStr, "normal"}] = FiltOutput,
    ok.

process_scheduling_test(_Config) ->
    {Pid, Output} = dyntrace_util:trace(process_scheduling_script(),
                                        fun process_scheduling_scenario/0),
    TermOutput = [termify_line(L) || L <- Output],
    PidStr = ?p2l(Pid),
    FiltOutput = [L || L <- TermOutput, element(2, L) =:= PidStr],
    ct:log("trace output:~n~p~n", [FiltOutput]),
    [{schedule, PidStr},
     {hibernate, PidStr, "dyntrace_SUITE:process_scheduling_f/0"},
     {unschedule, PidStr},
     {schedule, PidStr},
     {exit, PidStr},
     {unschedule, PidStr}] = FiltOutput,
    ok.

message_test(Config) ->
    do_message_test(Config,
                    fun message_scenario/1, fun check_local_message/3).

message_self_test(Config) ->
    do_message_test(Config,
                    fun message_self_scenario/1, fun check_local_message/3).

message_dist_test(Config) ->
    do_message_test(Config,
                    fun message_dist_scenario/1, fun check_dist_message/3).

%%%-------------------------------------------------------------------
%%% Test helpers
%%%-------------------------------------------------------------------

process_spawn_exit_scenario() ->
    {Pid, Ref} = spawn_monitor(fun() ->
                                       receive quit -> ok end
                               end),
    Pid ! quit,
    receive
        {'DOWN', Ref, process, Pid, normal} -> Pid
    end.

process_scheduling_scenario() ->
    {Pid, Ref} = spawn_monitor(fun process_scheduling_f/0),
    Pid ! hibernate,
    Pid ! quit,
    receive
        {'DOWN', Ref, process, Pid, normal} -> Pid
    end.

process_scheduling_f() ->
    receive
        hibernate ->
            erlang:hibernate(?MODULE, process_scheduling_f, []);
        quit ->
            ok
    end.

do_message_test(Config, TestF, CheckF) ->
    {{Sender, Receiver}, Output} =
        dyntrace_util:trace(message_script(), fun() -> TestF(Config) end),
    TermOutput = [termify_line(L) || L <- Output],
    CheckF(Sender, Receiver, TermOutput),
    ok.

message_scenario(_Config) ->
    {Receiver, Ref} = spawn_monitor(fun() ->
                                            receive ?msg1 -> ok end
                                    end),
    Sender = spawn(fun() ->
                           Receiver ! ?msg2,
                           Receiver ! ?msg1
                   end),
    receive
        {'DOWN', Ref, process, Receiver, normal} -> {Sender, Receiver}
    end.

message_self_scenario(_Config) ->
    {Receiver, Ref} = spawn_monitor(fun() ->
                                            self() ! ?msg2,
                                            self() ! ?msg1,
                                            receive ?msg1 -> ok end
                                    end),
    receive
        {'DOWN', Ref, process, Receiver, normal} -> {Receiver, Receiver}
    end.

message_dist_scenario(Config) ->
    {Receiver, Ref} = spawn_monitor(fun() ->
                                            receive ?msg1 -> ok end
                                    end),
    Sender = spawn(?config(slave, Config), slave, relay, [Receiver]),
    Sender ! ?msg2,
    Sender ! ?msg1,
    receive
        {'DOWN', Ref, process, Receiver, normal} -> {Sender, Receiver}
    end.

check_local_message(Sender, Receiver, TermOutput) ->
    {SenderStr, ReceiverStr} = {?p2l(Sender), ?p2l(Receiver)},
    {Msg1Size, Msg2Size} = {?msize(?msg1), ?msize(?msg2)},
    FiltOutput = [L || L <- TermOutput, element(2, L) =:= SenderStr
                           orelse element(2, L) =:= ReceiverStr],
    {Msg1Size, Msg2Size} = {?msize(?msg1), ?msize(?msg2)},
    ct:log("trace output:~n~p~n", [FiltOutput]),
    [{sent, SenderStr, ReceiverStr, Msg2Size},
     {queued, ReceiverStr, Msg2Size, "1"},
     {sent, SenderStr, ReceiverStr, Msg1Size},
     {queued, ReceiverStr, Msg1Size, "2"},
     {received, ReceiverStr, Msg1Size, "1"}] = FiltOutput.

check_dist_message(Sender, Receiver, TermOutput) ->
    {SenderStr, ReceiverStr} = {?p2l(Sender), ?p2l(Receiver)},
    FiltOutput = [L || L <- TermOutput, element(2, L) =:= SenderStr
                           orelse element(2, L) =:= ReceiverStr],
    ct:log("trace output:~n~p~n", [FiltOutput]),
    [{queued, ReceiverStr, _, "1"},
     {queued, ReceiverStr, _, "2"},
     {received, ReceiverStr, _, "1"}] = FiltOutput.

termify_line(L) ->
    [H|T] = re:split(L, " ", [{return,list}]),
    list_to_tuple([list_to_atom(H)|T]).

%%%-------------------------------------------------------------------
%%% Dyntrace scripts
%%%-------------------------------------------------------------------

process_spawn_exit_script() ->
    [{probe, ['begin'], [],
      [{printf, "\n", []}]},
     {probe, ["process-spawn"], [],
      [{printf, "spawn %s %s\n", [{arg_str,1}, {arg_str,2}]}]},
     {probe, ["process-exit"], [],
      [{printf, "exit %s %s\n", [{arg_str,1}, {arg_str,2}]}]}
    ].

process_scheduling_script() ->
    [{probe, ['begin'], [],
      [{printf, "\n", []}]},
     {probe, ["process-scheduled"], [],
      [{printf, "schedule %s\n", [{arg_str,1}]}]},
     {probe, ["process-unscheduled"], [],
      [{printf, "unschedule %s\n", [{arg_str,1}]}]},
     {probe, ["process-hibernate"], [],
      [{printf, "hibernate %s %s\n", [{arg_str,1}, {arg_str,2}]}]},
     {probe, ["process-exit"], [],
      [{printf, "exit %s\n", [{arg_str,1}]}]}
    ].

message_script() ->
    [{probe, ['begin'], [],
      [{printf, "\n", []}]},
     {probe, ["message-send"], [],
      [{printf, "sent %s %s %d\n", [{arg_str,1}, {arg_str,2}, {arg,3}]}]},
     {probe, ["message-queued"], [],
      [{printf, "queued %s %d %d\n", [{arg_str,1}, {arg,2}, {arg,3}]}]},
     {probe, ["message-receive"], [],
      [{printf, "received %s %d %d\n", [{arg_str,1}, {arg,2}, {arg,3}]}]}
    ].
