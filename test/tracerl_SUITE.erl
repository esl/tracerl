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
-module(tracerl_SUITE).

-include("tracerl_test.hrl").
-include("tracerl_util.hrl").
-include_lib("test_server/include/test_server.hrl").

-compile(export_all).

-import(tracerl_test_util, [all_if_dyntrace/1,
                            start_trace/1,
                            send_all/1, receive_all/1]).

-define(msg1, "message").
-define(msg2, "other message").

suite() ->
    [{timetrap, {minutes, 1}}].

all() ->
    all_if_dyntrace(
      [{group, local},
       {group, dist}]).

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
    case whereis(tracerl_process) of
        undefined -> ok;
        DP        -> tracerl_process:stop(DP)
    end.

%%%-------------------------------------------------------------------
%%% Test cases
%%%-------------------------------------------------------------------

process_spawn_exit_test(_Config) ->
    DP = start_trace(process_spawn_exit_script()),
    ?wait_for({line, ["start"]}),
    Pid = process_spawn_exit_scenario(),
    tracerl_process:stop(DP),
    ?wait_for({line, ["spawn", Pid, "erlang:apply/2"]}),
    ?wait_for({line, ["exit", Pid, "normal"]}),
    ?wait_for(eof),
    ok.

process_scheduling_test(_Config) ->
    DP = start_trace(process_scheduling_script()),
    ?wait_for({line, ["start"]}),
    Pid = process_scheduling_scenario(),
    tracerl_process:stop(DP),
    ?wait_for({line, ["schedule", Pid]}),
    ?wait_for({line, ["hibernate", Pid,
                      "tracerl_SUITE:process_scheduling_f/0"]}),
    ?wait_for({line, ["unschedule", Pid]}),
    ?wait_for({line, ["schedule", Pid]}),
    ?wait_for({line, ["exit", Pid]}),
    ?wait_for({line, ["unschedule", Pid]}),
    ?wait_for(eof),
    ok.

message_test(_Config) ->
    Sender = spawn(fun() -> send_all([?msg2, ?msg1]) end),
    {Receiver, Ref} = spawn_monitor(fun() -> receive_all([?msg1]) end),
    DP = start_trace(message_script(Receiver)),
    ?wait_for({line, ["start"]}),
    Sender ! {start, Receiver},
    receive {'DOWN', Ref, process, Receiver, normal} -> ok end,
    tracerl_process:stop(DP),
    check_local_message(Sender, Receiver).

message_self_test(_Config) ->
    {Pid, Ref} = spawn_monitor(fun() ->
                                       send_all([?msg2, ?msg1]),
                                       receive_all([?msg1])
                               end),
    DP = start_trace(message_script(Pid)),
    ?wait_for({line, ["start"]}),
    Pid ! {start, Pid},
    receive {'DOWN', Ref, process, Pid, normal} -> ok end,
    tracerl_process:stop(DP),
    check_local_message(Pid, Pid).

message_dist_test(Config) ->
    {Receiver, Ref} = spawn_monitor(fun() -> receive_all([?msg1]) end),
    Sender = spawn(?config(slave, Config), slave, relay, [Receiver]),
    DP = start_trace(message_script(Receiver)),
    ?wait_for({line, ["start"]}),
    Sender ! ?msg2,
    Sender ! ?msg1,
    receive {'DOWN', Ref, process, Receiver, normal} -> ok end,
    tracerl_process:stop(DP),
    check_dist_message(Sender, Receiver).

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

check_local_message(Sender, Receiver) ->
    {Size1, Size2} = {?msize(?msg1), ?msize(?msg2)},
    ?wait_for({line, ["sent", Sender, Receiver, Size2]}),
    ?wait_for({line, ["queued", Receiver, Size2, 1]}),
    ?wait_for({line, ["sent", Sender, Receiver, Size1]}),
    ?wait_for({line, ["queued", Receiver, Size1, 2]}),
    ?wait_for({line, ["received", Receiver, Size1, 1]}),
    ?wait_for(eof).

check_dist_message(_Sender, Receiver) ->
    ?wait_for({line, ["queued", Receiver, _, 1]}),
    ?wait_for({line, ["queued", Receiver, _, 2]}),
    ?wait_for({line, ["received", Receiver, _, 1]}),
    ?wait_for(eof).

%%%-------------------------------------------------------------------
%%% Dyntrace scripts
%%%-------------------------------------------------------------------

process_spawn_exit_script() ->
    [{probe, 'begin',
      [{printf, "start\n"}]},
     {probe, "process-spawn",
      [{printf, "spawn %s %s\n", [pid, mfa]}]},
     {probe, "process-exit",
      [{printf, "exit %s %s\n", [pid, reason]}]}
    ].

process_scheduling_script() ->
    [{probe, 'begin',
      [{printf, "start\n"}]},
     {probe, "process-scheduled",
      [{printf, "schedule %s\n", [pid]}]},
     {probe, "process-unscheduled",
      [{printf, "unschedule %s\n", [pid]}]},
     {probe, "process-hibernate",
      [{printf, "hibernate %s %s\n", [pid, mfa]}]},
     {probe, "process-exit",
      [{printf, "exit %s\n", [pid]}]}
    ].

message_script(Receiver) ->
    [{probe, 'begin',
      [{printf, "start\n"}]},
     {probe, "message-send", {'==', receiver_pid, Receiver},
      [{printf, "sent %s %s %d\n", [sender_pid, receiver_pid, size]}]},
     {probe, "message-queued", {'==', pid, Receiver},
      [{printf, "queued %s %d %d\n", [pid, size, queue_length]}]},
     {probe, "message-receive", {'==', pid, Receiver},
      [{printf, "received %s %d %d\n", [pid, size, queue_length]}]}
    ].
