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
                            start_trace/1, start_trace/2,
                            send_all/1, receive_all/1]).

-define(msg1, "message").
-define(msg2, "other message").

suite() ->
    [{timetrap, {minutes, 1}}].

all() ->
    all_if_dyntrace(
      [{group, local},
       {group, dist}]).

configure() ->
    [{require, dbg}].

groups() ->
    [{local, [],
      [process_spawn_exit_test,
       process_scheduling_test,
       message_test,
       message_self_test,
       user_trace_test,
       user_trace_n_test]},
     {dist, [],
      [message_dist_test]}].

init_per_suite(Config) ->
    case ct:get_config(dbg) of
        []    -> ok;
        [_|_] -> dbg:tracer()
    end,
    Config.

end_per_suite(_Config) ->
    case ct:get_config(dbg) of
        []    -> ok;
        [_|_] -> dbg:stop_clear()
    end,
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
    case lists:member(message, ct:get_config(dbg)) of
        true  -> dbg:p(new);
        false -> ok
    end,
    Config.

end_per_testcase(_Case, _Config) ->
    case lists:member(message, ct:get_config(dbg)) of
        true  -> dbg:p(clear);
        false -> ok
    end.

%%%-------------------------------------------------------------------
%%% Test cases
%%%-------------------------------------------------------------------

process_spawn_exit_test(_Config) ->
    DP = start_trace(process_spawn_exit_script()),
    ?wait_for({line, ["start"]}),
    Pid = process_spawn_exit_scenario(),
    tracerl:stop(DP),
    ?wait_for({line, ["spawn", Pid, "erlang:apply/2"]}),
    ?wait_for({line, ["exit", Pid, "normal"]}),
    ?wait_for(eof),
    ok.

process_scheduling_test(_Config) ->
    DP = start_trace(process_scheduling_script()),
    ?wait_for({line, ["start"]}),
    Pid = process_scheduling_scenario(),
    tracerl:stop(DP),
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
    DP = start_trace(message_script([Receiver])),
    ?wait_for({line, ["start"]}),
    Sender ! {start, Receiver},
    receive {'DOWN', Ref, process, Receiver, normal} -> ok end,
    tracerl:stop(DP),
    check_local_message(Sender, Receiver).

message_self_test(_Config) ->
    {Pid, Ref} = spawn_monitor(fun() ->
                                       send_all([?msg2, ?msg1]),
                                       receive_all([?msg1])
                               end),
    DP = start_trace(message_script([Pid])),
    ?wait_for({line, ["start"]}),
    Pid ! {start, Pid},
    receive {'DOWN', Ref, process, Pid, normal} -> ok end,
    tracerl:stop(DP),
    check_self_message(Pid).

message_dist_test(Config) ->
    {Receiver, Ref} = spawn_monitor(fun() -> receive_all([?msg1]) end),
    Slave = ?config(slave, Config),
    Sender = spawn(Slave, slave, relay, [Receiver]),
    DP = start_trace(message_script([Sender, Receiver])),
    SlaveDP = start_trace(message_script([Sender, Receiver], "slave-"), Slave),
    {Size1, Size2} = {?msize(?msg1), ?msize(?msg2)},
    Self = self(),
    [SlaveStr, NodeStr] = ["'"++L++"'" || L <- [?a2l(Slave), ?a2l(node())]],
    ?wait_for({line, ["start"]}),
    ?wait_for({line, ["slave-start"]}),

    Sender ! ?msg2,
    ?wait_for({line, ["slave-sent-remote", Sender, _, Receiver, Size2]}),
    ?expect({line, ["slave-sent", Sender, Receiver, Size2]}),
    ?expect({line, ["slave-queued", Sender, _, 1]}),
    ?expect({line, ["slave-received", Sender, Size2, 0]}),
    ?wait_for({line, ["queued", Receiver, _, 1]}),
    ?expect({line, ["sent", Self, Sender, Size2]}),
    ?expect({line, ["sent-remote", Self, SlaveStr, Sender, Size2]}),

    Sender ! ?msg1,
    ?wait_for({line, ["slave-sent-remote", Sender, NodeStr, Receiver, Size1]}),
    ?expect({line, ["slave-sent", Sender, Receiver, Size1]}),
    ?expect({line, ["slave-queued", Sender, _, 1]}),
    ?expect({line, ["slave-received", Sender, Size1, 0]}),
    ?wait_for({line, ["received", Receiver, Size1, 1]}),
    ?expect({line, ["queued", Receiver, _, 2]}),
    ?expect({line, ["sent", Self, Sender, Size1]}),
    ?expect({line, ["sent-remote", Self, SlaveStr, Sender, Size1]}),

    receive {'DOWN', Ref, process, _, normal} -> ok end,
    tracerl:stop(DP),
    tracerl:stop(SlaveDP),
    ?wait_for(eof),
    ?wait_for(eof),
    ?expect_not({line, _}).

user_trace_test(_Config) ->
    DP = start_trace(user_trace_script()),
    ?wait_for({line, ["start"]}),
    dyntrace:p(1, 2, 3, "my", "probe"),
    dyntrace:put_tag("tag123"),
    dyntrace:p("yet", "another", "probe"),
    tracerl:stop(DP),
    Self = self(),
    ?wait_for(eof),
    ?expect({line, [Self, "", 1, 2, 3, 0, "my", "probe", "", ""]}),
    ?expect({line, [Self, "tag123", 0, 0, 0, 0,
                    "yet", "another", "probe", ""]}),
    ?expect_not({line, _}).

user_trace_n_test(_Config) ->
    DP = start_trace(user_trace_n_script()),
    ?wait_for({line, ["start"]}),
    dyntrace:pn(1, 1, 2, 3, "my", "probe"),
    dyntrace:put_tag("tag123"),
    dyntrace:pn(777, "yet", "another", "probe"),
    tracerl:stop(DP),
    Self = self(),
    ?wait_for(eof),
    ?expect({line, [Self, "", 1, 2, 3, "my", "probe"]}),
    ?expect({line, [Self, "tag123", "yet", "another", "probe"]}),
    ?expect_not({line, _}).

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

check_self_message(Pid) ->
    Size = ?msize({start, Pid}),
    ?wait_for({line, ["sent", _, Pid, Size]}),
    ?wait_for({line, ["queued", Pid, Size, 1]}),
    ?wait_for({line, ["received", Pid, Size, 0]}),
    check_local_message(Pid, Pid).

check_local_message(Sender, Receiver) ->
    {Size1, Size2} = {?msize(?msg1), ?msize(?msg2)},
    ?wait_for({line, ["sent", Sender, Receiver, Size2]}),
    ?wait_for({line, ["queued", Receiver, Size2, 1]}),
    ?wait_for({line, ["sent", Sender, Receiver, Size1]}),
    ?wait_for({line, ["queued", Receiver, Size1, 2]}),
    ?wait_for({line, ["received", Receiver, Size1, 1]}),
    ?wait_for(eof),
    ?expect_not({line, _}).

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

message_script(Receivers) ->
    message_script(Receivers, "").

message_script(Receivers, Tag) ->
    [{probe, 'begin',
      [{printf, Tag ++ "start\n"}]},
     {probe, "message-send", pid_pred(receiver_pid, Receivers),
      [{printf, Tag ++ "sent %s %s %d\n", [sender_pid, receiver_pid, size]}]},
     {probe, "message-send-remote", pid_pred(receiver_pid, Receivers),
      [{printf, Tag ++ "sent-remote %s %s %s %d\n",
        [sender_pid, node, receiver_pid, size]}]},
     {probe, "message-queued", pid_pred(pid, Receivers),
      [{printf, Tag ++ "queued %s %d %d\n", [pid, size, queue_length]}]},
     {probe, "message-receive", pid_pred(pid, Receivers),
      [{printf, Tag ++ "received %s %d %d\n", [pid, size, queue_length]}]}
    ].

pid_pred(Name, Pids) ->
    {'||', [{'==', Name, Pid} || Pid <- Pids]}.

user_trace_script() ->
    [{probe, 'begin',
      [{printf, "start\n"}]},
     {probe, "user_trace-i4s4",
      [{printf, "%s %s %d %d %d %d %s %s %s %s\n",
        [pid, tag, i1, i2, i3, i4, s1, s2, s3, s4]}]}
    ].

user_trace_n_script() ->
    [{probe, 'begin',
      [{printf, "start\n"}]},
     {probe, "user_trace-n1",
      [{printf, "%s %s %d %d %d %s %s\n",
        [pid, tag, i1, i2, i3, s1, s2]}]},
     {probe, "user_trace-n777",
      [{printf, "%s %s %s %s %s\n",
        [pid, tag, s1, s2, s3]}]}
    ].
