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
                            send_all/1, receive_all/1,
                            test_function/0]).

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
       copy_test,
       function_test,
       driver_test,
       driver_outputv_test,
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
    DP = start_trace(message_script([Receiver])),
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
    DP = start_trace(message_script([Pid])),
    ?wait_for({line, ["start"]}),
    Pid ! {start, Pid},
    receive {'DOWN', Ref, process, Pid, normal} -> ok end,
    tracerl_process:stop(DP),
    check_self_message(Pid).

message_dist_test(Config) ->
    {Receiver, Ref} = spawn_monitor(fun() -> receive_all([?msg1]) end),
    Slave = ?config(slave, Config),
    Sender = spawn(Slave, slave, relay, [Receiver]),
    true = rpc:call(Slave, erlang, register, [relay, Sender]),
    [SlaveStr, NodeStr] = ["'" ++ L ++ "'" || L <- [?a2l(Slave), ?a2l(node())]],
    RelayStr = "{relay," ++ SlaveStr ++ "}",
    DP = start_trace(message_script([Sender, Receiver, RelayStr])),
    ?wait_for({line, ["start"]}),
    SlaveDP = start_trace(message_script([Sender, Receiver], "slave-"), Slave),
    ?wait_for({line, ["slave-start"]}),
    {Size1, Size2} = {?msize(?msg1), ?msize(?msg2)},
    Self = self(),

    Sender ! ?msg2,
    ?wait_for({line, ["slave-sent-remote", Sender, _, Receiver, Size2]}),
    ?expect({line, ["slave-queued", Sender, _, 1]}),
    ?expect({line, ["slave-received", Sender, Size2, 0]}),
    ?wait_for({line, ["queued", Receiver, _, 1]}),
    ?expect({line, ["sent-remote", Self, SlaveStr, Sender, Size2]}),

    {relay, Slave} ! ?msg1,
    ?wait_for({line, ["slave-sent-remote", Sender, NodeStr, Receiver, Size1]}),
    ?expect({line, ["slave-queued", Sender, _, 1]}),
    ?expect({line, ["slave-received", Sender, Size1, 0]}),
    ?wait_for({line, ["received", Receiver, Size1, 1]}),
    ?expect({line, ["queued", Receiver, _, 2]}),
    ?expect({line, ["sent-remote", Self, SlaveStr, RelayStr, Size1]}),

    receive {'DOWN', Ref, process, _, normal} -> ok end,
    tracerl_process:stop(DP),
    tracerl_process:stop(SlaveDP),
    ?wait_for(eof),
    ?wait_for(eof),
    ?expect_not({line, _}).

copy_test(_Config) ->
    L = lists:seq(1, 10000),
    Size = ?msize(L),
    DP = start_trace(copy_script(Size)),
    ?wait_for({line, ["start"]}),
    spawn(fun() -> _A = L end),
    ?wait_for({line, ["struct", _N]}, ?DEFAULT_TIMEOUT, _N >= Size),
    put(data, L),
    Self = self(),
    process_info(Self),
    ?wait_for({line, ["object", Self, _M]}, ?DEFAULT_TIMEOUT, _M >= Size),
    tracerl_process:stop(DP),
    ?wait_for(eof).

function_test(_Config) ->
    {P, Ref} = spawn_monitor(fun() ->
                                     receive start -> ok end,
                                     test_function()
                             end),
    DP = start_trace(function_script([P])),
    ?wait_for({line, ["start"]}),
    P ! start,
    receive {'DOWN', Ref, process, _, normal} -> ok end,
    ok = tracerl_process:stop(DP),
    ?wait_for(eof),
    FunStr = "tracerl_test_util:test_function",
    Str0 = FunStr ++ "/0",
    [Str1, Str2] = [FunStr ++ "_" ++ ?i2l(I) ++ "/" ++ ?i2l(I)|| I <- [1, 2]],
    % Notes:
    %   - return probes show the enclosing (destination) function
    %   - return probes do not work for tail calls
    %       (and some other kinds of calls as well)
    ?expect({line, ["call", "global", P, Str0, 0]}),
    ?expect({line, ["call", "global", P, Str1, 0]}),
    ?expect({line, ["return", P, Str0, 0]}),
    ?expect({line, ["call", "local", P, Str2, 0]}),
    ?expect({line, ["return", P, Str0, 0]}),
    ?expect({line, ["call", "local", P, Str2, 0]}).

driver_test(_Config) ->
    driver_test_scenario("test_drv", "output").

driver_outputv_test(_Config) ->
    driver_test_scenario("test_outputv_drv", "outputv").

user_trace_test(_Config) ->
    DP = start_trace(user_trace_script()),
    ?wait_for({line, ["start"]}),
    dyntrace:p(1, 2, 3, "my", "probe"),
    dyntrace:put_tag("tag123"),
    dyntrace:p("yet", "another", "probe"),
    tracerl_process:stop(DP),
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
    tracerl_process:stop(DP),
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

driver_test_scenario(Name, OutputF) ->
    DP = start_trace(driver_script([Name])),
    ?wait_for({line, ["start"]}),
    ok = test_driver:load(Name),
    ?wait_for({line, ["init", Name, _, _, 0]}),
    Pid = test_driver:start_link(Name, a),
    ?wait_for({line, ["start", Pid, Name, DriverPort]}),
    Port = DriverPort,
    2 = test_driver:foo(a, sync, 1),
    ?wait_for({line, [OutputF, Pid, Port, Name, 3]}),
    2 = test_driver:foo(a, async, 1),
    ?wait_for({line, ["ready_async", Pid, Port, Name]}),
    ?expect({line, [OutputF, Pid, Port, Name, 3]}),
    2 = test_driver:foo(a, control, 1),
    ?wait_for({line, ["control", Pid, Port, Name, 1, 1]}),
    2 = test_driver:foo(a, call, 1),
    ?wait_for({line, ["call", Pid, Port, Name, 1, 3]}),
    test_driver:stop(a),
    ?wait_for({line, ["stop", Pid, Name, Port]}),
    ok = test_driver:unload(Name),
    ?wait_for({line, ["finish", Name]}),
    ok = tracerl_process:stop(DP),
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

copy_script(MinSize) ->
    [{probe, 'begin',
      [{printf, "start\n"}]},
     {probe, "copy-struct", {'>=', size, MinSize},
      [{printf, "struct %d\n", [size]}]},
     {probe, "copy-object", {'>=', size, MinSize},
      [{printf, "object %s %d\n", [pid, size]}]}
    ].

function_script(Pids) ->
    [{probe, 'begin',
      [{printf, "start\n"}]},
     {probe, "local-function-entry", pid_pred(pid, Pids),
      [{printf, "call local %s %s %d\n", [pid, mfa, depth]}]},
     {probe, "global-function-entry", pid_pred(pid, Pids),
      [{printf, "call global %s %s %d\n", [pid, mfa, depth]}]},
     {probe, "function-return", pid_pred(pid, Pids),
      [{printf, "return %s %s %d\n", [pid, mfa, depth]}]}
    ].

pid_pred(Name, Pids) ->
    {'||', [{'==', Name, Pid} || Pid <- Pids]}.

driver_script(Names) ->
    [{probe, 'begin',
      [{printf, "start\n"}]},
     {probe, "driver-init", name_pred(Names),
      [{printf, "init %s %d %d %d\n", [name, major, minor, flags]}]},
     {probe, "driver-start", name_pred(Names),
      [{printf, "start %s %s %s\n", [pid, name, port]}]},
     {probe, "driver-stop", name_pred(Names),
      [{printf, "stop %s %s %s\n", [pid, name, port]}]},
     {probe, "driver-finish", name_pred(Names),
      [{printf, "finish %s\n", [name]}]},
     {probe, "driver-flush", name_pred(Names), %% untested
      [{printf, "flush %s %s %s\n", [pid, port, name]}]},
     {probe, "driver-output", name_pred(Names),
      [{printf, "output %s %s %s %d\n", [pid, port, name, bytes]}]},
     {probe, "driver-outputv", name_pred(Names),
      [{printf, "outputv %s %s %s %d\n", [pid, port, name, bytes]}]},
     {probe, "driver-control", name_pred(Names),
      [{printf, "control %s %s %s %d %d\n",
        [pid, port, name, command, bytes]}]},
     {probe, "driver-call", name_pred(Names),
      [{printf, "call %s %s %s %d %d\n",
        [pid, port, name, command, bytes]}]},
     {probe, "driver-event", name_pred(Names), %% untested
      [{printf, "event %s %s %s\n", [pid, port, name]}]},
     {probe, "driver-ready_input", name_pred(Names), %% untested
      [{printf, "ready_input %s %s %s\n", [pid, port, name]}]},
     {probe, "driver-ready_output", name_pred(Names), %% untested
      [{printf, "ready_output %s %s %s\n", [pid, port, name]}]},
     {probe, "driver-timeout", name_pred(Names), %% untested
      [{printf, "timeout %s %s %s\n", [pid, port, name]}]},
     {probe, "driver-ready_async", name_pred(Names),
      [{printf, "ready_async %s %s %s\n", [pid, port, name]}]},
     {probe, "driver-process_exit", name_pred(Names), %% untested
      [{printf, "process_exit %s %s %s\n", [pid, port, name]}]},
     {probe, "driver-stop_select", name_pred(Names), %% untested
      [{printf, "stop_select %s\n", [name]}]}
    ].

name_pred(Names) ->
    {'||', [{'==', name, Name} || Name <- Names]}.

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
