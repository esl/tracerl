%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Tests for generating dtrace/systemtap scripts.
%%%
%%% @end
%%% Created : 16 Jul 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(tracerl_gen_SUITE).

-include("tracerl_test.hrl").
-include("tracerl_util.hrl").
-include_lib("test_server/include/test_server.hrl").

-compile(export_all).

-import(tracerl_test_util,
        [all_if_dyntrace/1, start_trace/1, start_term_trace/1,
         send_n/2, receive_n/2, send_all/1, receive_all/1,
         ring_start/1, ring_stop/1, ring_send/3]).

suite() ->
    [{timetrap, {minutes, 1}}].

all() ->
    all_if_dyntrace(
      [begin_tick_end_test,
       variable_test,
       associative_array_test,
       count_messages_by_sender_test,
       count_messages_by_sender_with_reset_test,
       count_messages_by_sender_and_receiver_test,
       count_messages_by_sender_and_receiver_term_test,
       count_messages_up_down_test,
       sender_and_receiver_set_test,
       message_receive_stats_test]).

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%%-------------------------------------------------------------------
%%% Test cases
%%%-------------------------------------------------------------------
begin_tick_end_test(_Config) ->
    DP = start_trace(begin_tick_end_script()),
    ?wait_for({line, ["start"]}),
    ?wait_for({line, ["ticked"]}),
    ?wait_for({line, ["ticked"]}),
    ?wait_for({line, ["ticked"]}),
    tracerl_process:stop(DP),
    ?wait_for(eof),
    ?expect({line, ["finish"]}).

variable_test(_Config) ->
    Messages = ["aa", "bbbb"],
    {Receiver, Ref} = spawn_monitor(fun() -> receive_all(Messages) end),
    Sender = spawn(fun() -> send_all(Messages) end),
    DP = start_trace(variable_script(Sender)),
    ?wait_for({line, ["start"]}),
    Sender ! {start, Receiver},
    receive {'DOWN', Ref, process, Receiver, normal} -> ok end,
    [Size1, Size2] = [?msize(M) || M <- Messages],
    TotalSize = Size1 + Size2,
    ?wait_for({line, ["sent", Sender, Receiver, Size1]}),
    ?wait_for({line, ["sent", Sender, Receiver, Size2]}),
    tracerl_process:stop(DP),
    ?wait_for(eof),
    ?expect({line, ["last", Sender, Receiver, Size2]}),
    ?expect({line, ["total", "num", 2, "size", TotalSize]}).

associative_array_test(_Config) ->
    Messages = ["aa", "bbbb"],
    {Receiver, Ref} = spawn_monitor(fun() -> receive_all(Messages) end),
    Sender = spawn(fun() -> send_all(Messages) end),
    DP = start_trace(associative_array_script(Sender)),
    ?wait_for({line, ["start"]}),
    Sender ! {start, Receiver},
    receive {'DOWN', Ref, process, Receiver, normal} -> ok end,
    tracerl_process:stop(DP),
    ?wait_for(eof),
    TotalSize = lists:sum([?msize(M) || M <- Messages]),
    ?expect({line, ["total", "num", 2, "size", TotalSize]}).

count_messages_by_sender_test(_Config) ->
    {Receiver, Ref} = spawn_monitor(fun() -> receive_n(1, 30) end),
    Sender1 = spawn(fun() -> send_n(1, 10) end),
    Sender2 = spawn(fun() -> send_n(11, 30) end),
    DP = start_trace(count_messages_by_sender_script([Sender1, Sender2])),
    ?wait_for({line, ["start"]}),
    Sender1 ! {start, Receiver},
    Sender2 ! {start, Receiver},
    receive {'DOWN', Ref, process, Receiver, normal} -> ok end,
    tracerl_process:stop(DP),
    ?wait_for(eof),
    ?expect({line, ["sent", 10, "from", Sender1]}),
    ?expect({line, ["sent", 20, "from", Sender2]}).

count_messages_by_sender_with_reset_test(_Config) ->
    {Receiver, Ref} = spawn_monitor(fun() -> receive_n(1, 30) end),
    Sender1 = spawn(fun() -> send_n(1, 5) end),
    Sender2 = spawn(fun() -> send_n(6, 20), send_n(21, 30) end),
    DP = start_trace(
           count_messages_by_sender_with_reset_script([Sender1, Sender2])),
    ?wait_for({line, ["start"]}),
    Sender1 ! {start, Receiver},
    Sender2 ! {start, Receiver},
    ?wait_for({line, ["sent", 5, "from", Sender1]}),
    ?wait_for({line, ["sent", 15, "from", Sender2]}),
    Sender2 ! {start, Receiver},
    receive {'DOWN', Ref, process, Receiver, normal} -> ok end,
    ?wait_for({line, ["sent", 10, "from", Sender2]}),
    tracerl_process:stop(DP),
    ?wait_for(eof),
    ?expect_not({line, [sent, _, _, _]}).

count_messages_by_sender_and_receiver_test(_Config) ->
    {Receiver1, Ref1} = spawn_monitor(fun() -> receive_n(1, 30) end),
    {Receiver2, Ref2} = spawn_monitor(fun() -> receive_n(31, 40) end),
    Sender1 = spawn(fun() -> send_n(1, 10),
                             self() ! {start, Receiver2},
                             send_n(31, 35) end),
    Sender2 = spawn(fun() -> send_n(11, 30),
                             self() ! {start, Receiver2},
                             send_n(36, 40) end),
    DP = start_trace(
           count_messages_by_sender_and_receiver_script([Sender1, Sender2])),
    ?wait_for({line, ["start"]}),
    Sender1 ! {start, Receiver1},
    Sender2 ! {start, Receiver1},
    receive {'DOWN', Ref1, process, Receiver1, normal} -> ok end,
    receive {'DOWN', Ref2, process, Receiver2, normal} -> ok end,
    tracerl_process:stop(DP),
    ?wait_for(eof),
    ?expect({line, ["sent", 10, "from", Sender1, "to", Receiver1]}),
    ?expect({line, ["sent", 1,  "from", Sender1, "to", Sender1]}),
    ?expect({line, ["sent", 5,  "from", Sender1, "to", Receiver2]}),
    ?expect({line, ["sent", 20, "from", Sender2, "to", Receiver1]}),
    ?expect({line, ["sent", 1,  "from", Sender2, "to", Sender2]}),
    ?expect({line, ["sent", 5,  "from", Sender2, "to", Receiver2]}).

count_messages_by_sender_and_receiver_term_test(_Config) ->
    {Receiver1, Ref1} = spawn_monitor(fun() -> receive_n(1, 30) end),
    {Receiver2, Ref2} = spawn_monitor(fun() -> receive_n(31, 40) end),
    Sender1 = spawn(fun() -> send_n(1, 10),
                             self() ! {start, Receiver2},
                             send_n(31, 35) end),
    Sender2 = spawn(fun() -> send_n(11, 30),
                             self() ! {start, Receiver2},
                             send_n(36, 40) end),
    DP = start_term_trace(
           count_messages_by_sender_and_receiver_term_script(
             [Sender1, Sender2])),
    ?wait_for({term, start}),
    Sender1 ! {start, Receiver1},
    Sender2 ! {start, Receiver1},
    receive {'DOWN', Ref1, process, Receiver1, normal} -> ok end,
    receive {'DOWN', Ref2, process, Receiver2, normal} -> ok end,
    tracerl_process:stop(DP),
    ?wait_for(eof),
    ?wait_for({term, {sent, [stat|Stat0]}}, 0),
    Stat = [{?l2p(P1), ?l2p(P2), N} || {P1, P2, N} <- Stat0],
    true = lists:member({Sender1, Receiver1, 10}, Stat),
    true = lists:member({Sender1, Receiver2, 5}, Stat),
    true = lists:member({Sender2, Receiver1, 20}, Stat),
    true = lists:member({Sender2, Receiver2, 5}, Stat).

count_messages_up_down_test(_Config) ->
    [A, B, C, D] = Ps = ring_start(4),
    DP = start_trace(count_messages_up_down_script(Ps)),
    ?wait_for({line, ["start"]}),
    Ref1 = ring_send(Ps, 10, self()),
    Ref2 = ring_send(lists:reverse(Ps), 5, self()),
    receive {finished, Ref1} -> ok end,
    receive {finished, Ref2} -> ok end,
    tracerl_process:stop(DP),
    ring_stop(Ps),
    ?wait_for(eof),
    ?expect({line, ["sent", "up", 10, "down", 5, "from", A, "to", B]}),
    ?expect({line, ["sent", "up", 10, "down", 5, "from", B, "to", C]}),
    ?expect({line, ["sent", "up", 10, "down", 5, "from", C, "to", D]}),
    ?expect({line, ["sent", "up", 5, "down", 10, "from", A, "to", D]}).

sender_and_receiver_set_test(_Config) ->
    [A, B, C] = Ps = ring_start(3),
    DP = start_trace(sender_and_receiver_set_script(Ps)),
    ?wait_for({line, ["start"]}),
    Ref1 = ring_send(Ps, 1, self()),
    Ref2 = ring_send(tl(Ps), 1, self()),
    receive {finished, Ref1} -> ok end,
    receive {finished, Ref2} -> ok end,
    tracerl_process:stop(DP),
    ring_stop(Ps),
    ?wait_for(eof),
    ?expect({line, ["sent", "from", A, "to", B]}),
    ?expect({line, ["sent", "from", B, "to", C]}),
    ?expect({line, ["sent", "from", C, "to", A]}),
    ?expect({line, ["sent", "from", C, "to", B]}),
    ?expect_not({line, ["sent", "from", B, "to", A]}),
    ?expect_not({line, ["sent", "from", A, "to", C]}).

message_receive_stats_test(_Config) ->
    Messages = ["1", "222", "33333333", "44444", "55"],
    {Messages1, Messages2} = lists:split(3, Messages),
    {Receiver1, Ref1} = spawn_monitor(fun() -> receive_all(Messages1) end),
    {Receiver2, Ref2} = spawn_monitor(fun() -> receive_all(Messages2) end),
    Sender1 = spawn(fun() ->
                            send_all([hd(Messages1)]),
                            self() ! {start, Receiver2},
                            send_all(tl(Messages2))
                    end),
    Sender2 = spawn(fun() ->
                            send_all([hd(Messages2)]),
                            self() ! {start, Receiver1},
                            send_all(tl(Messages1))
                    end),
    DP = start_trace(message_receive_stats_script([Receiver1, Receiver2])),
    ?wait_for({line, ["start"]}),
    Sender1 ! {start, Receiver1},
    Sender2 ! {start, Receiver2},
    receive {'DOWN', Ref1, process, Receiver1, normal} -> ok end,
    receive {'DOWN', Ref2, process, Receiver2, normal} -> ok end,
    tracerl_process:stop(DP),
    ?wait_for(eof),
    [S1, S2, S3, S4, S5] = [?msize(M) || M <- Messages],
    {Min1, Avg1, Max1, Total1} = {S1, (S1+S2+S3) div 3, S3, S1+S2+S3},
    {Min2, Avg2, Max2, Total2} = {S5, (S5+S4) div 2, S4, S4+S5},
    ?expect({line, ["pid", Receiver1, "received", 3, "messages:",
                    "min", Min1, "avg", Avg1,
                    "max", Max1, "total", Total1]}),
    ?expect({line, ["pid", Receiver2, "received", 2, "messages:",
                    "min", Min2, "avg", Avg2,
                    "max", Max2, "total", Total2]}).

%%%-------------------------------------------------------------------
%%% Dyntrace scripts
%%%-------------------------------------------------------------------

begin_tick_end_script() ->
    [{probe, 'begin',
      [{printf, "start\n"}]},
     {probe, {tick, 1},
      [{printf, "ticked\n"}]},
     {probe, 'end',
      [{printf, "finish\n"}]}].

variable_script(Sender) ->
    [{probe, 'begin',
      [{'=', total_num, 0},
       {'=', total_size, 0},
       {printf, "start\n"}]},
     {probe, "message-send", {'==', sender_pid, Sender},
      [{'=', sender, sender_pid},
       {'=', receiver, receiver_pid},
       {'=', msize, size},
       {'++', total_num},
       {'+=', total_size, size},
       {printf, "sent %s %s %d\n", [sender, receiver, msize]}]},
     {probe, 'end',
      [{printf, "last %s %s %d\n", [sender, receiver, msize]},
       {printf, "total num %d size %d\n", [total_num, total_size]}]}].

associative_array_script(Sender) ->
    [{probe, 'begin',
      [{'=', total, ["num"], 0},
       {'=', total, ["size"], 0},
       {printf, "start\n"}]},
     {probe, "message-send", {'==', sender_pid, Sender},
      [{'++', total, ["num"]},
       {'+=', total, ["size"], size},
       {printf, "DEBUG sent %s %s %d\n", [sender_pid, receiver_pid, size]}]},
     {probe, 'end',
      [{printf, "total num %d size %d\n", [{total, ["num"]},
                                           {total, ["size"]}]}]}].

count_messages_by_sender_script(Senders) ->
    Predicates = [{'==', sender_pid, Sender} || Sender <- Senders],
    [{probe, 'begin',
      [{printf, "start\n"}]},
     {probe, "message-send", {'||', Predicates},
      [{printf, "DEBUG sent %s %s %d\n", [sender_pid, receiver_pid, size]},
       {count, msg, [sender_pid]}]},
     {probe, 'end',
      [{printa, "sent %@d from %s\n", [msg]}]}].

count_messages_by_sender_with_reset_script(Senders) ->
    Predicates = [{'==', sender_pid, Sender} || Sender <- Senders],
    [{probe, 'begin',
      [{printf, "start\n"}]},
     {probe, "message-send", {'||', Predicates},
      [{printf, "DEBUG sent %s %s %d\n", [sender_pid, receiver_pid, size]},
       {count, msg, [sender_pid]}]},
     {probe, {tick, 1},
      [{printa, "sent %@d from %s\n", [msg]},
       {reset, msg}]}].

count_messages_by_sender_and_receiver_script(Senders) ->
    Predicates = [{'==', sender_pid, Sender} || Sender <- Senders],
    [{probe, 'begin',
      [{printf, "start\n"}]},
     {probe, "message-send", {'||', Predicates},
      [{printf, "DEBUG sent %s %s %d\n", [sender_pid, receiver_pid, size]},
       {count, msg, [sender_pid, receiver_pid]}]},
     {probe, 'end',
      [{printa, "sent %@d from %s to %s\n", [msg]}]}].

count_messages_by_sender_and_receiver_term_script(Senders) ->
    Predicates = [{'==', sender_pid, Sender} || Sender <- Senders],
    [{probe, 'begin',
      [{print_term, start}]},
     {probe, "message-send", {'||', Predicates},
      [%{print_term, {debug, sent %s %s %d\n", [sender_pid, receiver_pid, size]},
       {count, msg, [sender_pid, receiver_pid]}]},
     {probe, 'end',
      [{print_term, {sent, '$1'},
        [{stat, {"%s", "%s", "%@d"}, msg}]}
      ]}
    ].

count_messages_up_down_script(Senders) ->
    Predicates = [{'==', sender_pid, Sender} || Sender <- Senders],
    [{probe, 'begin',
      [{printf, "start\n"}]},
     {probe, "message-send", {'&&', [{'||', Predicates},
                                     {'<', sender_pid, receiver_pid}]},
      [{printf, "DEBUG sent up %s %s %d\n", [sender_pid, receiver_pid, size]},
       {count, msg_up, [sender_pid, receiver_pid]}]},
     {probe, "message-send", {'&&', [{'||', Predicates},
                                     {'>', sender_pid, receiver_pid}]},
      [{printf, "DEBUG sent down %s %s %d\n", [sender_pid, receiver_pid, size]},
       {count, msg_down, [receiver_pid, sender_pid]}]},
     {probe, 'end',
      [{printa, "sent up %@d down %@d from %s to %s\n", [msg_up, msg_down]}]}].

sender_and_receiver_set_script(Senders) ->
    Predicates = [{'==', sender_pid, Sender} || Sender <- Senders],
    [{probe, 'begin',
      [{printf, "start\n"}]},
     {probe, "message-send", {'||', Predicates},
      [{printf, "DEBUG sent %s %s %d\n", [sender_pid, receiver_pid, size]},
       {set, msg_proc, [sender_pid, receiver_pid]}]},
     {probe, 'end',
      [{printa, "sent from %s to %s\n", [msg_proc]}]}].

message_receive_stats_script(Receivers) ->
    Predicates = [{'==', pid, Receiver} || Receiver <- Receivers],
    [{probe, 'begin',
      [{printf, "start\n"}]},
     {probe, "message-receive", {'||', Predicates},
      [{printf, "DEBUG received %s %d %d\n", [pid, size, queue_length]},
       {count, recv, [pid]},
       {avg, avg_size, [pid], size},
       {min, min_size, [pid], size},
       {max, max_size, [pid], size},
       {sum, total_size, [pid], size}]},
     {probe, 'end',
      [{printa, "pid %s received %@d messages: "
        "min %@d avg %@d max %@d total %@d\n",
        [recv, min_size, avg_size, max_size, total_size]}]}].
