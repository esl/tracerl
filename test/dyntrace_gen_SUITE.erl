%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Tests for generating dtrace/systemtap scripts.
%%%
%%% @end
%%% Created : 16 Jul 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(dyntrace_gen_SUITE).

-include_lib("test_server/include/test_server.hrl").

-compile(export_all).

-define(msg, "message").
-define(msize(Msg), erts_debug:flat_size(Msg)).
-define(p2l(P), pid_to_list(P)).
-define(i2l(I), integer_to_list(I)).

suite() ->
    [{timetrap, {minutes, 1}}].

all() ->
    case erlang:system_info(dynamic_trace) of
        none ->
            {skip, "No dynamic trace in this run-time system"};
        _ ->
            [begin_tick_end_test,
             variable_test,
             count_messages_by_sender_test,
             count_messages_by_sender_with_reset_test,
             count_messages_by_sender_and_receiver_test,
             count_messages_up_down_test,
             sender_and_receiver_set_test,
             message_receive_stats_test]
    end.

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%%-------------------------------------------------------------------
%%% Test cases
%%%-------------------------------------------------------------------
-define(wait_for(Pattern, Timeout),
        receive
            Pattern ->
                ok
        after Timeout ->
                erlang:error(not_found)
        end).
-define(expect(Pattern), ?wait_for(Pattern, 0)).
-define(wait_for(Pattern), ?wait_for(Pattern, 10000)).
-define(expect_not(Pattern),
        receive
            Pattern ->
                erlang:error(found_unexpected)
        after 0 ->
                ok
        end).

begin_tick_end_test(_Config) ->
    DP = start_trace(begin_tick_end_script()),
    ?wait_for({line, ["start"]}),
    ?wait_for({line, ["ticked"]}),
    ?wait_for({line, ["ticked"]}),
    ?wait_for({line, ["ticked"]}),
    dyntrace_process:stop(DP),
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
    dyntrace_process:stop(DP),
    ?wait_for(eof),
    ?expect({line, ["last", Sender, Receiver, Size2]}),
    ?expect({line, ["total", "num", 2, "size", TotalSize]}),
    ct:log("~p~n",[self()]).

count_messages_by_sender_test(_Config) ->
    {Receiver, Ref} = spawn_monitor(fun() -> receive_n(1, 30) end),
    Sender1 = spawn(fun() -> send_n(1, 10) end),
    Sender2 = spawn(fun() -> send_n(11, 30) end),
    DP = start_trace(count_messages_by_sender_script([Sender1, Sender2])),
    ?wait_for({line, ["start"]}),
    Sender1 ! {start, Receiver},
    Sender2 ! {start, Receiver},
    receive {'DOWN', Ref, process, Receiver, normal} -> ok end,
    dyntrace_process:stop(DP),
    ?wait_for(eof),
    ?expect({line, ["sent", 10, "from", Sender1]}),
    ?expect({line, ["sent", 20, "from", Sender2]}),
    ct:log("~p~n",[self()]).

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
    dyntrace_process:stop(DP),
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
    dyntrace_process:stop(DP),
    ?wait_for(eof),
    ?expect({line, ["sent", 10, "from", Sender1, "to", Receiver1]}),
    ?expect({line, ["sent", 1,  "from", Sender1, "to", Sender1]}),
    ?expect({line, ["sent", 5,  "from", Sender1, "to", Receiver2]}),
    ?expect({line, ["sent", 20, "from", Sender2, "to", Receiver1]}),
    ?expect({line, ["sent", 1,  "from", Sender2, "to", Sender2]}),
    ?expect({line, ["sent", 5,  "from", Sender2, "to", Receiver2]}).

count_messages_up_down_test(_Config) ->
    [A, B, C, D] = Ps = ring_start(4),
    DP = start_trace(count_messages_up_down_script(Ps)),
    ?wait_for({line, ["start"]}),
    Ref1 = ring_send(Ps, 10, self()),
    Ref2 = ring_send(lists:reverse(Ps), 5, self()),
    receive {finished, Ref1} -> ok end,
    receive {finished, Ref2} -> ok end,
    dyntrace_process:stop(DP),
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
    dyntrace_process:stop(DP),
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
    dyntrace_process:stop(DP),
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
%%% Test helpers
%%%-------------------------------------------------------------------

send_n(StartNo, EndNo) ->
    send_all(lists:seq(StartNo, EndNo)).

receive_n(StartNo, EndNo) ->
    receive_all(lists:seq(StartNo, EndNo)).

send_all(Messages) ->
    receive {start, Receiver} -> ok end,
    [Receiver ! Msg || Msg <- Messages].

receive_all(Messages) ->
    [receive Msg -> ok end || Msg <- Messages].

start_trace(ScriptSrc) ->
    Self = self(),
    Collector = spawn(fun() -> collect(Self, []) end),
    {ok, DP} = dyntrace_process:start_link(ScriptSrc, node(),
                                           fun(Msg) -> Collector ! Msg end),
    ct:log(gen_server:call(DP, get_script)),
    DP.

collect(Dest, Output) ->
    receive
        {line, "DEBUG " ++ _ = L} ->
            TermL = termify_line(L),
            collect(Dest, [TermL|Output]);
        {line, L} ->
            TermL = termify_line(L),
            Dest ! {line, TermL},
            collect(Dest, [TermL|Output]);
        eof ->
            ct:log("trace output:~n~p~n", [lists:reverse(Output)]),
            Dest ! eof
    end.

termify_line(L) ->
    [termify_token(Token) || Token <- re:split(L, " ", [{return,list}])].

termify_token(L) ->
    try list_to_integer(L)
    catch error:badarg ->
            try list_to_pid(L)
            catch error:badarg -> L
            end
    end.

ring_start(ProcNum) ->
    lists:sort([spawn(fun ring_proc/0) || _ <- lists:seq(1, ProcNum)]).

ring_stop(Ps) ->
    [P ! stop || P <- Ps].

ring_proc() ->
    receive
        [Ps|Rest] when is_list(Ps) ->
            [P ! Rest || P <- Ps],
            ring_proc();
        [{notify, Pid, Ref}] ->
            Pid ! {finished, Ref},
            ring_proc();
        [Next|Rest] ->
            Next ! Rest,
            ring_proc();
        [] ->
            ring_proc();
        stop ->
            ok
    end.

ring_send(Ps, MsgNum, Pid) ->
    Ref = make_ref(),
    lists:last(Ps) ! lists:flatten([lists:duplicate(MsgNum, Ps),
                                    {notify, Pid, Ref}]),
    Ref.

%%%-------------------------------------------------------------------
%%% Dyntrace scripts
%%%-------------------------------------------------------------------

begin_tick_end_script() ->
    [{probe, 'begin',
      [{printf, "start\n", []}]},
     {probe, {tick, 1},
      [{printf, "ticked\n", []}]},
     {probe, 'end',
      [{printf, "finish\n", []}]}].

variable_script(Sender) ->
    [{probe, 'begin',
      [{'=', total_num, 0},
       {'=', total_size, 0},
       {printf, "start\n", []}]},
     {probe, "message-send", {'==', {arg_str,1}, Sender},
      [{'=', sender, {arg_str,1}},
       {'=', receiver, {arg_str,2}},
       {'=', size, {arg,3}},
       {'++', total_num},
       {'+=', total_size, size},
       {printf, "sent %s %s %d\n", [sender, receiver, size]}]},
     {probe, 'end',
      [{printf, "last %s %s %d\n", [sender, receiver, size]},
       {printf, "total num %d size %d\n", [total_num, total_size]}]}].

count_messages_by_sender_script(Senders) ->
    Predicates = [{'==', {arg_str,1}, Sender} || Sender <- Senders],
    [{probe, 'begin',
      [{printf, "start\n", []}]},
     {probe, "message-send", {'||', Predicates},
      [{printf, "DEBUG sent %s %s %d\n", [{arg_str,1}, {arg_str,2}, {arg,3}]},
       {count, msg, [{arg_str,1}]}]},
     {probe, 'end',
      [{printa, "sent %@d from %s\n", [msg]}]}].

count_messages_by_sender_with_reset_script(Senders) ->
    Predicates = [{'==', {arg_str,1}, Sender} || Sender <- Senders],
    [{probe, 'begin',
      [{printf, "start\n", []}]},
     {probe, "message-send", {'||', Predicates},
      [{printf, "DEBUG sent %s %s %d\n", [{arg_str,1}, {arg_str,2}, {arg,3}]},
       {count, msg, [{arg_str,1}]}]},
     {probe, {tick, 1},
      [{printa, "sent %@d from %s\n", [msg]},
       {reset, msg}]}].

count_messages_by_sender_and_receiver_script(Senders) ->
    Predicates = [{'==', {arg_str,1}, Sender} || Sender <- Senders],
    [{probe, 'begin',
      [{printf, "start\n", []}]},
     {probe, "message-send", {'||', Predicates},
      [{printf, "DEBUG sent %s %s %d\n", [{arg_str,1}, {arg_str,2}, {arg,3}]},
       {count, msg, [{arg_str,1}, {arg_str,2}]}]},
     {probe, 'end',
      [{printa, "sent %@d from %s to %s\n", [msg]}]}].

count_messages_up_down_script(Senders) ->
    Predicates = [{'==', {arg_str,1}, Sender} || Sender <- Senders],
    [{probe, 'begin',
      [{printf, "start\n", []}]},
     {probe, "message-send", {'&&', [{'||', Predicates},
                                     {'<', {arg_str,1}, {arg_str,2}}]},
      [{printf, "DEBUG sent up %s %s %d\n", [{arg_str,1}, {arg_str,2}, {arg,3}]},
       {count, msg_up, [{arg_str,1}, {arg_str,2}]}]},
     {probe, "message-send", {'&&', [{'||', Predicates},
                                     {'>', {arg_str,1}, {arg_str,2}}]},
      [{printf, "DEBUG sent down %s %s %d\n", [{arg_str,2}, {arg_str,1}, {arg,3}]},
       {count, msg_down, [{arg_str,2}, {arg_str,1}]}]},
     {probe, 'end',
      [{printa, "sent up %@d down %@d from %s to %s\n", [msg_up, msg_down]}]}].

sender_and_receiver_set_script(Senders) ->
    Predicates = [{'==', {arg_str,1}, Sender} || Sender <- Senders],
    [{probe, 'begin',
      [{printf, "start\n", []}]},
     {probe, "message-send", {'||', Predicates},
      [{printf, "DEBUG sent %s %s %d\n", [{arg_str,1}, {arg_str,2}, {arg,3}]},
       {set, msg_proc, [{arg_str,1}, {arg_str,2}]}]},
     {probe, 'end',
      [{printa, "sent from %s to %s\n", [msg_proc]}]}].

message_receive_stats_script(Receivers) ->
    Predicates = [{'==', {arg_str,1}, Receiver} || Receiver <- Receivers],
    [{probe, 'begin',
      [{printf, "start\n", []}]},
     {probe, "message-receive", {'||', Predicates},
      [{printf, "DEBUG received %s %d %d\n", [{arg_str,1}, {arg,2}, {arg,3}]},
       {count, recv, [{arg_str,1}]},
       {avg, avg_size, [{arg_str,1}], {arg,2}},
       {min, min_size, [{arg_str,1}], {arg,2}},
       {max, max_size, [{arg_str,1}], {arg,2}},
       {sum, total_size, [{arg_str,1}], {arg,2}}]},
     {probe, 'end',
      [{printa, "pid %s received %@d messages: "
        "min %@d avg %@d max %@d total %@d\n",
        [recv, min_size, avg_size, max_size, total_size]}]}].
