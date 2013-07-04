%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Script generator for dtrace
%%%
%%% @end
%%% Created : 4 Jul 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(dyntrace_SUITE).

-compile(export_all).

suite() ->
    [{timetrap, {minutes, 1}}].

all() ->
    case erlang:system_info(dynamic_trace) of
	none ->
	    {skip, "No dynamic trace in this run-time system"};
	_ ->
	    [
             message
            ]
    end.

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

-define(msg1, "message").
-define(msg2, "other message").
-define(msize(Msg), integer_to_list(erts_debug:flat_size(Msg))).
-define(p2l(Pid), pid_to_list(Pid)).

message(Config) ->
    message(Config, fun message_test/0).

message_test() ->
    {Receiver, Ref} = spawn_monitor(fun() ->
                                            receive ?msg1 -> ok end
                                    end),
    Sender = spawn(fun() ->
                           Receiver ! ?msg2,
                           Receiver ! ?msg1
                   end),
    receive
        {'DOWN', Ref, process, Receiver, normal} ->
            {Sender, Receiver}
    end.

message(_Config, TestF) ->
    {{Sender, Receiver}, Output0} =
        dyntrace_util:trace(message_script(), TestF),
    Output1 = [termify_line(L) || L <- Output0],
    {SenderStr, ReceiverStr} = {?p2l(Sender), ?p2l(Receiver)},
    {Msg1Size, Msg2Size} = {?msize(?msg1), ?msize(?msg2)},
    Output = [L || L <- Output1, element(2, L) =:= SenderStr
                       orelse element(2, L) =:= ReceiverStr],
    ct:log("~p\n", [Output]),
    [{sent, SenderStr, ReceiverStr, Msg2Size},
     {queued, ReceiverStr, Msg2Size, "1"},
     {sent, SenderStr, ReceiverStr, Msg1Size},
     {queued, ReceiverStr, Msg1Size, "2"},
     {received, ReceiverStr, Msg1Size, "1"}] = Output,
    ok.

message_script() ->
    [{probe, 'BEGIN',
      [{printf, ["\n"]}]},
     {probe, ["message-send"], [],
      [{printf, ["sent %s %s %d\n", {arg_str,1}, {arg_str,2}, {arg,3}]}]},
     {probe, ["message-queued"], [],
      [{printf, ["queued %s %d %d\n", {arg_str,1}, {arg,2}, {arg,3}]}]},
     {probe, ["message-receive"], [],
      [{printf, ["received %s %d %d\n", {arg_str,1}, {arg,2}, {arg,3}]}]}
    ].

termify_line(L) ->
    [H|T] = re:split(L, " ", [{return,list}]),
    list_to_tuple([list_to_atom(H)|T]).
