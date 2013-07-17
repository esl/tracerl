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
-define(p2l(Pid), pid_to_list(Pid)).

suite() ->
    [{timetrap, {minutes, 1}}].

all() ->
    case erlang:system_info(dynamic_trace) of
        none ->
            {skip, "No dynamic trace in this run-time system"};
        _ ->
            [begin_tick_end_test,
             count_messages_by_sender_test]
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
            Pattern -> ok
        after Timeout ->
                erlang:error(not_found)
        end).
-define(expect(Pattern), ?wait_for(Pattern, 0)).
-define(wait_for(Pattern), ?wait_for(Pattern, 10000)).

begin_tick_end_test(_Config) ->
    DP = start_trace(begin_tick_end_script()),
    ?wait_for({line, {begun}}),
    ?wait_for({line, {ticked}}),
    ?wait_for({line, {ticked}}),
    ?wait_for({line, {ticked}}),
    dyntrace_process:stop(DP),
    ?wait_for(eof),
    ?expect({line, {ended}}).

count_messages_by_sender_test(_Config) ->
    {Receiver, Ref} = spawn_monitor(fun() -> receive_n(1, 30) end),
    Sender1 = spawn(fun() -> send_n(Receiver, 1, 10) end),
    Sender2 = spawn(fun() -> send_n(Receiver, 11, 30) end),
    DP = start_trace(count_messages_by_sender_script([Sender1, Sender2])),
    ?wait_for({line, {start}}),
    Sender1 ! start,
    Sender2 ! start,
    receive {'DOWN', Ref, process, Receiver, normal} -> ok end,
    dyntrace_process:stop(DP),
    ?wait_for(eof),
    {Sender1Str, Sender2Str} = {?p2l(Sender1), ?p2l(Sender2)},
    ?expect({line, {sent, "10", "from", Sender1Str}}),
    ?expect({line, {sent, "20", "from", Sender2Str}}).

%%%-------------------------------------------------------------------
%%% Test helpers
%%%-------------------------------------------------------------------

send_n(Receiver, StartNo, EndNo) ->
    receive start -> ok end,
    [Receiver ! {self(), No} || No <- lists:seq(StartNo, EndNo)].

receive_n(StartNo, EndNo) ->
    [receive {_, No} -> ok end || No <- lists:seq(StartNo, EndNo)].

start_trace(ScriptSrc) ->
    Self = self(),
    Collector = spawn(fun() -> collect(Self, []) end),
    {ok, DP} = dyntrace_process:start_link(ScriptSrc, node(),
                                           fun(Msg) -> Collector ! Msg end),
    DP.

collect(Dest, Output) ->
    receive
        {line, L} ->
            TermL = termify_line(L),
            Dest ! {line, TermL},
            collect(Dest, [TermL|Output]);
        eof ->
            ct:log("trace output:~n~p~n", [lists:reverse(Output)]),
            Dest ! eof
    end.

termify_line(L) ->
    [H|T] = re:split(L, " ", [{return,list}]),
    list_to_tuple([list_to_atom(H)|T]).

%%%-------------------------------------------------------------------
%%% Dyntrace scripts
%%%-------------------------------------------------------------------

count_messages_by_sender_script(Senders) ->
    Predicates = [{'==', {arg_str,1}, Sender} || Sender <- Senders],
    [{probe, 'begin',
      [{printf, "start\n", []}]},
     {probe, "message-send", {'||', Predicates},
      [{printf, "sent %s %s %d\n", [{arg_str,1}, {arg_str,2}, {arg,3}]},
       {count, msg, {arg_str,1}}]},
     {probe, 'end',
      [{printa, "sent %@d from %s\n", [msg]}]}].

begin_tick_end_script() ->
    [{probe, 'begin',
      [{printf, "begun\n", []}]},
     {probe, {tick, 1},
      [{printf, "ticked\n", []}]},
     {probe, 'end',
      [{printf, "ended\n", []}]}].
