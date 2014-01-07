%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Test utilities
%%%
%%% @end
%%% Created : 12 Aug 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(tracerl_test_util).

-include("tracerl_util.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all_if_dyntrace/1,
         start_trace/1, start_trace/2,
         start_term_trace/1, start_term_trace/2]).

%% test helpers
-export([send_n/2, receive_n/2,
         send_all/1, receive_all/1,
         ring_start/1, ring_stop/1, ring_send/3,
         test_function/0, test_function_1/1]).

all_if_dyntrace(All) ->
    case erlang:system_info(dynamic_trace) of
        none -> {skip, "No dynamic trace in this run-time system"};
        _    -> All
    end.

start_trace(ScriptSrc) ->
    start_trace(ScriptSrc, node()).

start_trace(ScriptSrc, Node) ->
    Self = self(),
    Collector = proc_lib:spawn_link(fun() -> collect(Self, Node, []) end),
    {ok, DP} = tracerl:start_link(ScriptSrc, Node, Collector),
    ct:log(gen_server:call(DP, get_script)),
    DP.

start_term_trace(ScriptSrc) ->
    start_term_trace(ScriptSrc, node()).

start_term_trace(ScriptSrc, Node) ->
    Self = self(),
    Collector = proc_lib:spawn_link(fun() -> collect_terms(Self, []) end),
    {ok, DP} = tracerl:start_link(ScriptSrc, Node, Collector, [term]),
    ct:log(gen_server:call(DP, get_script)),
    DP.

collect(Dest, Node, Output) ->
    process_flag(trap_exit, true),
    receive
        {line, ""} ->
            collect(Dest, Node, Output);
        {line, "DEBUG " ++ _ = L} ->
            TermL = termify_line(L, Node),
            collect(Dest, Node, [TermL|Output]);
        {line, L} ->
            TermL = termify_line(L, Node),
            Dest ! {line, TermL},
            collect(Dest, Node, [TermL|Output]);
        eof ->
            ct:log("finished, trace output:~n~p~n", [lists:reverse(Output)]),
            Dest ! eof;
        Msg ->
            ct:pal(error, ?HI_IMPORTANCE,
                   "exited because of ~p~ntrace output:~n~p~n",
                   [Msg, lists:reverse(Output)])
    end.

collect_terms(Dest, Output) ->
    process_flag(trap_exit, true),
    receive
        {term, {debug, _} = Debug} ->
            collect_terms(Dest, [Debug|Output]);
        {term, Term} ->
            Dest ! {term, Term},
            collect_terms(Dest, [Term|Output]);
        eof ->
            ct:log("finished, trace output:~n~p~n", [lists:reverse(Output)]),
            Dest ! eof;
        Msg ->
            ct:pal(error, ?HI_IMPORTANCE,
                   "exited because of ~p~ntrace output:~n~p~n",
                   [Msg, lists:reverse(Output)])
    end.

termify_line(L, Node) ->
    [termify_token(Token, Node) || Token <- re:split(L, " ", [{return,list}])].

termify_token(L, Node) ->
    try
        ?l2i(L)
    catch error:badarg ->
            case ?l2p(Node, L) of
                {badrpc, _} -> L;
                Pid -> fix_pid_creation(Pid, Node)
            end
    end.

fix_pid_creation(Pid, Node) ->
    case node(Pid) == Node of
        true  -> Pid;
        false -> binary_to_term(
                   rpc:call(node(Pid), erlang, term_to_binary, [Pid]))
    end.

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
    sets:from_list([]),
    Ref = make_ref(),
    lists:last(Ps) ! lists:flatten([lists:duplicate(MsgNum, Ps),
                                    {notify, Pid, Ref}]),
    Ref.

test_function() ->
    A = ?MODULE:test_function_1(123),
    {B, C} = test_function_2(A, b),
    test_function_2(C, B).

test_function_1(A) ->
    A.
    %% The line below would make the test case fail!
    %% Apparently beam would not know which function it returned to.
    %% receive X -> X after 0 -> {A} end.

test_function_2(A, B) ->
    {A, B}.
