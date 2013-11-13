%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Dynamic trace utilities
%%%
%%% @end
%%% Created : 4 Jul 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(tracerl_util).

-export([trace/2, start_trace/2, start_trace/3, quit/1, term_handler/1]).

-define(tracerl_gen(Node),
        (case rpc:call(Node, erlang, system_info, [dynamic_trace]) of
             dtrace    -> tracerl_gen_dtrace;
             systemtap -> tracerl_gen_systemtap
         end)).

trace(ScriptSrc, Action) ->
    {Port, Pid, SrcFile, _Script} =
        start_trace(ScriptSrc, [stream, in, stderr_to_stdout, eof], node()),
    receive
        {Port, {data, Sofar}} ->
            Res = Action(),
            quit(Pid),
            {Res, get_data(Port, Sofar, SrcFile)}
    end.

get_data(Port, Sofar, SrcFile) ->
    receive
        {Port, {data, Bytes}} ->
            get_data(Port, [Sofar|Bytes], SrcFile);
        {Port, eof} ->
            port_close(Port),
            file:delete(SrcFile),
            re:split(lists:flatten(Sofar), "\n", [{return, list}, trim])
    end.

start_trace(ScriptSrc, Node) ->
    start_trace(ScriptSrc, [stream, {line, 1024}, eof], Node).

start_trace(ScriptSrc, PortArgs, Node) ->
    Sudo = os:find_executable(sudo),
    Pid = proc_lib:spawn_link(Node, timer, sleep, [infinity]),
    PidStr = rpc:call(Node, erlang, pid_to_list, [Pid]),
    Termination = termination_probe(PidStr),
    Script = ?tracerl_gen(Node):script([Termination|ScriptSrc], Node),
    {A, B, C} = now(),
    SrcFile = lists:flatten(io_lib:format("tracerl-script-~p-~p.~p.~p",
                                          [node(), A, B, C])),
    ok = file:write_file(SrcFile, Script),
    Args = case ?tracerl_gen(Node) of
               tracerl_gen_dtrace ->
                   [os:find_executable(dtrace), "-q", "-s", SrcFile];
               tracerl_gen_systemtap ->
                   [os:find_executable(stap), SrcFile]
           end,
    Port = open_port({spawn_executable, Sudo}, [{args, Args} | PortArgs]),
    {Port, Pid, SrcFile, Script}.

quit(Pid) ->
    exit(Pid, quit).

term_handler(Handler) ->
    fun(eof) ->
            Handler(eof);
       ({line, ""}) ->
            ok;
       ({line, Line}) ->
            try
                {ok, Tokens, _} = erl_scan:string(Line),
                {ok, Term} = erl_parse:parse_term(Tokens),
                Handler({term, Term})
            catch
                error:Reason ->
                    Handler({error, Reason, Line})
            end
    end.

%% Helpers

termination_probe(Pid) ->
    {probe, "process-exit", {'==', {arg_str,1}, Pid},
     [exit]}.
