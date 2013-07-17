%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Dynamic trace utilities
%%%
%%% @end
%%% Created : 4 Jul 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(dyntrace_util).

-export([trace/2, start_trace/2, start_trace/3, quit/1]).

-define(dyntrace_gen(Node),
        (case rpc:call(Node, erlang, system_info, [dynamic_trace]) of
             dtrace    -> dyntrace_gen_dtrace;
             systemtap -> dyntrace_gen_systemtap
         end)).

trace(Script, Action) ->
    {Port, Pid, SrcFile} =
        start_trace(Script, [stream, in, stderr_to_stdout, eof], node()),
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
    Pid = spawn(Node, timer, sleep, [infinity]),
    PidStr = rpc:call(Node, erlang, pid_to_list, [Pid]),
    Termination = termination_probe(PidStr),
    Script = ?dyntrace_gen(Node):script([Termination|ScriptSrc], Node),
    io:format("~s~n", [Script]),
    {A, B, C} = now(),
    SrcFile = lists:flatten(io_lib:format("dyntrace-script-~p-~p.~p.~p",
                                          [node(), A, B, C])),
    ok = file:write_file(SrcFile, Script),
    Args = case ?dyntrace_gen(Node) of
               dyntrace_gen_dtrace ->
                   [os:find_executable(dtrace), "-q", "-s", SrcFile];
               dyntrace_gen_systemtap ->
                   [os:find_executable(stap), SrcFile]
           end,
    Port = open_port({spawn_executable, Sudo}, [{args, Args} | PortArgs]),
    {Port, Pid, SrcFile}.

quit(Pid) ->
    exit(Pid, quit).

termination_probe(Pid) ->
    {probe, ["process-exit"], [{'==', {arg_str,1}, Pid}],
     [exit]}.
