%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Dynamic trace utilities
%%%
%%% @end
%%% Created : 4 Jul 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(dyntrace_util).

-export([trace/2, start_trace/3]).

-define(dyntrace_gen(Node),
        (case rpc:call(Node, erlang, system_info, [dynamic_trace]) of
             dtrace    -> dyntrace_gen_dtrace;
             systemtap -> dyntrace_gen_systemtap
         end)).

trace(Script, Action) ->
    Pid = spawn(fun() ->
                        receive _ -> exit(done) end
                end),
    Termination = termination_probe(Pid),
    TerminatedScript = Script ++ [Termination],
    {Port, SrcFile} = start_trace(TerminatedScript,
                                  [stream, in, stderr_to_stdout, eof], node()),
    receive
	{Port, {data, Sofar}} ->
	    Res = Action(),
	    Pid ! quit,
	    {Res, get_data(Port, Sofar, SrcFile)}
    end.

get_data(Port, Sofar, SrcFile) ->
    receive
	{Port, {data, Bytes}} ->
	    get_data(Port, [Sofar|Bytes], SrcFile);
	{Port, eof} ->
	    port_close(Port),
            file:delete(SrcFile),
	    [$\n|T] = lists:flatten(Sofar),
	    re:split(T, "\n", [{return, list}, trim])
    end.

start_trace(Script, PortArgs, Node) ->
    Sudo = os:find_executable(sudo),
    Termination = termination_probe(self()),
    GenScript = ?dyntrace_gen(Node):script(Script ++ [Termination], Node),
    io:format("~s~n", [GenScript]),
    {A, B, C} = now(),
    SrcFile = lists:flatten(io_lib:format("dyntrace-script-~p-~p.~p.~p",
                                          [node(), A, B, C])),
    ok = file:write_file(SrcFile, GenScript),
    Args = case ?dyntrace_gen(Node) of
               dyntrace_gen_dtrace ->
                   [os:find_executable(dtrace), "-q", "-s", SrcFile];
               dyntrace_gen_systemtap ->
                   [os:find_executable(stap), SrcFile]
           end,
    Port = open_port({spawn_executable, Sudo}, [{args, Args} | PortArgs]),
    {Port, SrcFile}.

termination_probe(Pid) ->
    {probe, ["process-exit"], [{'==', {arg_str,1}, Pid}],
     [{action, exit}]}.
