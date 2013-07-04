%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Dynamic trace utilities
%%%
%%% @end
%%% Created : 4 Jul 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(dyntrace_util).

-compile(export_all).

-define(dyntrace_gen,
        (case erlang:system_info(dynamic_trace) of
             dtrace    -> dyntrace_gen_dtrace;
             systemtap -> dyntrace_gen_systemtap
         end)).

trace(Script0, Action) ->
    Sudo = os:find_executable(sudo),
    {Termination, Pid} = termination_probe(),
    Script1 = Script0 ++ [Termination],
    Script = ?dyntrace_gen:script(Script1),
    io:format("~s\n", [Script]),
    SrcFile = "test-dyntrace.d",
    ok = file:write_file(SrcFile, Script),
    Args = case erlang:system_info(dynamic_trace) of
               dtrace    -> [os:find_executable(dtrace), "-q", "-s", SrcFile];
               systemtap -> [os:find_executable(stap)]
           end,
    Port = open_port({spawn_executable, Sudo},
		     [{args, Args}, stream, in, stderr_to_stdout, eof]),
    receive
	{Port, {data, Sofar}} ->
	    Res = Action(),
	    Pid ! quit,
	    {Res, get_data(Port, Sofar)}
    end.

get_data(Port, Sofar) ->
    receive
	{Port, {data, Bytes}} ->
	    get_data(Port, [Sofar|Bytes]);
	{Port, eof} ->
	    port_close(Port),
	    [$\n|T] = lists:flatten(Sofar),
	    re:split(T, "\n", [{return, list}, trim])
    end.

termination_probe() ->
    Pid = spawn(fun() ->
                        receive _ -> exit(done) end
		end),
    {{probe, ["process-exit"], [{'==', {arg_str,1}, Pid}],
      [{action, exit}]}, Pid}.
