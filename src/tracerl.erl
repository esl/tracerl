%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Main tracerl API
%%%
%%% @end
%%% Created : 21 Aug 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(tracerl).

-export([start_link/3, start_link/4, stop/1]).

start_link(ScriptSrc, Node, PidOrHandler) ->
    start_link(ScriptSrc, Node, PidOrHandler, []).

start_link(ScriptSrc, Node, Pid, Options) when is_pid(Pid) ->
    Handler = fun(Msg) -> Pid ! Msg end,
    tracerl_process:start_link(ScriptSrc, Node, Handler, Options);
start_link(ScriptSrc, Node, Handler, Options) when is_function(Handler) ->
    tracerl_process:start_link(ScriptSrc, Node, Handler, Options).

stop(Pid) ->
    tracerl_process:stop(Pid).
