%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Main tracerl API
%%%
%%% @end
%%% Created : 21 Aug 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(tracerl).

-export([start_trace/3, start_trace/4, stop_trace/1]).

start_trace(ScriptSrc, Node, PidOrHandler) ->
    start_trace(ScriptSrc, Node, PidOrHandler, []).

start_trace(ScriptSrc, Node, PidOrHandler, Options) ->
    tracerl_sup:start_child(ScriptSrc, Node, PidOrHandler, Options).

stop_trace(Pid) ->
    tracerl_process:stop(Pid).
