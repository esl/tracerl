%%%-------------------------------------------------------------------
%%% @author  Pawel Chrzaszcz
%%% @copyright (C) 2014, Erlang Solutions Ltd.
%%% @doc Top level supervisor for tracerl application.
%%%
%%% @end
%%% Created : 7 Jan 2014 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(tracerl_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/4]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 4, 1},
          [{tracerl_process, {tracerl_process, start_link, []},
            temporary, 1000, worker, [tracerl_process]}]}}.

start_child(ScriptSrc, Node, PidOrhandler, Options) ->
    supervisor:start_child(?MODULE, [ScriptSrc, Node, PidOrhandler, Options]).
