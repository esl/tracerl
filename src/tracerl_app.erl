%%%-------------------------------------------------------------------
%%% @author  Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc tracerl application
%%%
%%% @end
%%% Created : 7 Jan 2014 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(tracerl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    tracerl_sup:start_link().

stop(_State) ->
    ok.
