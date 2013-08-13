%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Script generator
%%%
%%% @end
%%% Created : 26 Jul 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(tracerl_gen).

-export([script/2, script/3]).

script(CbkMod, ScriptSrc) ->
    script(CbkMod, ScriptSrc, node()).

script(CbkMod, ScriptSrc, Node) when is_atom(Node) ->
    PidStr = rpc:call(Node, os, getpid, []),
    script(CbkMod, ScriptSrc, PidStr);
script(CbkMod, ScriptSrc, PidStr) ->
    {PreScriptSrc, State} =
        process(CbkMod, preprocess, ScriptSrc, CbkMod:init_state(PidStr)),
    io:format("~p~n", [PreScriptSrc]),
    {Script, _State} =
        process(CbkMod, generate, PreScriptSrc, State),
    Script.

process(CbkMod, F, Item, InState) ->
    process(CbkMod, F, nop, Item, InState).

process(CbkMod, PreF, PostF, Item, InState) ->
    {InChildren, State} = call(CbkMod, PreF, Item, InState),
    {OutChildren, OutState} = process_list(CbkMod, InChildren, State),
    call(CbkMod, PostF, Item, OutChildren, OutState).

process_list(CbkMod, ItemL, InState) ->
    lists:mapfoldl(fun(L, St) when is_list(L) ->
                           process_list(CbkMod, L, St);
                      (I, St) when is_integer(I) ->
                           {I, St};
                      ({F, Item}, St) ->
                           process(CbkMod, F, Item, St);
                      ({PreF, PostF, Item}, St) ->
                           process(CbkMod, PreF, PostF, Item, St)
                   end, InState, ItemL).

call(CbkMod, F, Item, State) ->
    Res = case erlang:function_exported(CbkMod, F, 2) of
              true  -> CbkMod:F(Item, State);
              false -> false
          end,
    case Res of
        false -> tracerl_gen_common:F(Item, State);
        _     -> Res
    end.

call(CbkMod, F, Item, Children, State) ->
    Res = case erlang:function_exported(CbkMod, F, 3) of
              true  -> CbkMod:F(Item, Children, State);
              false -> false
          end,
    case Res of
        false -> tracerl_gen_common:F(Item, Children, State);
        _     -> Res
    end.
