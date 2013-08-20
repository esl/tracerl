%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Common callbacks for dtrace and systemtap
%%%
%%% @end
%%% Created : 8 Aug 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(tracerl_gen_common).

-include("tracerl_util.hrl").

-import(tracerl_gen_util, [sep/2, sep_t/3, tag/2, tag/3, sep_f/3]).

%% pass 1
-export([preprocess/2, pre_probe/2, pre_after_probe/3,
         pre_st/2, pre_after_st/3]).

%% pass 2
-export([after_probe/3, st/2, st_body/2, op/2, nop/2, nop/3,
         indent/2, outdent/3, align/2]).

-compile(export_all).

%%-----------------------------------------------------------------------------
%% common callbacks for systemtap and dtrace - pass 1: preprocess
%%-----------------------------------------------------------------------------
preprocess(Probes, State) ->
    {tag(pre_probe, pre_after_probe, Probes), State}.

pre_probe({probe, Point, _Predicate, Statements}, State) ->
    pre_probe({probe, Point, split_terms(Statements)}, State);
pre_probe({probe, _Point, Statements}, State) ->
    {tag(pre_st, pre_after_st, split_terms(Statements)), State}.

pre_after_probe({probe, Point, Predicate, _}, Children, State) ->
    {{probe, Point, Predicate, Children}, State};
pre_after_probe({probe, Point, _}, Children, State) ->
    {{probe, Point, Children}, State}.

pre_st({?op_assign, Name, _Value}, State) ->
    add_var(Name, State);
pre_st({?op_assign, Name, _Keys, _Value}, State) ->
    add_var(Name, State);
pre_st(_, State) ->
    {[], State}.

pre_after_st(Item, _Children, State) ->
    {Item, State}.

%% helpers

add_var(Name, State = #gen_state{vars = Vars}) ->
    {[], State#gen_state{vars = ordsets:add_element(Name, Vars)}}.

split_terms(Statements) ->
    lists:flatmap(fun({print_term, Term}) ->
                          split_term(Term, []);
                     ({print_term, Term, Items}) ->
                          split_term(Term, Items);
                     (St) ->
                          [St]
                  end, Statements).

split_term(Term, Items) ->
    Str = lists:flatten(io_lib:format("~p.", [Term])),
    {ok, Tokens, _} = erl_scan:string(Str, 1, [text]),
    Parts = lists:reverse(lists:foldl(fun(Token, Acc) ->
                                              add_token(Token, Acc, Items)
                                      end, [], Tokens)),
    FlatParts = lists:flatten([process_part(Pt) || Pt <- Parts]),
    lists:reverse(lists:foldl(fun({printf, NewText}, [{printf, Text}|Rest]) ->
                                      [{printf, Text ++ NewText}|Rest];
                                 (Pt, Pts) ->
                                      [Pt|Pts]
                              end, [], FlatParts ++ [{printf, "\n"}])).

add_token(Token, Acc, Items) ->
    case maybe_token_to_item(Token, Items) of
        {ok, Item} ->
            [Item|Acc];
        _ ->
            {text, Text} = erl_scan:token_info(Token, text),
            case Acc of
                [{text, Texts}|Rest] ->
                    [{text, [Text|Texts]}|Rest];
                _ ->
                    [{text, [Text]}|Acc]
            end
    end.

maybe_token_to_item({atom, _, Atom}, Items) ->
    case atom_to_list(Atom) of
        [$$|NumStr] ->
            case catch list_to_integer(NumStr) of
                Num when Num > 0, Num =< length(Items) ->
                    {ok, lists:nth(Num, Items)};
                _ ->
                    false
            end;
        _ ->
            false
    end;
maybe_token_to_item(_, _) -> false.

process_part({text, RevStrings}) ->
    {printf, lists:flatten(lists:reverse(RevStrings))};
process_part({stat, Format, Stats}) when is_list(Format) ->
    process_part({stat, {Format}, Stats});
process_part({stat, Format, Stats}) when is_atom(Stats) ->
    process_part({stat, Format, [Stats]});
process_part({stat, Format0, Stats}) ->
    Format = [process_format_item(F) || F <- tuple_to_list(Format0)],
    [{printf, "[stat"},
     {printa,
      case Format of
          [F] -> "," ++ F;
          _   -> lists:flatten([",{", sep(Format, ","), "}"])
      end, Stats},
     {printf, "]"}].

process_format_item("%s")  -> "\"%s\"";
process_format_item("%@s") -> "\"%@s\"";
process_format_item(F)     -> F.

%% dfs(F, Elem, Acc0) ->
%%     {Acc, Children} = F(Elem, Acc),
%%     lists:mapfoldl(fun(El, AccIn) -> dfs(F, El, AccIn) end, Acc, Children).

%%-----------------------------------------------------------------------------
%% common callbacks for systemtap and dtrace - pass 2: generate
%%-----------------------------------------------------------------------------
after_probe(_Item, Children, State) ->
    {Children, State#gen_state{args = orddict:new()}}.

st(Item, State) ->
    {[{align, {st_body, Item}}], State}.

st_body({Op, Name, Value}, State = #gen_state{vars = Vars})
  when ?is_assign2(Op) ->
    true = ordsets:is_element(Name, Vars),
    {[?a2l(Name), " ", ?a2l(Op), " ", {op, Value}], State};
st_body({Op, Name, Keys, Value}, State = #gen_state{vars = Vars})
  when ?is_assign2(Op) ->
    true = ordsets:is_element(Name, Vars),
    {[?a2l(Name), "[", sep_t(op, Keys, ", "), "] ", ?a2l(Op), " ",
      {op, Value}], State};
st_body({Op, Name}, State = #gen_state{vars = Vars})
  when ?is_assign1(Op) ->
    true = ordsets:is_element(Name, Vars),
    {[?a2l(Name), " ", ?a2l(Op)], State};
st_body({Op, Name, Keys}, State = #gen_state{vars = Vars})
  when ?is_assign1(Op) ->
    true = ordsets:is_element(Name, Vars),
    {[?a2l(Name), "[", sep_t(op, Keys, ", "), "] ", ?a2l(Op)], State}.

op({Op, Operand1, Operand2}, State) when ?is_logic2(Op); ?is_arith2(Op) ->
    op({Op, [Operand1, Operand2]}, State);
op({Op, Operands}, State) when ?is_logic2(Op); ?is_arith2(Op) ->
    {["(", sep_t(op, Operands, [") ", ?a2l(Op), " ("]), ")"], State};
op({Op, Operand}, State) when ?is_logic1(Op); ?is_arith1(Op) ->
    {[?a2l(Op), "(", {op, Operand}, ")"], State};
op({Op, Operand1, Operand2}, State) when ?is_cmp(Op) ->
    {[{op, Operand1}, " ", ?a2l(Op), " ", {op, Operand2}], State};
op(Name, State = #gen_state{args = Args, vars = Vars})
  when is_atom(Name) ->
    {case orddict:find(Name, Args) of
         {ok, Arg} ->
             [{op, Arg}];
         error ->
             ct:log("~p ~p ~n", [Name, State]),
             true = ordsets:is_element(Name, Vars),
             ?a2l(Name)
     end, State};
op({Name, Keys}, State = #gen_state{vars = Vars}) when is_atom(Name) ->
    true = ordsets:is_element(Name, Vars),
    {[?a2l(Name), "[", sep_t(op, Keys, ", "), "]"], State}.

nop(Item, State) ->
    {Item, State}.

nop(_Item, Children, State) ->
    {Children, State}.

indent(Item, State = #gen_state{level = Level}) ->
    {Item, State#gen_state{level = Level+1}}.

outdent(_Item, Children, State = #gen_state{level = Level}) ->
    {Children, State#gen_state{level = Level-1}}.

align(Item, State) ->
    {[lists:duplicate(?INDENT * State#gen_state.level, $ ), Item], State}.
