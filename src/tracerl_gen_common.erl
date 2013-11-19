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

%%-----------------------------------------------------------------------------
%% common callbacks for systemtap and dtrace - pass 1: preprocess
%%-----------------------------------------------------------------------------
preprocess(Probes, State) ->
    {tag(pre_probe, pre_after_probe, Probes), State}.

pre_probe({probe, Point, _Predicate, Statements}, State) ->
    pre_probe({probe, Point, format_terms(Statements)}, State);
pre_probe({probe, _Point, Statements}, State) ->
    {tag(pre_st, pre_after_st, format_terms(Statements)), State}.

pre_after_probe({probe, Point, Predicate, _}, Children, State) ->
    {{probe, Point, Predicate, merge_printfs(Children)}, State};
pre_after_probe({probe, Point, _}, Children, State) ->
    {{probe, Point, merge_printfs(Children)}, State}.

pre_st({?op_assign, Name, _Value}, State) ->
    add_var(Name, State);
pre_st({?op_assign, Name, _Keys, _Value}, State) ->
    add_var(Name, State);
pre_st({Type, Name, Keys}, State)
  when Type == set; Type == count ->
    add_stat(Name, Type, Keys, State);
pre_st({Type, Name, Keys, _Value}, State)
  when Type == sum; Type == min; Type == max; Type == avg ->
    add_stat(Name, Type, Keys, State);
pre_st(_, State) ->
    {[], State}.

pre_after_st(Item, _Children, State) ->
    {Item, State}.

%% helpers

add_var(Name, State = #gen_state{vars = Vars}) ->
    {[], State#gen_state{vars = ordsets:add_element(Name, Vars)}}.

add_stat(Name, Type, Keys, State = #gen_state{stats = Stats}) ->
    Value = {Type, length(Keys)},
    %% TODO assert type
    {[], State#gen_state{stats = orddict:store(Name, Value, Stats)}}.

format_terms(Statements) ->
    lists:flatmap(fun({print_term, Term})        -> format_term(Term, []);
                     ({print_term, Term, Items}) -> format_term(Term, Items);
                     (St)                        -> [St]
                  end, Statements).

format_term(Term, Items) ->
    Str = lists:flatten(io_lib:format("~p.~n", [Term])),
    {ok, Tokens, _} = erl_scan:string(Str, 1, [text]),
    lists:flatmap(fun(Token) -> process_token(Token, Items) end, Tokens).

process_token(Token, Items) ->
    case maybe_token_to_item(Token, Items) of
        {ok, Item} ->
            process_item(Item);
        false ->
            {text, Text} = erl_scan:token_info(Token, text),
            [{printf, Text}]
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

process_item({stat, Format, Stats}) when is_list(Format) ->
    process_item({stat, {Format}, Stats});
process_item({stat, Format, Stats}) when is_atom(Stats) ->
    process_item({stat, Format, [Stats]});
process_item({stat, Format0, Stats}) ->
    Format = [process_format_item(F) || F <- tuple_to_list(Format0)],
    [{printf, "[stat"},
     {printa,
      case Format of
          [F] -> "," ++ F;
          _   -> lists:flatten([",{", sep(Format, ","), "}"])
      end, Stats},
     {printf, "]"}];
process_item({FormatItem, Op}) ->
    [{printf, process_format_item(FormatItem), [Op]}].

process_format_item("%s")  -> "\"%s\"";
process_format_item("%@s") -> "\"%@s\"";
process_format_item(F)     -> F.

merge_printfs(Statements) ->
    lists:foldl(fun maybe_merge/2, [], lists:reverse(Statements)).

maybe_merge({printf, NewText}, [{printf, Text}|Rest]) ->
    [{printf, NewText ++ Text}|Rest];
maybe_merge({printf, NewText}, [{printf, Format, Items}|Rest]) ->
    [{printf, NewText ++ Format, Items}|Rest];
maybe_merge({printf, NewFormat, NewItems}, [{printf, Text}|Rest]) ->
    [{printf, NewFormat ++ Text, NewItems}|Rest];
maybe_merge({printf, NewFormat, NewItems}, [{printf, Format, Items}|Rest]) ->
    [{printf, NewFormat ++ Format, NewItems ++ Items}|Rest];
maybe_merge(St, OutStatements) ->
    [St|OutStatements].

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
             true = ordsets:is_element(Name, Vars),
             ?a2l(Name)
     end, State};
op({Name, Keys}, State = #gen_state{vars = Vars}) when is_atom(Name) ->
    true = ordsets:is_element(Name, Vars),
    {[?a2l(Name), "[", sep_t(op, Keys, ", "), "]"], State};
op(Pid, State = #gen_state{node = Node}) when is_pid(Pid) ->
    {["\"", ?p2l(Node, Pid), "\""], State};
op(Str, State) when is_integer(hd(Str)) ->
    {io_lib:format("~p", [Str]), State};
op(Int, State) when is_integer(Int) ->
    {?i2l(Int), State}.

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
