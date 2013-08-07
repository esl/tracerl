%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Script generator callback module for systemtap
%%%
%%% @end
%%% Created : 3 Jul 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(dyntrace_gen_systemtap).

-include("dyntrace_util.hrl").

-import(dyntrace_gen_util, [sep/2, sep_t/3, tag/2, sep_f/3]).

-compile(export_all).

-record(state, {name,
                pid,
                stats = orddict:new(),
                multi_keys = orddict:new(),
                level = 0}).

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------
script(ScriptSrc) ->
    dyntrace_gen:script(?MODULE, ScriptSrc).

script(ScriptSrc, NodeOrPidStr) ->
    dyntrace_gen:script(?MODULE, ScriptSrc, NodeOrPidStr).

%%-----------------------------------------------------------------------------
%% dyntrace_gen callbacks - pass 1: preprocess
%%-----------------------------------------------------------------------------
preprocess(Probes, State) ->
    {tag(pre_probe, Probes), State}.

pre_probe({probe, Point, _Predicate, Statements}, State) ->
    pre_probe({probe, Point, Statements}, State);
pre_probe({probe, _Point, Statements}, State) ->
    {tag(pre_st, Statements), State}.

pre_st({printa, _Format, Args}, State) when length(Args) =< 1 ->
    {[], State};
pre_st({printa, _Format, Args}, State = #state{multi_keys = MKeys0}) ->
    MKey = lists:sort(Args),
    MV = sep_f(MKey, "__", fun atom_to_list/1),
    MKeys = orddict:store(MKey, list_to_atom(lists:flatten(MV)), MKeys0),
    {[], State#state{multi_keys = MKeys}};
pre_st(_, State) ->
    {[], State}.

%%-----------------------------------------------------------------------------
%% dyntrace_gen callbacks - pass 2: generate
%%-----------------------------------------------------------------------------
generate(Probes, State) ->
    {[{nop, add_globals, [sep_t(probe, Probes, "\n")]}], State}.

init_state(PidStr) when is_list(PidStr) ->
    Name = os:cmd(io_lib:format(
                    "ps -p ~p -o command | tail -n 1 | awk '{print $1}'",
                    [PidStr])),
    #state{name = re:replace(Name, "\\s*$", "", [{return, list}]),
           pid = PidStr}.

add_globals(Script, State = #state{stats = []}) ->
    {Script, State};
add_globals(Script, State = #state{stats = Stats}) ->
    {["global ", sep_f(orddict:fetch_keys(Stats), ", ", fun atom_to_list/1),
      "\n" | Script],
     State}.

probe({probe, Point, Statements}, State) when not is_list(Point) ->
    {[{probe_point, Point}, " ", {st, {group, Statements}}, "\n"], State};
probe({probe, Point, Predicate, Statements}, State) when not is_list(Point) ->
    {[{probe_point, Point},
      {nop, indent, " {\n"},
      {align, "if ("}, {op, Predicate}, ") ",
      {st_body, {group, Statements}},
      {outdent, "\n}\n"}], State};
probe({probe, Point, Statements}, State) ->
    {[{probe_point, Point},
      {nop, indent, " {\n"},
      {align, "if (pid() == "}, State#state.pid, ") ",
      {st_body, {group, Statements}},
      {outdent, "\n}\n"}], State};
probe({probe, Point, Predicate, Statements}, State) ->
    {[{probe_point, Point},
      {nop, indent, " {\n"},
      {align, "if (pid() == "}, State#state.pid,
      " && (", {op, Predicate}, ")) ",
      {st_body, {group, Statements}},
      {outdent, "\n}\n"}], State}.

probe_point('begin', State) ->
    {"probe begin", State};
probe_point('end', State) ->
    {"probe end", State};
probe_point({tick, N}, State) ->
    {["probe timer.s(", ?i2l(N), ")"], State};
probe_point(Function, State) when is_integer(hd(Function)) ->
    {["probe process(\"", State#state.name, "\").mark(\"", Function, "\")"],
     State}.

probe_predicates([SinglePred]) ->
    op(SinglePred);
probe_predicates(Preds) ->
    op({'&&', Preds}).

st(Item, State) ->
    {[{align, {st_body, Item}}], State}.

st_body({set, Name, Keys}, State = #state{stats = Stats}) ->
    {[?a2l(Name), "[", sep_t(op, Keys, ", "), "] = 1"],
     State#state{stats = orddict:store(Name, {set, length(Keys)}, Stats)}};
st_body({count, Name, Keys}, State) ->
    stat_body({count, Name, Keys, 1}, State);
st_body({Type, Name, Keys, Value}, State)
  when Type == sum; Type == min; Type == max; Type == avg ->
    stat_body({Type, Name, Keys, Value}, State);
st_body({group, Items}, State) ->
    {[{nop, indent, "{\n"},
      sep_t(st, Items, "\n"),
      {nop, outdent, "\n"}, {align, "}"}],
     State};
st_body(exit, State) ->
    {["exit()"], State};
st_body({printa, Format, Args}, State = #state{stats = Stats,
                                               multi_keys = MKeys}) ->
    {Items, KeyNum} = printa_items(Args, Stats),
    ArgSpec = printa_args_spec(Format, Items, KeyNum),
    PrintfFormat = re:replace(Format, "@", "", [{return, list}, global]),
    {["foreach([", sep_keys(KeyNum), "] in ",
      ?a2l(case Args of
               [Arg] -> Arg;
               _     -> orddict:fetch(lists:sort(Args), MKeys)
           end),
      ")\n",
      {indent, outdent,
       [{align, ["printf(", sep([{op,PrintfFormat} | ArgSpec], ", "), ")"]}]}],
     State};
st_body({printf, Format, Args}, State) ->
    {["printf(", sep_t(op, [Format | Args], ", "), ")"], State}.

stat_body({Type, Name, Keys, Value}, State = #state{stats = Stats,
                                                    multi_keys = MKeys}) ->
    MVs = get_multi_vals(Name, MKeys),
    {[?a2l(Name), "[", sep_t(op, Keys, ", "), "] <<< ", {op, Value} |
      [["\n", {st, {set, MV, Keys}}] || MV <- MVs]],
     State#state{stats = orddict:store(Name, {Type, length(Keys)}, Stats)}}.

get_multi_vals(StatName, MKeys) ->
    orddict:fold(fun(MK, MV, MVAcc) ->
                         case lists:member(StatName, MK) of
                             true  -> [MV | MVAcc];
                             false -> MVAcc
                         end
                 end, [], MKeys).

printa_items(Args, Stats) ->
    {RevItems, KeyNumber} =
        lists:foldl(fun(Name, empty) ->
                            {Type, KeyNum} = orddict:fetch(Name, Stats),
                            {[{Name, Type}], KeyNum};
                       (Name, {Items, KeyNum}) ->
                            {Type, KeyNum} = orddict:fetch(Name, Stats),
                            {[{Name, Type}|Items], KeyNum}
                    end, empty, Args),
    {lists:reverse(RevItems), KeyNumber}.

printa_args_spec(Format, Items, KeyNum) ->
    {ArgsSpec, {_, KeyN}} =
        lists:mapfoldl(
          fun([$@|_], {[{Name, Type}|Itms], KeyNo}) ->
                  {["@", ?a2l(Type), "(", ?a2l(Name),
                    "[", sep_keys(KeyNum), "])"],
                   {Itms, KeyNo}};
             (_, {Itms, KeyNo}) when KeyNo =< KeyNum ->
                  {["key", ?i2l(KeyNo)],
                   {Itms, KeyNo+1}}
          end, {Items, 1}, tl(re:split(Format, "%", [{return,list}]))),
    KeyN = KeyNum + 1,
    ArgsSpec.

sep_keys(KeyNum) ->
    sep([["key", ?i2l(KN)] || KN <- lists:seq(1, KeyNum)], ",").

op(Item, State) ->
    {op(Item), State}.

op({'&&', Ops}) ->
    ["(", sep_t(op, Ops, ") && ("), ")"];
op({'||', Ops}) ->
    ["(", sep_t(op, Ops, ") || ("), ")"];
op({'==', Op1, Op2}) ->
    [{op,Op1}, " == ", {op,Op2}];
op({'<', Op1, Op2}) ->
    [{op,Op1}, " < ", {op,Op2}];
op({'>', Op1, Op2}) ->
    [{op,Op1}, " > ", {op,Op2}];
op({'=<', Op1, Op2}) ->
    [{op,Op1}, " <= ", {op,Op2}];
op({'>=', Op1, Op2}) ->
    [{op,Op1}, " >= ", {op,Op2}];
op({arg_str, N}) when is_integer(N), N > 0 ->
    ["user_string($arg",?i2l(N),")"];
op({arg, N}) when is_integer(N), N > 0 ->
    ["$arg",?i2l(N)];
%% op({Func,List}) when is_atom(Func), is_list(List) ->
%%     [?a2l(Func), "(", sep_ops(List, ", "), ")"];
%% op({pid_pred, PidStr}) ->
%%     ["pid() == ", PidStr];
op(Pid) when is_pid(Pid) ->
    ["\"", ?p2l(Pid), "\""];
op(Str) when is_integer(hd(Str)) ->
    io_lib:format("~p", [Str]);
op(Int) when is_integer(Int) ->
    ?i2l(Int).

nop(Item, State) ->
    {Item, State}.

indent(Item, State = #state{level = Level}) ->
    {Item, State#state{level = Level+1}}.

outdent(Item, State = #state{level = Level}) ->
    {Item, State#state{level = Level-1}}.

align(Item, State) ->
    {[lists:duplicate(?INDENT * State#state.level, $ ), Item], State}.
