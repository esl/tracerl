%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Script generator callback module for systemtap
%%%
%%% @end
%%% Created : 3 Jul 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(dyntrace_gen_systemtap).

-compile(export_all).

-define(a2l(A), atom_to_list(A)).
-define(i2l(I), integer_to_list(I)).
-define(p2l(P), pid_to_list(P)).
-define(INDENT, 2).
-record(state, {name,
                pid,
                stats = orddict:new(),
                level = 0}).

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------
script(ScriptSrc) ->
    dyntrace_gen:script(?MODULE, ScriptSrc).

script(ScriptSrc, NodeOrPidStr) ->
    dyntrace_gen:script(?MODULE, ScriptSrc, NodeOrPidStr).

%%-----------------------------------------------------------------------------
%% dyntrace_gen callbacks
%%-----------------------------------------------------------------------------
probes(Probes, State) ->
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
    {["global ", sep([?a2l(Name) || {Name, _, _} <- Stats], ", "), "\n"|Script],
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

st_body({count, Count, Key}, State = #state{stats = Stats}) ->
    false = lists:keyfind(Key, 1, Stats),
    {[?a2l(Count), "[", {op,Key}, "] <<< 1"],
     State#state{stats = [{Count, count, 1}|Stats]}};
st_body({group, Items}, State) ->
    {[{nop, indent, "{\n"},
      sep_t(st, Items, "\n"),
      {nop, outdent, "\n"}, {align, "}"}],
     State};
st_body(exit, State) ->
    {["exit()"], State};
st_body({printa, Format, Args}, State = #state{stats = Stats}) ->
    {Items = [{Name,_}|_], KeyNum} = printa_items(Args, Stats),
    ArgSpec = printa_args_spec(Format, Items, KeyNum),
    PrintfFormat = re:replace(Format, "@", "", [{return, list}, global]),
    {["foreach([", sep_keys(KeyNum), "] in ", ?a2l(Name), ")\n",
      {indent, outdent,
       [{align, ["printf(", sep([{op,PrintfFormat} | ArgSpec], ", "), ")"]}]}],
     State};
st_body({printf, Format, Args}, State) ->
    {["printf(", sep_t(op, [Format | Args], ", "), ")"], State}.

printa_items(Args, Stats) ->
    lists:foldl(fun(Name, empty) ->
                        {Name, Type, KeyNum} = lists:keyfind(Name, 1, Stats),
                        {[{Name, Type}], KeyNum};
                   (Name, {Items, KeyNum}) ->
                        {Name, Type, KeyNum} = lists:keyfind(Name, 1, Stats),
                        {[{Name, Type}|Items], KeyNum}
                end, empty, Args).

printa_args_spec(Format, Items, KeyNum) ->
    {ArgsSpec, {[], KeyN}} =
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

indent(Item, State = #state{level = Level}) ->
    {Item, State#state{level = Level+1}}.

outdent(Item, State = #state{level = Level}) ->
    {Item, State#state{level = Level-1}}.

nop(Item, State) ->
    {Item, State}.

align(Item, State) ->
    {[lists:duplicate(?INDENT * State#state.level, $ ), Item], State}.

sep(Args, Sep) ->
    sep_f(Args, Sep, fun(Arg) -> Arg end).

sep_t(Tag, Args, Sep) ->
    sep_f(Args, Sep, fun(Arg) -> {Tag, Arg} end).

sep_f([A, B | T], Sep, F) -> [F(A), Sep | sep_f([B|T], Sep, F)];
sep_f([H], _Sep, F)       -> [F(H)];
sep_f([], _Sep, _F)       -> [].
