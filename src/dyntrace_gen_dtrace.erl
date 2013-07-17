%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Script generator for dtrace
%%%
%%% @end
%%% Created : 3 Jul 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(dyntrace_gen_dtrace).

-compile(export_all).

-define(a2l(A), atom_to_list(A)).
-define(i2l(I), integer_to_list(I)).
-define(p2l(P), pid_to_list(P)).
-define(INDENT, 2).
-record(state, {pid, sets = sets:new(), counts = sets:new(), level = 0}).

script(ScriptSrc) ->
    script(ScriptSrc, node()).

script(ScriptSrc, Node) when is_atom(Node) ->
    PidStr = rpc:call(Node, os, getpid, []),
    script(ScriptSrc, PidStr);
script(ScriptSrc, PidStr) ->
    {Script, _State} = process(probes, ScriptSrc, init_state(PidStr)),
    Script.

probes(Probes, State) ->
    {sep_t(probe, Probes, "\n"), State}.

init_state(PidStr) when is_list(PidStr) ->
    #state{pid = PidStr}.

probe({probe, Point, Statements}, State) ->
    {[{probe_point, Point}, " ", {st, {group, Statements}}], State};
probe({probe, Point, Predicate, Statements}, State) ->
    {[{probe_point, Point},
      "\n/ ", {op, Predicate}, " /\n",
      {st, {group, Statements}}], State}.

probe_point('begin', State) ->
    {"BEGIN", State};
probe_point('end', State) ->
    {"END", State};
probe_point({tick, N}, State) ->
    {["tick-", ?i2l(N), "s"], State};
probe_point(Function, State) when is_integer(hd(Function)) ->
    {["erlang", State#state.pid, ":::", Function], State}.

st(Item, State) ->
    {Body, NewState} = st_body(Item, State),
    {lists:duplicate(?INDENT * State#state.level, $ ) ++ Body, NewState}.

st_body({set, Key, Value}, State = #state{sets = Sets}) ->
    {[$@, ?a2l(Key), "[", {op,Value}, "] = sum(0)"],
     State#state{sets = sets:add_element(Key, Sets)}};
st_body({count, Key, Value}, State = #state{counts = Counts}) ->
    {[$@, ?a2l(Key), "[", {op,Value}, "] = count()"],
     State#state{counts = sets:add_element(Key, Counts)}};
st_body({group, Items}, State) ->
    {[{nop, indent, "{\n"}, sep_t(st, Items, ";\n"), {outdent, ";\n}\n"}],
     State};
st_body(exit, State) ->
    {["exit(0)"], State};
st_body({printa, Format, Args}, State = #state{sets = Sets, counts = Counts}) ->
    [] = sets:to_list(sets:intersection(Sets, Counts)),
    Stats = sets:union(Sets, Counts),
    ArgSpec = [printa_arg_spec(Arg, Stats) || Arg <- Args],
    {["printa(", sep([{op,Format} | ArgSpec], ", "), ")"], State};
st_body({printf, Format, Args}, State) ->
    {["printf(", sep_t(op, [Format | Args], ", "), ")"], State}.

printa_arg_spec(Arg, Stats) when is_atom(Arg) ->
    true = sets:is_element(Arg, Stats),
    [$@ | ?a2l(Arg)];
printa_arg_spec(Arg, _Stats) ->
    {op, Arg}.

op(Item, State) ->
    {op(Item), State}.

op({'&&', Ops}) ->
    ["(", sep_t(op, Ops, ") && ("), ")"];
op({'||', Ops}) ->
    ["(", sep_t(op, Ops, ") || ("), ")"];
op({'==', Op1, Op2}) ->
    [{op,Op1}, " == ", {op,Op2}];
op({arg_str, N}) when is_integer(N), N > 0 ->
    ["copyinstr(arg", ?i2l(N-1), ")"];
op({arg, N}) when is_integer(N), N > 0 ->
    ["arg", ?i2l(N-1)];
%% op({Func, List}) when is_atom(Func), is_list(List) ->
%%     [?a2l(Func), "(", sep_ops(List, ", "), ")"];
op(Pid) when is_pid(Pid) ->
    ["\"", ?p2l(Pid), "\""];
op(Str) when is_integer(hd(Str)) ->
    io_lib:format("~p", [Str]);
op(Int) when is_integer(Int) ->
    ?i2l(Int).

sep(Args, Sep) ->
    sep_f(Args, Sep, fun(Arg) -> Arg end).

sep_t(Tag, Args, Sep) ->
    sep_f(Args, Sep, fun(Arg) -> {Tag, Arg} end).

sep_f([A, B | T], Sep, F) -> [F(A), Sep | sep_f([B|T], Sep, F)];
sep_f([H], _Sep, F)       -> [F(H)];
sep_f([], _Sep, _F)       -> [].

process(F, Item, InState) ->
    process(F, nop, Item, InState).

process(PreF, PostF, Item, InState) ->
    {InChildren, State} = ?MODULE:PreF(Item, InState),
    {OutChildren, OutState} = process_list(InChildren, State),
    ?MODULE:PostF(OutChildren, OutState).

process_list(ItemL, InState) ->
    lists:mapfoldl(fun(L, St) when is_list(L) ->
                           process_list(L, St);
                      (I, St) when is_integer(I) ->
                           {I, St};
                      ({F, Item}, St) ->
                           process(F, Item, St);
                      ({PreF, PostF, Item}, St) ->
                           process(PreF, PostF, Item, St)
                   end, InState, ItemL).

indent(Item, State = #state{level = Level}) ->
    {Item, State#state{level = Level+1}}.

outdent(Item, State = #state{level = Level}) ->
    {Item, State#state{level = Level-1}}.

nop(Item, State) ->
    {Item, State}.
