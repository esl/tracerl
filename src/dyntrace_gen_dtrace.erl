%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Script generator callback module for dtrace
%%%
%%% @end
%%% Created : 3 Jul 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(dyntrace_gen_dtrace).

-include("dyntrace_util.hrl").

-import(dyntrace_gen_util, [sep/2, sep_t/3, sep_f/3]).

-compile(export_all).

-record(state, {pid,
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
%% dyntrace_gen callbacks - pass 1: preprocess
%%-----------------------------------------------------------------------------
preprocess(_Probes, State) ->
    {[], State}.

%%-----------------------------------------------------------------------------
%% dyntrace_gen callbacks - pass 2: generate
%%-----------------------------------------------------------------------------
generate(Probes, State) ->
    {sep_t(probe, Probes, "\n"), State}.

init_state(PidStr) when is_list(PidStr) ->
    #state{pid = PidStr}.

probe({probe, Point, Statements}, State) ->
    {[{probe_point, Point}, " ", {st, {group, Statements}}, "\n"], State};
probe({probe, Point, Predicate, Statements}, State) ->
    {[{probe_point, Point},
      "\n/ ", {op, Predicate}, " /\n",
      {st, {group, Statements}}, "\n"], State}.

probe_point('begin', State) ->
    {"BEGIN", State};
probe_point('end', State) ->
    {"END", State};
probe_point({tick, N}, State) ->
    {["tick-", ?i2l(N), "s"], State};
probe_point(Function, State) when is_integer(hd(Function)) ->
    {["erlang", State#state.pid, ":::", Function], State}.

st(Item, State) ->
    {[{align, {st_body, Item}}], State}.

st_body({set, Name, Keys}, State) ->
    stat_body({sum, Name, Keys, "0"}, State);
st_body({count, Name, Keys}, State) ->
    stat_body({count, Name, Keys, ""}, State);
st_body({Type, Name, Keys, Value}, State)
  when Type == sum; Type == min; Type == max; Type == avg ->
    stat_body({Type, Name, Keys, {op, Value}}, State);
st_body({reset, Name}, State) ->
    {["trunc(@", ?a2l(Name), ")"], State};
st_body({group, Items}, State) ->
    {[{nop, indent, "{\n"},
      sep_t(st, Items, ";\n"),
      {nop, outdent, ";\n"}, {align, "}"}],
     State};
st_body(exit, State) ->
    {["exit(0)"], State};
st_body({printa, Format, Args}, State = #state{stats = Stats}) ->
    ArgSpec = [printa_arg_spec(Arg, Stats) || Arg <- Args],
    {["printa(", sep([{op,Format} | ArgSpec], ", "), ")"], State};
st_body({printf, Format, Args}, State) ->
    {["printf(", sep_t(op, [Format | Args], ", "), ")"], State}.

stat_body({Type, Name, Keys, Value}, State = #state{stats = Stats}) ->
    {[$@, ?a2l(Name), "[", sep_t(op, Keys, ", "), "] = ", ?a2l(Type),
      "(", Value, ")"],
     State#state{stats = orddict:store(Name, Type, Stats)}}.

printa_arg_spec(Arg, Stats) when is_atom(Arg) ->
    true = orddict:is_key(Arg, Stats),
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
op({'<', Op1, Op2}) ->
    [{op,Op1}, " < ", {op,Op2}];
op({'>', Op1, Op2}) ->
    [{op,Op1}, " > ", {op,Op2}];
op({'=<', Op1, Op2}) ->
    [{op,Op1}, " <= ", {op,Op2}];
op({'>=', Op1, Op2}) ->
    [{op,Op1}, " >= ", {op,Op2}];
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

nop(Item, State) ->
    {Item, State}.

indent(Item, State = #state{level = Level}) ->
    {Item, State#state{level = Level+1}}.

outdent(Item, State = #state{level = Level}) ->
    {Item, State#state{level = Level-1}}.

align(Item, State) ->
    {[lists:duplicate(?INDENT * State#state.level, $ ), Item], State}.
