%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Script generator callback module for systemtap
%%%
%%% @end
%%% Created : 3 Jul 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(tracerl_gen_systemtap).

-include("tracerl_util.hrl").

-import(tracerl_gen_util, [sep/2, sep_t/3, tag/2, sep_f/3]).

-compile(export_all).

-record(sstate, {name,
                 pid,
                 multi_keys = orddict:new()}).

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------
script(ScriptSrc) ->
    tracerl_gen:script(?MODULE, ScriptSrc).

script(ScriptSrc, NodeOrPidStr) ->
    tracerl_gen:script(?MODULE, ScriptSrc, NodeOrPidStr).

%%-----------------------------------------------------------------------------
%% tracerl_gen callbacks - pass 1: preprocess
%%-----------------------------------------------------------------------------
pre_st({printa, _Format, Args}, State) when length(Args) =< 1 ->
    {[], State};
pre_st({printa, _Format, Args},
       State = #gen_state{st = SState = #sstate{multi_keys = MKeys0}}) ->
    MKey = lists:sort(Args),
    MV = sep_f(MKey, "__", fun atom_to_list/1),
    MKeys = orddict:store(MKey, list_to_atom(lists:flatten(MV)), MKeys0),
    {[], State#gen_state{st = SState#sstate{multi_keys = MKeys}}};
pre_st(_, _) ->
    false.

%%-----------------------------------------------------------------------------
%% tracerl_gen callbacks - pass 2: generate
%%-----------------------------------------------------------------------------
generate(Probes, State) ->
    {[{nop, add_globals, [sep_t(probe, Probes, "\n")]}], State}.

init_state(PidStr) when is_list(PidStr) ->
    Name = os:cmd(io_lib:format(
                    "ps -p ~p -o command | tail -n 1 | awk '{print $1}'",
                    [PidStr])),
    SState = #sstate{name = re:replace(Name, "\\s*$", "", [{return, list}]),
                     pid = PidStr},
    #gen_state{st = SState}.

add_globals(Script, State = #gen_state{stats = [], vars = []}) ->
    {Script, State};
add_globals(Script, State = #gen_state{stats = Stats, vars = Vars}) ->
    {["global ", sep_f(orddict:fetch_keys(Stats) ++ ordsets:to_list(Vars),
                       ", ", fun atom_to_list/1),
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
probe({probe, Point, Statements}, State = #gen_state{st = SState}) ->
    {[{probe_point, Point},
      {nop, indent, " {\n"},
      {align, "if (pid() == "}, SState#sstate.pid, ") ",
      {st_body, {group, Statements}},
      {outdent, "\n}\n"}], State};
probe({probe, Point, Predicate, Statements}, State = #gen_state{st = SState}) ->
    {[{probe_point, Point},
      {nop, indent, " {\n"},
      {align, "if (pid() == "}, SState#sstate.pid,
      " && (", {op, Predicate}, ")) ",
      {st_body, {group, Statements}},
      {outdent, "\n}\n"}], State}.

probe_point('begin', State) ->
    {"probe begin", State};
probe_point('end', State) ->
    {"probe end", State};
probe_point({tick, N}, State) ->
    {["probe timer.s(", ?i2l(N), ")"], State};
probe_point(Function, State = #gen_state{st = SState})
  when is_integer(hd(Function)) ->
    {["probe process(\"", SState#sstate.name, "\").mark(\"", Function, "\")"],
     State}.

st_body({set, Name, Keys}, State = #gen_state{stats = Stats}) ->
    {[?a2l(Name), "[", sep_t(op, Keys, ", "), "] = 1"],
     State#gen_state{stats = orddict:store(Name, {set, length(Keys)}, Stats)}};
st_body({count, Name, Keys}, State) ->
    stat_body({count, Name, Keys, 1}, State);
st_body({Type, Name, Keys, Value}, State)
  when Type == sum; Type == min; Type == max; Type == avg ->
    stat_body({Type, Name, Keys, Value}, State);
st_body({reset, Name}, State) ->
    {["delete ", ?a2l(Name)], State};
st_body({group, Items}, State) ->
    {[{nop, indent, "{\n"},
      sep_t(st, Items, "\n"),
      {nop, outdent, "\n"}, {align, "}"}],
     State};
st_body(exit, State) ->
    {["exit()"], State};
st_body({printa, Format, Args}, State = #gen_state{stats = Stats,
                                                   st = SState}) ->
    {Items, KeyNum} = printa_items(Args, Stats),
    ArgSpec = printa_args_spec(Format, Items, KeyNum),
    PrintfFormat = re:replace(Format, "@", "", [{return, list}, global]),
    {["foreach([", sep_keys(KeyNum), "] in ",
      ?a2l(case Args of
               [Arg] -> Arg;
               _ -> orddict:fetch(lists:sort(Args), SState#sstate.multi_keys)
           end),
      ")\n",
      {indent, outdent,
       [{align, ["printf(", sep([{op,PrintfFormat} | ArgSpec], ", "), ")"]}]}],
     State};
st_body({printf, Format, Args}, State) ->
    {["printf(", sep_t(op, [Format | Args], ", "), ")"], State};
st_body(_, _) ->
    false.

stat_body({Type, Name, Keys, Value}, State = #gen_state{stats = Stats,
                                                    st = SState}) ->
    MVs = get_multi_vals(Name, SState#sstate.multi_keys),
    {[?a2l(Name), "[", sep_t(op, Keys, ", "), "] <<< ", {op, Value} |
      [["\n", {st, {set, MV, Keys}}] || MV <- MVs]],
     State#gen_state{stats = orddict:store(Name, {Type, length(Keys)}, Stats)}}.

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

op({arg_str, N}, State) when is_integer(N), N > 0 ->
    {["user_string($arg",?i2l(N),")"], State};
op({arg, N}, State) when is_integer(N), N > 0 ->
    {["$arg",?i2l(N)], State};
op(Pid, State) when is_pid(Pid) ->
    {["\"", ?p2l(Pid), "\""], State};
op(Str, State) when is_integer(hd(Str)) ->
    {io_lib:format("~p", [Str]), State};
op(Int, State) when is_integer(Int) ->
    {?i2l(Int), State};
op(_, _) ->
    false.
