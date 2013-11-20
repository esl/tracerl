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

-import(tracerl_gen_util, [sep/2, sep_t/3, sep_t/4, tag/2, tag/3, sep_f/3,
                           insert_args/2]).

-compile(export_all).

-record(sstate, {name,
                 multi_keys = orddict:new()}).

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------
script(ScriptSrc) ->
    tracerl_gen:script(?MODULE, ScriptSrc).

script(ScriptSrc, Node) ->
    tracerl_gen:script(?MODULE, ScriptSrc, Node).

%%-----------------------------------------------------------------------------
%% tracerl_gen callbacks
%%-----------------------------------------------------------------------------
init_state(State = #gen_state{pid = OSPid}) ->
    Name = os:cmd(io_lib:format(
                    "ps -p ~p -o command | tail -n 1 | awk '{print $1}'",
                    [OSPid])),
    SState = #sstate{name = re:replace(Name, "\\s*$", "", [{return, list}])},
    State#gen_state{st = SState}.

%%-----------------------------------------------------------------------------
%% tracerl_gen callbacks - pass 1: preprocess
%%-----------------------------------------------------------------------------
pre_st({printa, Format, Args},
       State = #gen_state{stats = Stats,
                          st = SState = #sstate{multi_keys = MKeys}})
  when length(Args) > 1 ->
    [true = orddict:is_key(Arg, Stats) || Arg <- Args],
    case printa_key_num(Format) of
        0 ->
            {[], State};
        _ ->
            MKey = lists:sort(Args),
            MV = sep_f(MKey, "__", fun atom_to_list/1),
            MKs = orddict:store(MKey, list_to_atom(lists:flatten(MV)), MKeys),
            {[], State#gen_state{st = SState#sstate{multi_keys = MKs}}}
    end;
pre_st(_, _) ->
    false.

printa_key_num(Format) ->
    length(lists:filter(fun([C|_]) -> C /= $@ end,
                        tl(re:split(Format, "%", [{return,list}])))).

%%-----------------------------------------------------------------------------
%% tracerl_gen callbacks - pass 2: generate
%%-----------------------------------------------------------------------------
generate(Probes, State) ->
    {[{nop, add_globals, [sep_t(probe, after_probe, Probes, "\n")]}], State}.

add_globals(_, Script, State = #gen_state{stats = [], vars = [],
                                          st = #sstate{multi_keys = []}}) ->
    {Script, State};
add_globals(_, Script, State = #gen_state{stats = Stats, vars = Vars,
                                          st = #sstate{multi_keys = MKeys}}) ->
    {["global ", sep_f(orddict:fetch_keys(Stats) ++ ordsets:to_list(Vars)
                       ++ [MV || {_MK, MV} <- orddict:to_list(MKeys)],
                       ", ", fun atom_to_list/1),
      "\n" | Script],
     State}.

probe({probe, Point, Statements}, State) when not is_list(Point) ->
    {[{probe_point, Point}, " ", {st, {group, Statements}}, "\n"], State};
probe({probe, Point, Predicate, Statements}, State) when not is_list(Point) ->
    {[{probe_point, Point},
      " {\n",
      {indent, outdent, [{align, "if ("}, {op, Predicate}, ") ",
                         {st_body, {group, Statements}}]},
      "\n}\n"], State};
probe({probe, Point, Statements}, State = #gen_state{pid = OSPid}) ->
    {[{probe_point, Point},
      " {\n",
      {indent, outdent, [{align, "if (pid() == "}, OSPid, ") ",
                         {st_body, {group, Statements}}]},
      "\n}\n"], State};
probe({probe, Point, Predicate, Statements}, State = #gen_state{pid = OSPid}) ->
    {[{probe_point, Point},
      " {\n",
      {indent, outdent, [{align, "if (pid() == "}, OSPid,
                         " && (", {op, Predicate}, ")) ",
                         {st_body, {group, Statements}}]},
      "\n}\n"], State}.

probe_point('begin', State) ->
    {"probe begin", State};
probe_point('end', State) ->
    {"probe end", State};
probe_point({tick, N}, State) ->
    {["probe timer.s(", ?i2l(N), ")"], State};
probe_point(Function, State = #gen_state{st = SState})
  when is_integer(hd(Function)) ->
    {["probe process(\"", SState#sstate.name, "\").mark(\"", Function, "\")"],
     insert_args(Function, State)}.

st_body({set, Name, Keys}, State) ->
    {[?a2l(Name), "[", sep_t(op, Keys, ", "), "] = 1"], State};
st_body({count, Name, Keys}, State) ->
    stat_body({count, Name, Keys, 1}, State);
st_body({Type, Name, Keys, Value}, State)
  when Type == sum; Type == min; Type == max; Type == avg ->
    stat_body({Type, Name, Keys, Value}, State);
st_body({reset, Names}, State = #gen_state{st = #sstate{multi_keys = MKeys}})
  when is_list(Names) ->
    [H|T] = orddict:fold(fun(KNames, MKName, NamesAcc) ->
                                 case lists:all(fun(N) ->
                                                        lists:member(N, Names)
                                                end, KNames) of
                                     true  -> [MKName | NamesAcc];
                                     false -> NamesAcc
                                 end
                         end, Names, MKeys),
    {sep([{st_body, {reset, H}} |
          [{align, {st_body, {reset, Name}}} || Name <- T]], "\n"), State};
st_body({reset, Name}, State) ->
    {["delete ", ?a2l(Name)], State};
st_body({group, Items}, State) ->
    {["{\n",
      {indent, outdent, [sep_t(st, Items, "\n"), "\n"]},
      {align, "}"}], State};
st_body(exit, State) ->
    {["exit()"], State};
st_body({printa, Format, Args}, State = #gen_state{stats = Stats,
                                                   st = SState}) ->
    {Items, KeyNum} = printa_items(Args, Stats),
    ArgSpec = printa_args_spec(Format, Items, KeyNum),
    PrintfFormat = re:replace(Format, "@", "", [{return, list}, global]),
    Printf = ["printf(", sep([{op, PrintfFormat} | ArgSpec], ", "), ")"],
    {printa_body(KeyNum, Args, Printf, SState#sstate.multi_keys), State};
st_body({printf, Format}, State) ->
    st_body({printf, Format, []}, State);
st_body({printf, Format, Args}, State) ->
    {["printf(", sep_t(op, [Format | Args], ", "), ")"], State};
st_body(_, _) ->
    false.

stat_body({_Type, Name, Keys, Value}, State = #gen_state{st = SState}) ->
    MVs = get_multi_vals(Name, SState#sstate.multi_keys),
    {[?a2l(Name), key_expr(Keys), " <<< ", {op, Value} |
      [["\n", {st, {set, MV, Keys}}] || MV <- MVs]], State}.

key_expr([])   -> "";
key_expr(Keys) -> ["[", sep_t(op, Keys, ", "), "]"].

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
          fun([$@|_], {[{Name, Type}|Itms], KeyNo}) when Type /= set ->
                  {["@", ?a2l(Type),
                    "(", ?a2l(Name), printa_key_expr(KeyNum), ")"],
                   {Itms, KeyNo}};
             (_, {Itms, KeyNo}) when KeyNo =< KeyNum ->
                  {["key", ?i2l(KeyNo)],
                   {Itms, KeyNo+1}}
          end, {Items, 1}, tl(re:split(Format, "%", [{return,list}]))),
    KeyN = KeyNum + 1,
    ArgsSpec.

printa_key_expr(0) -> "";
printa_key_expr(KeyNum) ->
    ["[", sep([["key", ?i2l(KN)] || KN <- lists:seq(1, KeyNum)], ","), "]"].

printa_body(0, _Args, Printf, _MKeys) -> Printf;
printa_body(KeyNum, Args, Printf, MKeys) ->
    ["foreach(", printa_body_key_expr(KeyNum), " in ",
     ?a2l(case Args of
              [Arg] -> Arg;
              _ -> orddict:fetch(lists:sort(Args), MKeys)
          end),
     ")\n",
     {indent, outdent, [{align, Printf}]}].

printa_body_key_expr(1) -> "key1";
printa_body_key_expr(KeyNum) when KeyNum > 1 ->
    ["[", sep([["key", ?i2l(KN)] || KN <- lists:seq(1, KeyNum)], ","), "]"].

op({arg_str, N}, State) when is_integer(N), N > 0 ->
    {["user_string($arg",?i2l(N),")"], State};
op({arg, N}, State) when is_integer(N), N > 0 ->
    {["$arg",?i2l(N)], State};
op(_, _) ->
    false.
