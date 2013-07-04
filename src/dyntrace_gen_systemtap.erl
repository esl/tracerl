%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Script generator for systemtap
%%%
%%% @end
%%% Created : 3 Jul 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-module(dyntrace_gen_systemtap).

-compile(export_all).

-record(state, {name, pid}).

script(Probes) ->
    State = init_state(),
    sep([probe(Probe, State) || Probe <- Probes], "\n").

init_state() ->
    PidStr = os:getpid(),
    Name = os:cmd(io_lib:format(
                    "ps -p ~p -o command | tail -n 1 | awk '{print $1}'",
                    [PidStr])),
    #state{name = re:replace(Name, "\\s*$", "", [{return, list}]),
           pid = PidStr}.

probe({probe, 'BEGIN', Statements}, _State) ->
    ["probe begin\n", op({group, Statements})];
probe({probe, Functions, Predicates, Statements}, State) ->
    [sep([["probe process(\"", State#state.name,
           "\").mark(\"", Function, "\")\n"] || Function <- Functions], ",") ++
         ["{ if (",
          probe_predicates(
            [{'==', {pid, []}, State#state.pid}|Predicates]),
          ") ", op({group, Statements})], "}\n"].

probe_predicates([SinglePred]) ->
    op(SinglePred);
probe_predicates(Preds) ->
    op({'&&', Preds}).

op({group, Items}) ->
    ["{\n", sep_ops(Items, "\n"), "\n}\n"];
op({action, exit}) ->
    ["exit()"];
op({'&&', Ops}) ->
    ["(", sep_ops(Ops, ") && ("), ")"];
op({'==', Op1, Op2}) ->
    [op(Op1), " == ", op(Op2)];
op({arg_str,N}) ->
    ["user_string($arg",integer_to_list(N),")"];
op({arg,N}) ->
    ["$arg",integer_to_list(N)];
op({Func,List}) when is_atom(Func), is_list(List) ->
    [atom_to_list(Func), "(", sep_ops(List, ", "), ")"];
op(Pid) when is_pid(Pid) ->
    ["\"", pid_to_list(Pid), "\""];
op(Str) when is_integer(hd(Str)) ->
    io_lib:format("~p", [Str]);
op(Int) when is_integer(Int) ->
    integer_to_list(Int).

sep_ops([A,B|T], Sep) -> [op(A),Sep|sep_ops([B|T], Sep)];
sep_ops([H], _Sep)    -> op(H);
sep_ops([], _Sep)     -> [].

sep([A,B|T], Sep) -> [A,Sep|sep([B|T], Sep)];
sep([H], _Sep)    -> H;
sep([], _Sep)     -> [].
