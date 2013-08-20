%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Utility macros and constants
%%%
%%% @end
%%% Created : 26 Jul 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------
-record(gen_state, {st,
                    vars = ordsets:new(),
                    stats = orddict:new(),
                    args = orddict:new(),
                    level = 0}).

-define(a2l(A), atom_to_list(A)).
-define(i2l(I), integer_to_list(I)).
-define(p2l(P), pid_to_list(P)).
-define(l2p(L), list_to_pid(L)).

-define(INDENT, 2).

-define(op_assign, '=').

-define(is_assign1(Op),
        Op == '++').

-define(is_assign2(Op),
        Op == '='; Op == '+='; Op == '-='; Op == '*='; Op == '/='; Op == '%=').

-define(is_logic1(Op),
        Op == '!').

-define(is_logic2(Op),
        Op == '||'; Op == '&&').

-define(is_arith2(Op),
        Op == '+'; Op == '-'; Op == '*'; Op == '/'; Op == '%').

-define(is_arith1(Op),
        Op == '-').

-define(is_cmp(Op),
        Op == '=='; Op == '<='; Op == '>='; Op == '>'; Op == '<'; Op == '!=').
