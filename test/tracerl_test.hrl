%%%-------------------------------------------------------------------
%%% @author Pawel Chrzaszcz
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Test macros and constants
%%%
%%% @end
%%% Created : 12 Aug 2013 by pawel.chrzaszcz@erlang-solutions.com
%%%-------------------------------------------------------------------

-define(msize(Msg), erts_debug:flat_size(Msg)).

-define(wait_for(Pattern, Timeout),
        receive
            Pattern ->
                ok
        after Timeout ->
                erlang:error(not_found)
        end).
-define(expect(Pattern), ?wait_for(Pattern, 0)).
-define(wait_for(Pattern), ?wait_for(Pattern, 10000)).
-define(expect_not(Pattern),
        receive
            Pattern ->
                erlang:error(found_unexpected)
        after 0 ->
                ok
        end).
