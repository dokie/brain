%%%-------------------------------------------------------------------
%%% @author dokie
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 10. Feb 2014 22:19
%%%-------------------------------------------------------------------
-module(utilities_tests).
-author("dokie").

-include_lib("eunit/include/eunit.hrl").

simple_pmap_test() ->
  L = lists:seq(1, 10),
  F = fun (Elem) -> Elem * 2 end,
  Expected = lists:map(F, L),
  Result = utilities:pmap(F, L),
  ?_assertEqual(Expected, Result).

simple_hash_test() ->
  Tuple = {"Yo", 1, 2.13},
  Key = utilities:key(Tuple),
  Expected = "746c8f9724290fa2afea25debeb1eebb",
  ?_assertEqual(Expected, Key).
