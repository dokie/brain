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

simple_test() ->
  L = lists:seq(1, 10),
  F = fun (Elem) -> Elem * 2 end,
  Expected = lists:map(F, L),
  Result = utilities:pmap(F, L),
  ?_assertEqual(Expected, Result).
