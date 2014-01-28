%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 28. Jan 2014 10:17
%%%-------------------------------------------------------------------
-module(brain_sup_tests).
-author("mike").

-include_lib("eunit/include/eunit.hrl").

brain_sup_setup() -> ok.

brain_sup_cleanup(Pid) -> ok.

brain_sup_fixture_test_() ->
  {foreach,
    fun brain_sup_setup/0,
    fun brain_sup_cleanup/1,
    [{"Simple Test",
      fun simple_test/0}]
  }.

simple_test() ->
  ?assert(true).
