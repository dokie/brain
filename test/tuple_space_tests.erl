%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 28. Jan 2014 10:14
%%%-------------------------------------------------------------------
-module(tuple_space_tests).
-author("mike").

-include_lib("eunit/include/eunit.hrl").

tuple_space_setup() -> ok.

tuple_space_cleanup(Pid) -> ok.

tuple_space_fixture_test_() ->
  {foreach,
    fun tuple_space_setup/0,
    fun tuple_space_cleanup/1,
    [{"Simple Test",
      fun simple_test/0}]
  }.

simple_test() ->
  ?assert(true).
