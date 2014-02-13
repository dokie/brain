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

brain_sup_setup() ->
  {ok, Pid} = brain_sup:start_link(),
  Pid.

brain_sup_cleanup(_Pid) ->
  ok.

brain_sup_fixture_test_() ->
  {foreach,
    fun brain_sup_setup/0,
    fun brain_sup_cleanup/1,
    [{"Do nothing",
      fun do_nothing/0}]
  }.

do_nothing() -> ok.