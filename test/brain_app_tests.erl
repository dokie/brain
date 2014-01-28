%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 28. Jan 2014 10:18
%%%-------------------------------------------------------------------
-module(brain_app_tests).
-author("mike").

-include_lib("eunit/include/eunit.hrl").

brain_app_setup() ->
  ok = application:start(brain).

brain_app_cleanup(_) ->
  ok = application:stop(brain).

brain_app_fixture_test_() ->
  {foreach,
    fun brain_app_setup/0,
    fun brain_app_cleanup/1,
    [{"Do nothing",
     fun do_nothing/0}]
  }.

do_nothing() -> ok.