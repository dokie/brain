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

-define(setup(F), {setup, fun tuple_space_setup/0, fun tuple_space_cleanup/1, F}).

tuple_space_setup() ->
  {ok, Pid} = tuple_space:start_link(),
  Pid.

tuple_space_cleanup(Pid) ->
  tuple_space:stop(Pid).

tuple_space_out_test_() ->
  {"",
   ?setup(fun simple_out/1)}.

simple_out(Pid) ->
  Tuple = {"hello", 1},
  Reply = space_call(Pid, {out, Tuple}),

  [?_assert(erlang:is_process_alive(Pid)),
   ?_assertEqual(ok, Reply)].

space_call(ServerRef, Request) ->
  gen_server:call(ServerRef, Request).