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

-define(SERVER, tuple_space).
-define(setup(F), {setup, fun tuple_space_setup/0, fun tuple_space_cleanup/1, F}).

tuple_space_setup() ->
  {ok, Pid} = ?SERVER:start_link(),
  Pid.

tuple_space_cleanup(_Pid) ->
  ?SERVER:stop().

tuple_space_out_test_() ->
  [{"Test of out with simple Tuple",
   ?setup(fun simple_out/1)},
   {"Test of out with representative Tuple",
    ?setup(fun representative_out/1)}].

simple_out(Pid) ->
  Tuple = {"hello", 1},
  Reply = ?SERVER:out(Tuple),

  [?_assert(erlang:is_process_alive(Pid)),
   ?_assertEqual(ok, Reply)].

representative_out(Pid) ->
  Id = uuid:to_string(uuid:uuid1()),
  Tenant = uuid:to_string(uuid:uuid1()),
  JobId = uuid:to_string(uuid:uuid1()),
  Tuple = {Id, "scalar", "P3", [Tenant, JobId]},
  Reply = ?SERVER:out(Tuple),

  [?_assert(erlang:is_process_alive(Pid)),
    ?_assertEqual(ok, Reply)].

tuple_space_in_test_() ->
  [{"Test of in with simple Tuple",
   ?setup(fun simple_in/1)}].

simple_in(_Pid) ->
  Tuple = {"Yo yo", 99},
  ok = ?SERVER:out(Tuple),
  {ok, NewTuple} = ?SERVER:in({"Yo, yo", 99}),
  [?_assertEqual(Tuple, NewTuple)].