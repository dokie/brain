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
  [{"Test of out with simple Tuple",
   ?setup(fun simple_out/1)},
   {"Test of out with representative Tuple",
    ?setup(fun representative_out/1)}].

simple_out(Pid) ->
  Tuple = {"hello", 1},
  Reply = tuple_space:out(Pid, Tuple),

  [?_assert(erlang:is_process_alive(Pid)),
   ?_assertEqual(ok, Reply)].

representative_out(Pid) ->
  Id = uuid:to_string(uuid:uuid1()),
  Tenant = uuid:to_string(uuid:uuid1()),
  JobId = uuid:to_string(uuid:uuid1()),
  Tuple = {Id, "scalar", "P3", [Tenant, JobId]},
  Reply = tuple_space:out(Pid, Tuple),

  [?_assert(erlang:is_process_alive(Pid)),
    ?_assertEqual(ok, Reply)].

space_call(ServerRef, Request) ->
  gen_server:call(ServerRef, Request).