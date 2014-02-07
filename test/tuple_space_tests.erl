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

-define(INTEGER, fun(I) -> is_integer(I) end).
-define(FLOAT, fun(F) -> is_float(F) end).
-define(BINARY, fun(B) -> is_binary(B) end).
-define(STRING, fun(S) -> io_lib:printable_list(S) end).
-define(ANY, fun(X) -> true end).

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
  [{"Test of in with exact Tuple",
   ?setup(fun exact_in/1)},
   {"Test of in with match of first field as STRING",
   ?setup(fun match_first_string_in/1)},
   {"Test of in with match of second field as INTEGER",
   ?setup(fun match_second_int_in/1)},
   {"Test of in with match of third field as FLOAT",
   ?setup(fun match_third_float_in/1)}].

exact_in(_Pid) ->
  Tuple = {"Yo yo", 99, <<"BS">>, 1.23},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:in({"Yo yo", 99, <<"BS">>, 1.23}),
  [?_assertEqual(Tuple, Match)].

match_first_string_in(_Pid) ->
  Tuple = {"Yo yo", 1, 3.14},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:in({?STRING, ?ANY, ?ANY}),
  [?_assertEqual(Tuple, Match)].

match_second_int_in(_Pid) ->
  Tuple = {"Yo yo", 42, 3.14},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:in({?ANY, ?INTEGER, ?ANY}),
  [?_assertEqual(Tuple, Match)].

match_third_float_in(_Pid) ->
  Tuple = {"Yo yo", 42, 3.14},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:in({?ANY, ?ANY, ?FLOAT}),
  [?_assertEqual(Tuple, Match)].

tuple_space_in_timeout_test_() -> {
  timeout, 3,
  [{"Test of in with no exact match",
   ?setup(fun no_match_inexact_in/1)}]}.

no_match_inexact_in(_Pid) ->
  Tuple = {"The Edge", 999, 3.14},
  ok = ?SERVER:out(Tuple),
  [?_assertExit({timeout, _}, ?SERVER:in({"The Edge", 999, 2.17}, 2000))].