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
  Second = ?SERVER:inp(Tuple),
  [?_assertEqual(Tuple, Match),
   ?_assertEqual(undefined, Second)].

match_first_string_in(_Pid) ->
  Tuple = {"Yo yo", 1, 3.14},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:in({string, any, any}),
  [?_assertEqual(Tuple, Match)].

match_second_int_in(_Pid) ->
  Tuple = {"Yo yo", 42, 3.14},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:in({any, integer, any}),
  [?_assertEqual(Tuple, Match)].

match_third_float_in(_Pid) ->
  Tuple = {"Yo yo", 42, 3.14},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:in({any, any, float}),
  [?_assertEqual(Tuple, Match)].

tuple_space_in_timeout_test_() -> {
  timeout, 5,
  [{"Test of in with no exact match",
   ?setup(fun no_match_inexact_in/1)},
   {"Test of in with no exact match at first but appears later",
   ?setup(fun no_match_at_first_in/1)}]}.

no_match_inexact_in(_Pid) ->
  Tuple = {"The Edge", 999, 3.14},
  ok = ?SERVER:out(Tuple),
  [?_assertExit({timeout, _}, ?SERVER:in({"The Edge", 999, 2.17}, 2000))].

no_match_at_first_in(_Pid) ->
  BadTuple = {"The Edge", 999, 3.14},
  GoodTuple = {"The Edge", 999, 2.17},
  ok = ?SERVER:out(BadTuple),
  spawn(fun () ->
    timer:sleep(1000),
    ?SERVER:out(GoodTuple)
    end),
  Match = ?SERVER:in({"The Edge", 999, 2.17}, 1500),
  [?_assertEqual(GoodTuple, Match)].

tuple_space_inp_test_() ->
  [{"Test of inp with exact Tuple",
    ?setup(fun exact_inp/1)},
    {"Test of inp with match of second field as INTEGER",
      ?setup(fun match_second_int_inp/1)},
    {"Test of inp with no match",
      ?setup(fun no_match_inp/1)}].

exact_inp(_Pid) ->
  Tuple = {"Yo yo", 99, <<"BS">>, 1.23},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:inp({"Yo yo", 99, <<"BS">>, 1.23}),
  [?_assertEqual(Tuple, Match)].

match_second_int_inp(_Pid) ->
  Tuple = {"Yo yo", 42, 3.14},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:inp({any, integer, any}),
  [?_assertEqual(Tuple, Match)].

no_match_inp(_Pid) ->
  Tuple = {"The Edge", 999, 3.14},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:inp({any, integer, string}),
  [?_assertEqual(undefined, Match)].

tuple_space_rd_test_() ->
  [{"Test of rd with exact match",
   ?setup(fun exact_rd/1)}].

exact_rd(_Pid) ->
  Tuple = {"Yo yo", 99, <<"BS">>, 1.23},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:rd({"Yo yo", 99, <<"BS">>, 1.23}),
  [?_assertEqual(Tuple, Match)].

tuple_space_guard_test_() ->
  [{"Test out without tuple",
   ?setup(fun out_arg_not_tuple/1)},
   {"Test in without tuple",
   ?setup(fun in_arg_not_tuple/1)},
   {"Test in with tuple but without integer timeout",
   ?setup(fun in_arg_not_timeout/1)},
   {"Test in with tuple but without valid integer timeout",
   ?setup(fun in_arg_not_valid_timeout/1)},
   {"Test inp without tuple",
   ?setup(fun inp_arg_not_tuple/1)},
   {"Test rd without tuple",
   ?setup(fun rd_arg_not_tuple/1)},
   {"Test rd with tuple but without integer timeout",
   ?setup(fun rd_arg_not_timeout/1)},
   {"Test rd with tuple but without valid integer timeout",
   ?setup(fun rd_arg_not_valid_timeout/1)}].

in_arg_not_tuple(_Pid) ->
  NotATuple = <<"Binary">>,
  [?_assertError(function_clause, ?SERVER:in(NotATuple))].

out_arg_not_tuple(_Pid) ->
  NotATuple = [1, 2, 3],
  [?_assertError(function_clause, ?SERVER:out(NotATuple))].

in_arg_not_timeout(_Pid) ->
  Template = {integer, any, float, "Hi"},
  NotATimeout= 1.23,
  [?_assertError(function_clause, ?SERVER:in(Template, NotATimeout))].

in_arg_not_valid_timeout(_Pid) ->
  Template = {integer, any, float, "Hi"},
  NotATValidimeout = -1,
  [?_assertError(function_clause, ?SERVER:in(Template, NotATValidimeout))].

inp_arg_not_tuple(_Pid) ->
  NotATuple = <<"Binary">>,
  [?_assertError(function_clause, ?SERVER:inp(NotATuple))].

rd_arg_not_tuple(_Pid) ->
  NotATuple = "no!",
  [?_assertError(function_clause, ?SERVER:rd(NotATuple))].

rd_arg_not_timeout(_Pid) ->
  Template = {integer, any, float, "Hi"},
  NotATimeout= 1.23,
  [?_assertError(function_clause, ?SERVER:rd(Template, NotATimeout))].

rd_arg_not_valid_timeout(_Pid) ->
  Template = {integer, any, float, "Hi"},
  NotATValidimeout = -1,
  [?_assertError(function_clause, ?SERVER:rd(Template, NotATValidimeout))].