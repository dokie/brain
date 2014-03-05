%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 28. Jan 2014 10:14
%%%-------------------------------------------------------------------
-module(tuple_space_server_tests).
-author("mike").

-include_lib("eunit/include/eunit.hrl").

-define(SERVER, tuple_space_server).

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
   ?setup(fun match_third_float_in/1)},
   {"Test of in with match of third field as fun",
   ?setup(fun match_third_fun_in/1)}].

exact_in(_Pid) ->
  Tuple = {"Yo yo", 99, <<"BS">>, 1.23},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:in({"Yo yo", 99, <<"BS">>, 1.23}),
  Second = ?SERVER:inp(Tuple),
  [?_assertEqual(Tuple, Match),
   ?_assertEqual(null, Second)].

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

match_third_fun_in(_Pid) ->
  Tuple = {"Yo yo", 42, 3.14},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:in({any, any, fun(X) -> X > 3.0 end}),
  [?_assertEqual(Tuple, Match)].

tuple_space_in_timeout_test_() -> {
  timeout, 30,
  [{"Test of in with no exact match",
   ?setup(fun no_match_inexact_in/1)},
   {"Test of in with no exact match at first but appears later",
   ?setup(fun no_match_at_first_in/1)}]}.

no_match_inexact_in(_Pid) ->
  Tuple = {"The Edge", 999, 3.14},
  ok = ?SERVER:out(Tuple),
  [?_assertExit({timeout, _}, ?SERVER:in({"The Edge", 999, 2.17}, 500))].

no_match_at_first_in(_Pid) ->
  BadTuple = {"The Edge", 999, 3.14},
  GoodTuple = {"The Edge", 998, 2.17},
  ok = ?SERVER:out(BadTuple),
  spawn(fun () ->
    timer:sleep(1000),
    ?SERVER:out(GoodTuple)
    end),
  Match = ?SERVER:in({"The Edge", 998, float}),
  [?_assertEqual(GoodTuple, Match)].

tuple_space_in_bad_test_() ->
  [{"Test of in with a bad template fun",
   ?setup(fun bad_template/1)}].

bad_template(_Pid) ->
  Tuple = {"Now", 42},
  ok = ?SERVER:out(Tuple),
  Bad = {any, fun (_X) -> 1/0 end},
  Match = ?SERVER:in(Bad),
  [?_assertEqual(null, Match)].

tuple_space_inp_test_() ->
  [{"Test of inp with exact Tuple",
    ?setup(fun exact_inp/1)},
    {"Test of inp with match of second field as INTEGER",
    ?setup(fun match_second_int_inp/1)},
    {"Test of inp with mismatched template size",
    ?setup(fun mismatch_tuple_inp/1)},
    {"Test of inp with no match",
    ?setup(fun no_match_inp/1)}].

exact_inp(_Pid) ->
  Tuple = {"Yo yo", 99, <<"BS">>, 1.23},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:inp(Tuple),
  Second = ?SERVER:inp(Tuple),
  [?_assertEqual(Tuple, Match),
   ?_assertEqual(null, Second)].

match_second_int_inp(_Pid) ->
  Tuple = {"Yo yo", 42, 3.14},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:inp({any, integer, any}),
  [?_assertEqual(Tuple, Match)].

mismatch_tuple_inp(_Pid) ->
  Tuple = {"Yo yo", 99, <<"BS">>, 1.23},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:inp({string, fun (E) -> E =:= 99 end, binary}),
  [?_assertEqual(null, Match)].

no_match_inp(_Pid) ->
  Tuple = {"The Edge", 999, 3.14},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:inp({any, integer, string}),
  [?_assertEqual(null, Match)].

tuple_space_rd_test_() ->
  [{"Test of rd with exact match",
   ?setup(fun exact_rd/1)},
   {"Test of rd with exact match after none found initially",
   ?setup(fun no_match_at_first_rd/1)}].

exact_rd(_Pid) ->
  Tuple = {"Yo yo", 99, <<"BS">>, 1.23},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:rd(Tuple),
  Second = ?SERVER:rdp(Tuple),
  [?_assertEqual(Tuple, Match),
   ?_assertEqual(Tuple, Second)].

no_match_at_first_rd(_Pid) ->
  BadTuple = {"The Edge", 999, 3.14},
  GoodTuple = {"The Edge", 999, 2.17},
  ok = ?SERVER:out(BadTuple),
  spawn(fun () ->
    timer:sleep(500),
    ?SERVER:out(GoodTuple)
  end),
  Match = ?SERVER:rd({"The Edge", 999, 2.17}, 1500),
  [?_assertEqual(GoodTuple, Match)].

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
   ?setup(fun rd_arg_not_valid_timeout/1)},
   {"Test rdp without tuple",
   ?setup(fun rdp_arg_not_tuple/1)}].

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

rdp_arg_not_tuple(_Pid) ->
  NotATuple = ok,
  [?_assertError(function_clause, ?SERVER:rdp(NotATuple))].

tuple_space_rdp_test_() ->
  [{"Test of rdp with exact Tuple",
    ?setup(fun exact_rdp/1)},
    {"Test of rdp with match of second field as atom",
      ?setup(fun match_second_atom_rdp/1)},
    {"Test of rdp with no match",
      ?setup(fun no_match_rdp/1)}].

exact_rdp(_Pid) ->
  Tuple = {"Good Bye", ok, <<"BS">>, 1.23},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:rdp(Tuple),
  [?_assertEqual(Tuple, Match)].

match_second_atom_rdp(_Pid) ->
  Tuple = {"Ciao", pizza, 3.14},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:rdp({string, atom, any}),
  [?_assertEqual(Tuple, Match)].

no_match_rdp(_Pid) ->
  Tuple = {<<1,2>>, "FFF", []},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:rdp({binary, integer, string}),
  [?_assertEqual(null, Match)].

tuple_space_eval_test_() ->
  [{"Test of simple eval",
   ?setup(fun simple_eval/1)},
   {"Test of coupled eval",
   ?setup(fun coupled_eval/1)}].

simple_eval(_Pid) ->
  Generator = {"roots", fun() -> math:sqrt(4) end, fun () -> math:sqrt(9) end },
  ok = ?SERVER:eval(Generator),
  Match = ?SERVER:rdp({"roots", float, float}),
  [?_assertEqual({"roots", 2.0, 3.0}, Match)].

coupled_eval(_Pid) ->
  Expected = [{"worker", "done"} || _I <- lists:seq(1,10)],
  Generator = fun (I) -> ?SERVER:eval({"worker", fun() -> io:format("Hello ~w~n", [I]), "done" end}) end,
  utilities:pmap(Generator, lists:seq(1,10)),
  Match = utilities:pmap(fun (_E) -> ?SERVER:in({string, "done"}) end, lists:seq(1,10)),
  [?_assertEqual(Expected, Match)].

tuple_space_count_test_() ->
  [{"Test of simple count",
   ?setup(fun simple_count/1)},
   {"Test of complex count",
   ?setup(fun complex_count/1)}].

simple_count(_Pid) ->
  ok = ?SERVER:out({"Hello", "World"}),
  Number = ?SERVER:count({string, string}),
  [?_assertEqual(1, Number)].

complex_count(_Pid) ->
  Expected = 10,
  Generator = fun (I) -> ?SERVER:eval({"worker",
    fun() -> io:format("Hello ~w~n", [I]), "done" end,
    fun() -> I end}) end,
  utilities:pmap(Generator, lists:seq(1,10)),
  Count = ?SERVER:count({string, "done", int}),
  [?_assertEqual(Expected, Count)].

tuple_space_array_record_test_() ->
  [{"Test of simple array",
   ?setup(fun simple_array/1)},
   {"Test of complex array",
   ?setup(fun complex_array/1)},
   {"Test of simple record",
   ?setup(fun simple_record/1)}].

simple_array(_Pid) ->
  L = [1,2,3],
  ok = ?SERVER:out({list, L}),
  {list, List} = ?SERVER:in({list, [{integer, 3}]}),
  [?_assertEqual(L, List)].

complex_array(_Pid) ->
  Tuple = {complex, [this, is, hard], ["so", "is", "this", "one"], [1.1, 2.2], [<<"Yikes">>], [9,8,7,6,5], [1.2, 3, "fred"]},
  ok = ?SERVER:out(Tuple),
  Match = ?SERVER:in({complex, [{atom, 3}], [{string, 4}], [{float, 2}], [{binary, 1}], [{int, 5}], [{any, 3}]}),
  [?_assertEqual(Match, Tuple)].

-record(point_3d, {x, y, z}).

simple_record(_Pid) ->
  Origin = #point_3d{x=0.0, y=0.0, z=0.0},
  ok = ?SERVER:out({coordinate, Origin}),
  {_, Point} = ?SERVER:in({coordinate, {record, point_3d}}),
  [?_assertEqual(Point, Origin)].

tuple_space_locking_test_() ->
  [{"Test of dual inp",
   ?setup(fun locked_inp/1)}].

locked_inp(_Pid) ->
  ok = ?SERVER:out({dual, "Fuel"}),
  Match = utilities:pmap(fun (_E) -> ?SERVER:inp({dual, string}) end, lists:seq(1,5)),
  ?debugFmt("~nMatch:~p~n", [Match]),
  Count = length(lists:filter(fun (E) -> null =:= E end, Match)),
  [?_assertEqual(4, Count)].

