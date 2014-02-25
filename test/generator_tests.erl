%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 25. Feb 2014 08:11
%%%-------------------------------------------------------------------
-module(generator_tests).
-author("mike").

-include_lib("eunit/include/eunit.hrl").

generate_callback_test() ->
  simple_generator:generate(self(), 0),
  receive
    {generated, Tuple, State} ->
      ?assert(is_tuple(Tuple)),
      ?assertEqual(1, State)
  end.

init_callback_test() ->
  {ok, Result} = simple_generator:init([]),
  ?assertEqual(0, Result).

start_link_test() ->
  {ok, Parent} = simple_generator:start_link(),
  ?assertEqual(false, is_process_alive(Parent)),
  exit(Parent, normal).

generator_test() ->
  meck:new(tuple_space_server),
  meck:expect(tuple_space_server, out, fun(_Tuple) -> ok end),
  {ok, _Parent} = simple_generator:start_link(),
  timer:sleep(100), %% needed to allow all messages to flow before we validate
  ?assert(meck:validate(tuple_space_server)),
  meck:unload(tuple_space_server).
