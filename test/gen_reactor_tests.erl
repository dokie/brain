%%%-------------------------------------------------------------------
%%% @author dokie
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2014 11:44
%%%-------------------------------------------------------------------
-module(gen_reactor_tests).
-author("dokie").

-include_lib("eunit/include/eunit.hrl").

-define(REACT, gen_reactor).

reactor_start_test() ->
  meck:new(dummy_reactor),
  meck:expect(dummy_reactor, init, fun(_Opts) -> ok end),
  meck:expect(dummy_reactor, reactants, fun() -> [{input, float}] end),

  meck:new(tuple_space_server),
  meck:expect(tuple_space_server, in, fun({input, float}, _) -> undefined end),

  {ok, _Pid} = ?REACT:start(dummy, dummy_reactor, []),
  ?assert(meck:called(tuple_space_server, in, [{input, float}, _])),
  ?assert(meck:validate(tuple_space_server)),
  ?assert(meck:validate(dummy_reactor)),

  meck:unload(tuple_space_server),
  meck:unload(dummy_reactor).