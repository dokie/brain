%%%-------------------------------------------------------------------
%%% @author dokie
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2014 11:44
%%%-------------------------------------------------------------------
-module(reactor_tests).
-author("dokie").

-include_lib("eunit/include/eunit.hrl").

-define(REACT, reactor).

simple_reactor_test() ->
  meck:new(tuple_space_server),
  meck:expect(tuple_space_server, out,
    fun(_) -> ok end),
  meck:expect(tuple_space_server, in,
    fun({"input", float}) -> {"input", 1.23} end),
  ?assertEqual(ok, ?REACT:react_once()),
  ?assert(meck:called(tuple_space_server, out, [{"input", 2.46}])),
  ?assert(meck:validate(tuple_space_server)),
  meck:unload(tuple_space_server).
