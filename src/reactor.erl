%%%-------------------------------------------------------------------
%%% @author dokie
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2014 12:04
%%%-------------------------------------------------------------------
-module(reactor).
-author("dokie").

%% API
-export([react_once/0]).

-define(CATALYST, {"input", float}).

react_once() ->
  {"input", X} = tuple_space_server:in(?CATALYST),
  %% React
  Product = reaction(X),
  ok = tuple_space_server:out({"input", Product}),
  ok.

reaction(Y) when is_float(Y) ->
  Y * 2.0.

