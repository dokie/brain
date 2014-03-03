%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 18. Feb 2014 12:06
%%%-------------------------------------------------------------------
-module(simple_reactor).
-author("mike").

-behaviour(reactor).

%% API
-export([init/1, react/2]).

-spec(init(Options :: list(term())) -> Templates :: list(tuple())).
init(_Options) ->
  [{simple, float}].

-spec(react(From :: atom() | pid() | port() | {atom(),atom()}, Reactants :: [{'simple',float()},...]) -> 'ok').
react(From, [{simple, X}]) when is_float(X) ->
  Product = {simple_product, X * 2.0},
  From ! {products, [Product]},
  ok.