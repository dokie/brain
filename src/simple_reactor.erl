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

-spec(init(Options :: list()) -> ok).
init(_Options) ->
  [{simple, float}].

-spec(react(From :: pid(), Reactants:: list(tuple)) -> no_return()).
react(From, [{simple, X}]) when is_float(X) ->
  Product = {simple_product, X * 2.0},
  From ! {products, [Product]}.