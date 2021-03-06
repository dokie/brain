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

-spec(init(Options :: list(term())) -> {ok, {Templates :: list(tuple()), State :: term()}}).

init(_Options) ->
  {ok, {[{simple, float}], ok}}.

-spec(react(From :: atom() | pid() | port() | {atom(),atom()}, {Reactants :: [{'simple',float()},...], State :: ok})
      -> ok).

react(From, {[{simple, X}], S}) when is_float(X) ->
  Product = {simple_product, X * 2.0},
  From ! {products, {[Product], S}},
  ok.