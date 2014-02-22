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
-export([start/0, stop/1]).
-export([init/1, reactants/0, react/1]).

-spec(init(Options :: list()) -> ok).
init(_Options) ->
  ok.

-spec(reactants() -> list(tuple())).
reactants() ->
  [{simple, float}].

-spec(react(Reactants:: list(tuple)) -> Products :: list(tuple())).
react([{simple, X}]) when is_float(X) ->
  Product = {simple_product, X * 2.0},
  [Product].

-spec(start() -> {ok, pid()}).
start() ->
  gen_reactor:start(simple, simple_reactor, []).

-spec(stop(Server :: pid()) -> ok).
stop(Server) ->
  gen_reactor:stop(Server, simple).