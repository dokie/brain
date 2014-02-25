%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 18. Feb 2014 12:06
%%%-------------------------------------------------------------------
-module(simple_extractor).
-author("mike").

-behaviour(extractor).

%% API
-export([start_link/0]).
-export([init/1, extractants/0, extract/1]).

-spec(init(Options :: list()) -> ok).
init(_Options) ->
  ok.

-spec(extractants() -> list(tuple())).
extractants() ->
  [{simple_product, float}].

-spec(extract(Extractants :: list(tuple)) -> no_return()).
extract([{simple_product, X}]) when is_float(X) ->
  io:format("Extracted ~p~n", [{simple_product, X}]).

-spec(start_link() -> {ok, pid()}).
start_link() ->
  gen_extractor:start_link(simple_extractor, simple_extractor, []).
