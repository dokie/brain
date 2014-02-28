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
-export([init/1, extract/1]).

-spec(init(Options :: list()) -> ExtractantTemplates :: list(tuple())).
init(_Options) ->
  [{simple_product, float}].

-spec(extract(Extractants :: list(tuple)) -> no_return()).
extract([{simple_product, X}]) when is_float(X) ->
  io:format("Extracted ~p~n", [{simple_product, X}]).