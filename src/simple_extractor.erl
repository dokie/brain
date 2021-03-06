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
-export([init/1, extract/2]).

-spec(init(Options :: list()) -> {ok, {ExtractantTemplates :: list(tuple()), State :: term()}}).

init(_Options) ->
  {ok, {[{simple_product, float}], ok}}.

-spec(extract(From :: atom() | pid() | port() | {atom(),atom()},
  {Extractants :: [{'simple_product',float()},...], State :: term()}) -> ok).

extract(_From, {[{simple_product, X}], ok}) when is_float(X) ->
  io:format("Extracted ~p~n", [{simple_product, X}]),
  ok.