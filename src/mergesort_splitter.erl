%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 18. Feb 2014 12:06
%%%-------------------------------------------------------------------
-module(mergesort_splitter).
-author("mike").

-behaviour(reactor).

%% API
-export([init/1, react/2]).

-spec(init(Options :: list(term())) -> {ok, {Templates :: list(tuple()), State :: term()}}).
init([N, M]) when is_integer(N), is_integer(M), M < N, M > 0 ->
  {ok, {[{mergesort, unsorted, [{int, N}]}], M}}.

-spec(react(From :: atom() | pid() | port() | {atom(),atom()}, {Reactants :: [{mergesort,unsorted, list()},...], State :: term()})
      -> 'ok').
react(From, {[{mergesort, unsorted, L}], M}) when is_list(L), M < length(L), M > 0 ->
  Products = split_list(L, M, []),
  From ! {{products, Products}, M},
  ok.

split_list([], _M, Result) ->
  Result;

split_list(List, M, Result) ->
  {Block, Rest} = lists:split(M, List),
  split_list(Rest, M, [{mergesort, unsorted, Block} | Result]).