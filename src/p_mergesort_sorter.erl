%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 18. Feb 2014 12:06
%%%-------------------------------------------------------------------
-module(p_mergesort_sorter).
-author("mike").

-behaviour(reactor).

%% API
-export([init/1, react/2]).

-spec(init(Options :: list(term())) -> {ok, {Templates :: list(tuple()), State :: term()}}).
init([M]) when is_integer(M), M > 0 ->
  {ok, {[{mergesort, unsorted, [{int, M}]}], M}}.

-spec(react(From :: atom() | pid() | port() | {atom(),atom()}, {Reactants :: [{mergesort, unsorted, list()},...], State :: term()})
      -> 'ok').
react(From, {[{mergesort, unsorted, L}], M}) when is_list(L), M =:= length(L), M > 0 ->
  F = fun () ->
    Sorted = lists:sort(L),
    Product = {mergesort, sorted, Sorted},
    From ! {{products, [Product]}, length(L)}
    end,
  spawn_link(F),
  ok.