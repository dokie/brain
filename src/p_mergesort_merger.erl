%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 18. Feb 2014 12:06
%%%-------------------------------------------------------------------
-module(p_mergesort_merger).
-author("mike").

-behaviour(reactor).

%% API
-export([init/1, react/2]).

-spec(init(Options :: list(term())) -> {ok, {Templates :: list(tuple()), State :: term()}}).
init([N, M]) when is_integer(N), is_integer(M), M < N, M > 0 ->
  {ok, {[{mergesort, sorted, [{int, M}]}], {N, M, []}}}.

-spec(react(From :: atom() | pid() | port() | {atom(),atom()}, {Reactants :: [{mergesort, sorted, list()},...], State :: term()})
      -> 'ok').
react(From, {[{mergesort, sorted, Part}], {N, M, Results}})
  when is_list(Part), M =:= length(Part), M > 0, N > M, is_list(Results) ->
  F = fun () ->
    Merged = lists:merge(Results, Part),
    case N =:= length(Merged) of
      true ->
        Product = {mergesort, merged, Merged},
        From ! {{products, [Product]}, {N, M, []}};
      false ->
        From ! {{products, []}, {N, M, Merged}}
    end
  end,
  spawn_link(F),
  ok.