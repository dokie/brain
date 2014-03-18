%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 18. Feb 2014 12:06
%%%-------------------------------------------------------------------
-module(mergesort_extractor).
-author("mike").

-behaviour(extractor).

%% API
-export([init/1, extract/2]).

-spec(init(Options :: list()) -> {ok, {ExtractantTemplates :: list(tuple()), State :: term()}}).

init([N]) when is_integer(N), N > 0 ->
  {ok, {[{mergesort, merged, [{int, N}]}], N}}.

-spec(extract(From :: atom() | pid() | port() | {atom(),atom()},
  {Extractants :: [{mergesort, merged, list()},...], State :: term()}) -> ok).

extract(_From, {[{mergesort, merged, Results}], N}) when is_integer(N), N > 0, is_list(Results) ->
  lists:foreach(fun (I) -> io:format("~p,", [I]) end, Results),
  io:format("~n",[]),
  ok.