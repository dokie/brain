%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 25. Feb 2014 08:20
%%%-------------------------------------------------------------------
-module(mergesort_factory).
-author("mike").

-behaviour(factory).

%% API
-export([init/1, run/2]).

-spec(init(Options :: list(term())) -> {ok, State :: term()} | tuple(error, Reason :: string())).
init([N]) when is_integer(N), N > 0 ->
  InitialState = N,
  {ok, InitialState}.

-spec(run(From :: pid(), State:: term()) -> no_return()).
run(From, N) when is_pid(From), is_integer(N), N > 0 ->
  F = fun (_E) -> random:uniform(N) end,
  From ! {ran, {mergesort, unsorted, fun () -> lists:map(F, lists:seq(1,N)) end}, N}.
