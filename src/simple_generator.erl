%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 25. Feb 2014 08:20
%%%-------------------------------------------------------------------
-module(simple_generator).
-author("mike").

-behaviour(generator).

%% API
-export([init/1, run/2]).

-spec(init(Options :: list(term())) -> {ok, State :: term()} | tuple(error, Reason :: string())).
init(_Options) ->
  {ok, 0}.

-spec(run(From :: pid(), State:: term()) -> no_return()).
run(From, State) when is_pid(From), is_number(State) ->
  if
    State < 11 ->
      NewState = State + 1,
      From ! {ran, {simple, 1.23 * NewState}, NewState};
    true ->
      ok
  end.
