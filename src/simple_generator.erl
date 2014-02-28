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
-export([init/1, generate/2]).

-spec(init(Options :: list(term())) -> {ok, State :: term()} | tuple(error, Reason :: string())).
init(_Options) ->
  {ok, 0}.

-spec(generate(From :: pid(), State:: term()) -> no_return()).
generate(From, State) when is_pid(From), is_number(State) ->
  if
    State < 11 ->
      NewState = State + 1,
      From ! {generated, {simple, 1.23 * NewState}, NewState};
    true ->
      ok
  end.
