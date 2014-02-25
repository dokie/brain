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
-export([start_link/0]).
-export([init/1, generate/2]).

-spec(start_link() -> {ok, pid()}).
start_link() ->
  gen_generator:start_link(simple_generator, simple_generator, []).

-spec(init(Options :: list(term())) -> {ok, State :: term()} | tuple(error, Reason :: string())).
init(_Options) ->
  {ok, 0}.

-spec(generate(From :: pid(), State:: term()) -> {generated, Tuple :: tuple()}).
generate(From, State) when is_pid(From), is_number(State) ->
  if
    State < 11 ->
      NewState = State + 1,
      From ! {generated, {simple, 1.23 * NewState}, NewState};
    State > 10 ->
      From ! stop;
    true ->
      From ! ok
  end.
