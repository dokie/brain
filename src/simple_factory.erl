%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 25. Feb 2014 08:20
%%%-------------------------------------------------------------------
-module(simple_factory).
-author("mike").

-behaviour(factory).

%% API
-export([init/1, run/2]).

-spec(init(Options :: list(term())) -> {ok, State :: term()} | tuple(error, Reason :: string())).
init(_Options) ->
  InitialState = random:seed0(),
  {ok, InitialState}.

-spec(run(From :: pid(), State:: term()) -> no_return()).
run(From, State) when is_pid(From) ->
  {X, NewState} = random:uniform_s(State),
  From ! {ran, {random, fun () -> X end}, NewState}.
