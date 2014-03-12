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
-export([init/1, create/2]).

-spec(init(Options :: list(term())) -> {ok, State :: term()} | tuple(error, Reason :: string())).
init(_Options) ->
  InitialState = random:seed0(),
  {ok, InitialState}.

-spec(create(From :: pid(), State:: term()) -> no_return()).
create(From, State) when is_pid(From) ->
  {X, NewState} = random:uniform_s(State),
  From ! {created, {random, fun () -> X end}, NewState}.
