%%%-------------------------------------------------------------------
%%% @author dokie
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 10. Feb 2014 22:30
%%%-------------------------------------------------------------------
-module(utilities).
-author("dokie").

%% API
-export([pmap/2]).

%%--------------------------------------------------------------------
%% @doc
%% A parallel map function for a list
%%
%% @end
%%--------------------------------------------------------------------
-spec(pmap(F :: fun(() -> any()), L :: list()) -> list()).

pmap(F, L) when is_function(F), is_list(L) ->
  S = self(),
  Workers = lists:map(fun(Elem) -> spawn(fun() -> pmap_f(S, F, Elem) end) end, L),
  pmap_gather(Workers).

pmap_gather([H|T]) ->
  receive
    {H, Res} -> [Res|pmap_gather(T)]
  end;

pmap_gather([]) ->
  [].

pmap_f(Parent, F, Elem) ->
  Parent ! {self(), (catch F(Elem))}.