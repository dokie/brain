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
  Workers = lists:map(fun(Elem) -> spawn(fun() -> execute_and_report(S, F, Elem) end) end, L),
  gather_results(Workers).

gather_results([Worker|T]) when is_pid(Worker) ->
  receive
    {Worker, Res} -> [Res| gather_results(T)]
  end;

gather_results([]) ->
  [].

execute_and_report(Parent, F, Elem) ->
  Parent ! {self(), (catch F(Elem))}.