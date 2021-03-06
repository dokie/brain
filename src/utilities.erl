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
-export([pmap/2, pforeach/2, key/1, each_with_index/2, atom_concat/2, pforeach_with_index/2]).

%%--------------------------------------------------------------------
%% @doc
%% A parallel map function for a list
%%
%% @end
%%--------------------------------------------------------------------
-spec(pmap(F :: fun((E :: any()) -> any()), L :: list()) -> list()).

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

-spec(pforeach(F :: fun((E :: any()) -> any()), L :: list()) -> ok).
pforeach(F, L) when is_function(F), is_list(L) ->
  lists:foreach(fun(Elem) -> spawn(fun() -> catch F(Elem) end) end, L).

-spec(pforeach_with_index(F :: fun((E :: any(), Index :: integer()) -> any()), L :: list()) -> ok).
pforeach_with_index(F, L) when is_function(F, 2), is_list(L) ->
  each_with_index(fun(Elem, Index) -> spawn(fun() -> catch F(Elem, Index) end) end, L).

%%--------------------------------------------------------------------
%% @doc
%% A md5 has key generator as a string
%%
%% @end
%%--------------------------------------------------------------------
-spec(key(T :: tuple()) -> string()).
key(T) when is_tuple(T) ->
  B = term_to_binary(T),
  H = erlang:md5(B),
  hexstring(H).

%%--------------------------------------------------------------------
%% @doc
%% Concatonate 2 atoms into one
%%
%% @end
%%--------------------------------------------------------------------
-spec(atom_concat(First :: atom(), Second :: atom() | integer()) -> atom()).
atom_concat(First, Second) when is_atom(First), is_atom(Second) ->
  list_to_atom(atom_to_list(First) ++ ("_" ++ atom_to_list(Second)));

%%--------------------------------------------------------------------
%% @doc
%% Concatonate an atom and an integer into an atom
%%
%% @end
%%--------------------------------------------------------------------
atom_concat(First, Second) when is_atom(First), is_integer(Second) ->
  list_to_atom(atom_to_list(First) ++ ("_" ++ integer_to_list(Second))).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert a Binary to a Hex String
%%
%% @end
%%--------------------------------------------------------------------
hexstring(<<X:128/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~32.16.0b", [X])).

each_with_index(F, L) when is_function(F, 2), is_list(L) ->
  [
    F(Elem, Index) || {Elem, Index} <- lists:zip(L, lists:seq(1, length(L)))
  ].