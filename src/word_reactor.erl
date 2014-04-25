%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2014 08:44
%%%-------------------------------------------------------------------
-module(word_reactor).
-author("mike").

-behaviour(reactor).

%% API
-export([init/1, react/2]).

init(Options) ->
  M = proplists:get_value(subsize, Options, 0),
  {ok, [{words, [{string, M}]}], M}.

react(From, {[{words, WordList}], M}) when is_list(WordList), length(WordList) =:= M ->
  LetterFold = fun (Word, Dict) ->
    if hd(Word) > 64 andalso hd(Word) < 123 ->
      LowerCaseWord = string:to_lower(Word),
      Key = list_to_atom([hd(LowerCaseWord)]),
      NewDict = dict:update(Key, fun (Old) -> Old + 1 end, 1, Dict);
      true -> Dict
    end
  end,
  InitialLetters = lists:foldl(LetterFold, dict:new(), List),
  InitialLetterCounts = dict:to_list(InitialLetters),
  UpdateLetterCount = fun ({Letter, Increment}) ->
    {Letter, Count} = tuple_space_server:in({Letter, int}),
    ok = tuple_space_server:out({Letter, Count + Increment})
  end,
  UpdatedCounts = lists:foreach(UpdateLetterCount, InitialLetterCounts),
  {total, CurrentTotal} = tuple_space_server:in({total, int}),
  UpdatedTotal = {total, CurrentTotal + M},
  From ! {products, {UpdatedCounts ++ UpdatedTotal, M}},
  ok.