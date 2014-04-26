%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 23. Apr 2014 17:46
%%%-------------------------------------------------------------------
-module(word_generator).
-author("mike").

-behaviour(generator).

%% API
-export([init/1, run/2, split_words/3]).

init(Options) ->
  Filename = proplists:get_value(filename, Options),
  M = proplists:get_value(subsize, Options),
  Words = parse_file(Filename),
  {ok, {Words, M}}.

run(From, {Words, M}) ->
  WordList = split_words(M, Words, []),
  Total = {total, 0},
  Products = [WordList, Total | [{list_to_atom([L]), 0} || L <- lists:seq(97,122)]],
  From ! {ran, Products}.

split_words(M, Words, Acc) when length(Words) < M ->
  L = length(Words),
  Padded = Words ++ lists:duplicate(M-L, "."),
  Acc ++ [{words, Padded}];

split_words(M, Words, Acc) when length(Words) >= M ->
  {ToAcc, Rest} = lists:split(M, Words),
  NewAcc = Acc ++ [{words, ToAcc}],
  split_words(M, Rest, NewAcc).


parse_file(Filename) ->
  case file:open(Filename, read) of
    {ok, IoDevice} ->
      _Words = process_each_line(IoDevice, [])
  end.

process_each_line(IoDevice, WordList) ->
  case io:get_line(IoDevice, "") of
    eof ->
      file:close(IoDevice),
      WordList;
    {error, Reason} ->
      file:close(IoDevice),
      throw(Reason);
    Data ->
      Trimmed = string:strip(Data),
      NewWordList = WordList ++ words(Trimmed),
      process_each_line(IoDevice, NewWordList)
  end.

words(String) ->
  case length(String) of
    0 -> [];
    1 -> [];
    _ ->
      {match, Captures} = re:run(String, "\\b\\w+\\b", [global,{capture,first,list}]),
      [hd(C) || C <- Captures]
  end.
