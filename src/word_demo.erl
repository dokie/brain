%%%-------------------------------------------------------------------
%%% @author dokie
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 25. Apr 2014 19:37
%%%-------------------------------------------------------------------
-module(word_demo).
-author("dokie").

%% API
-export([master_init/2, master_run/1, worker/1, worker_run/1, gather/0, gatherer_run/1]).

master_init(Subsize, Filename) ->
  brain:start(),
  %% Setup Master
    {ok, {Words, M}} = word_generator:init([{filename, Filename}, {subsize, Subsize}]),
  WordList = word_generator:split_words(M, Words, []),
  N = length(WordList) * M,
  LetterCounts = [{list_to_atom([L]), 0} || L <- lists:seq(97,122)],
  Total = {total, 0},
  %% Seed the Brain
  ok = tuple_space_server:out(Total),
  AddToBrain = fun (E) -> ok = tuple_space_server:out(E) end,
  lists:foreach(AddToBrain, LetterCounts),
  {N, WordList}.

master_run(WordList) ->
  AddToBrain = fun (E) -> ok = tuple_space_server:out(E) end,
  tuple_space_server:out(now()),
  utilities:pforeach(AddToBrain, WordList).

worker(M) ->
  receive
    run ->
      {words, List} = tuple_space_server:in({words, [{string, M}]}),
      LetterFold = fun (Word, Dict) ->
        if hd(Word) > 64 andalso hd(Word) < 123 ->
          LowerCaseWord = string:to_lower(Word),
          Key = list_to_atom([hd(LowerCaseWord)]),
          _NewDict = dict:update(Key, fun (Old) -> Old + 1 end, 1, Dict);
          true -> Dict
        end
      end,
      InitialLetters = lists:foldl(LetterFold, dict:new(), List),
      InitialLetterCounts = dict:to_list(InitialLetters),
      UpdateLetterCount = fun ({Letter, Increment}) ->
        {Letter, Count} = tuple_space_server:in({Letter, int}),
        ok = tuple_space_server:out({Letter, Count + Increment})
      end,
      lists:foreach(UpdateLetterCount, InitialLetterCounts),
      {total, CurrentTotal} = tuple_space_server:in({total, int}),
      ok = tuple_space_server:out({total, CurrentTotal + M}),
      Worker = spawn(word_demo, worker, [M]),
      Worker ! run;
      stop -> ok
  end.

worker_run(M) ->
  Workers = [spawn(word_demo, worker, [M]) || _X <- lists:seq(1,5)],
  lists:foreach(fun(W) -> W ! run end, Workers).

gather() ->
  receive
    {run, From, N} ->
      tuple_space_server:in({total, N}),
      Letters = [{list_to_atom([L]), int} || L <- lists:seq(97,122)],
      Results = lists:map(fun (L) -> tuple_space_server:in(L) end, Letters),
      PrintAndReset = fun ({Letter, Count}) ->
        tuple_space_server:out({Letter, 0}),
        io:format("~p : ~w~n", [Letter, Count])
      end,
      tuple_space_server:out({total, 0}),
      Start = tuple_space_server:in({int, int, int}),
      End = now(),
      DiffMS = timer:now_diff(End, Start) / 1000,
      io:format("The operation took: ~p milliseconds~n", [DiffMS]),
      lists:foreach(PrintAndReset, Results),
      From ! done,
      G = spawn(word_demo, gather, []),
      G ! {run, From, N};
    stop ->
      ok
  end.

gatherer_run(N) ->
  Gatherer = spawn(word_demo, gather, []),
  Gatherer ! {run, self(), N}.


