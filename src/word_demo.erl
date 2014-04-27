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
-export([master_init/2, master_run/1, worker/2, worker_run/2, gather/0, gatherer_run/1]).

-record(counts, {dict}).

master_init(Subsize, Filename) ->
  brain:start(),
  %% Setup Master
    {ok, {Words, M}} = word_generator:init([{filename, Filename}, {subsize, Subsize}]),
  WordList = word_generator:split_words(M, Words, []),
  N = length(WordList) * M,
  LetterFold = fun (Letter, Dict) ->
    _NewDict = dict:update(Letter, fun (Old) -> Old + 1 end, 0, Dict)
  end,
  Letters = [list_to_atom([L]) || L <- lists:seq(97,122)],
  InitialLetterDict = lists:foldl(LetterFold, dict:new(), Letters),
  Counts = #counts{dict = InitialLetterDict},
  ok = tuple_space_server:out({counts, Counts}),
  Total = {total, 0},
  %% Seed the Brain
  ok = tuple_space_server:out(Total),
  {N, WordList}.

master_run(WordList) ->
  AddToBrain = fun (E) -> ok = tuple_space_server:out(E) end,
  tuple_space_server:out(now()),
  utilities:pforeach(AddToBrain, WordList).

worker(M, Identity) ->
  receive
    run ->
      {words, List} = tuple_space_server:in({words, [{string, M}]}),
      io:format("Worker #~p is working.~n", [Identity]),
      LetterFold = fun (Word, Dict) ->
        LowerCaseWord = string:to_lower(Word),
        if hd(LowerCaseWord) > 96 andalso hd(LowerCaseWord) < 123 ->
          Key = list_to_atom([hd(LowerCaseWord)]),
          _NewDict = dict:update_counter(Key, 1, Dict);
          true -> Dict
        end
      end,
      io:format("Worker #~p is updating letter count.~n", [Identity]),
      {_, #counts{dict = CurrentCounts}} = tuple_space_server:in({counts, {record, counts}}),
      UpdatedCounts = lists:foldl(LetterFold, CurrentCounts, List),
      Counts = #counts{dict = UpdatedCounts},
      ok = tuple_space_server:out({counts, Counts}),
      io:format("Worker #~p is updating total count.~n", [Identity]),
      {total, CurrentTotal} = tuple_space_server:in({total, int}),
      ok = tuple_space_server:out({total, CurrentTotal + M}),
      io:format("Worker #~p is re-spawning.~n", [Identity]),
      Worker = spawn(word_demo, worker, [M, Identity]),
      Worker ! run;
      stop -> ok
  end.

worker_run(M, NumWorker) ->
  Workers = [spawn(word_demo, worker, [M, X]) || X <- lists:seq(1,NumWorker)],
  lists:foreach(fun(W) -> W ! run end, Workers).

gather() ->
  receive
    {run, From, N} ->
      tuple_space_server:in({total, N}),
      {counts, #counts{dict = CountDict}} = tuple_space_server:in({counts, {record, counts}}),
      ResetCountDict = dict:map(fun(_K,_V) -> 0 end, CountDict),
      tuple_space_server:out({counts, #counts{dict = ResetCountDict}}),
      tuple_space_server:out({total, 0}),
      Start = tuple_space_server:in({int, int, int}),
      End = now(),
      DiffMS = timer:now_diff(End, Start) / 1000,
      io:format("The operation took: ~p milliseconds~n", [DiffMS]),
      Results = dict:to_list(CountDict),
      PrintResult = fun ({Letter, Count}) ->
        io:format("~p : ~w~n", [Letter, Count])
      end,
      lists:foreach(PrintResult, Results),
      From ! done,
      G = spawn(word_demo, gather, []),
      G ! {run, From, N};
    stop ->
      ok
  end.

gatherer_run(N) ->
  Gatherer = spawn(word_demo, gather, []),
  Gatherer ! {run, self(), N}.


