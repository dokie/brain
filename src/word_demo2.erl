%%%-------------------------------------------------------------------
%%% @author dokie
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 25. Apr 2014 19:37
%%%-------------------------------------------------------------------
-module(word_demo2).
-author("dokie").

%% API
-export([master_init/3, master_run/2, worker/2, worker_run/2, gather/1, gatherer_run/2]).

-record(counts, {dict}).

master_init(Subsize, Filename, NumWorker) ->
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
  lists:foreach(fun (E) -> ok = tuple_space_server:out({counts, Counts, E}) end, lists:seq(1, NumWorker)),
  Total = {total, 0},
  %% Seed the Brain
  ok = tuple_space_server:out(Total),
  {N, WordList}.

master_run(WordList, NumWorker) ->
  AddToBrain = fun (E, Index) -> T = erlang:append_element(E, Index rem NumWorker), ok = tuple_space_server:out(T) end,
  tuple_space_server:out(now()),
  utilities:pforeach_with_index(AddToBrain, WordList).

worker(M, Identity) ->
  LetterFold = fun (Word, Dict) ->
    LowerCaseWord = string:to_lower(Word),
    if hd(LowerCaseWord) > 96 andalso hd(LowerCaseWord) < 123 ->
      Key = list_to_atom([hd(LowerCaseWord)]),
      dict:update_counter(Key, 1, Dict);
      true -> Dict
    end
  end,

  receive
    run ->
        {words, List, Index} = tuple_space_server:in({words, [{string, M}], Identity - 1 }, no_check),
        io:format("[~pS:~pI]", [Identity, Index + 1]),
        {counts, #counts{dict = CurrentCounts}, Identity} = tuple_space_server:in({counts, {record, counts}, Identity}),
        UpdatedCounts = lists:foldl(LetterFold, CurrentCounts, List),
        Counts = #counts{dict = UpdatedCounts},
        ok = tuple_space_server:out({counts, Counts, Identity}),
        {total, CurrentTotal} = tuple_space_server:in({total, int}),
        ok = tuple_space_server:out({total, CurrentTotal + M}),
        io:format("[~pE:~pI]", [Identity, Index + 1]),
        Worker = spawn(word_demo2, worker, [M, Identity]),
        Worker ! run;
     stop -> ok
  end.

worker_run(M, NumWorker) ->
  Workers = [spawn(word_demo2, worker, [M, X]) || X <- lists:seq(1, NumWorker)],
  lists:foreach(fun(W) -> W ! run end, Workers).

gather(NumWorker) ->
  receive
    {run, From, N} ->
      tuple_space_server:in({total, fun(I) -> I >= N end}),
      tuple_space_server:out({total, 0}),

      LetterFold = fun (Letter, Dict) ->
        _NewDict = dict:update(Letter, fun (Old) -> Old + 1 end, 0, Dict)
      end,

      Letters = [list_to_atom([L]) || L <- lists:seq(97,122)],
      InitialLetterDict = lists:foldl(LetterFold, dict:new(), Letters),

      CountDict = lists:foldl(fun (E, Acc) ->
        {counts, #counts{dict = WorkerCounts}, _I} = tuple_space_server:in({counts, {record, counts}, E}),
        dict:merge(fun (_K, V1, V2) -> V1 + V2 end, Acc, WorkerCounts) end,
        InitialLetterDict, lists:seq(1, NumWorker)),

      ResetCountDict = dict:map(fun(_K,_V) -> 0 end, CountDict),
      lists:foreach(fun (E) -> ok = tuple_space_server:out({counts, #counts{dict = ResetCountDict}, E}) end, lists:seq(1, NumWorker)),

      Start = tuple_space_server:in({int, int, int}),
      End = now(),
      DiffMS = timer:now_diff(End, Start) / 1000,
      Results = dict:to_list(CountDict),
      PrintResult = fun ({Letter, Count}) ->
        io:format("~p : ~w~n", [Letter, Count])
      end,
      io:format("~nThe operation took: ~p milliseconds~n", [DiffMS]),
      lists:foreach(PrintResult, Results),
      From ! done,
      G = spawn(word_demo2, gather, [NumWorker]),
      G ! {run, From, N};
    stop ->
      ok
  end.

gatherer_run(N, NumWorker) ->
  Gatherer = spawn(word_demo2, gather, [NumWorker]),
  Gatherer ! {run, self(), N}.
