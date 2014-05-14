%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2014 09:40
%%%-------------------------------------------------------------------
-module(tuple_space).
-author("mike").

%% API
-export([out/1, out/3, in/2, inp/2, rd/2, rdp/2, eval/1, count/2, expired/2]).

%% Definitions
-define(WAIT_MIN, 4).
-define(WAIT_MAX, 6).

-define(RESERVED_SLOT_COUNT, 2).

%%--------------------------------------------------------------------
%% @doc
%% Add a Tuple into the Tuplespace
%%
%% @spec out(Tuple) -> ok
%% @end
%%--------------------------------------------------------------------
-spec(out(Tuple :: tuple()) -> tuple()).

out(Tuple) ->
  %% Augment Tuple with UUID
  Uuid = uuid:to_string(simple, uuid:uuid4()),
  list_to_tuple([Uuid] ++ [false] ++ tuple_to_list(Tuple)).

%%--------------------------------------------------------------------
%% @doc
%% Add a Tuple into the Tuplespace with a TTl after which it will
%% be deleted
%%
%% @spec out(Tuple :: tuple(), Ttl :: integer(), Caller :: pid()) -> tuple()
%% @end
%%--------------------------------------------------------------------
-spec(out(Tuple :: tuple(), Ttl :: integer(), Caller :: pid()) -> tuple()).
out(Tuple, Ttl, Caller) ->
  ToStore = out(Tuple),
  timer:apply_after(Ttl, ?MODULE, expired, [ToStore, Caller]),
  ToStore.

%%--------------------------------------------------------------------
%% @doc
%% Requestes a Tuple be expired from the tuplespace
%%
%% @spec expired(Tuple :: tuple(), Server :: pid()) -> no_return()
%% @end
%%--------------------------------------------------------------------
-spec(expired(Tuple :: tuple(), Server :: pid()) -> no_return()).
expired(Tuple, Server) when is_tuple(Tuple) ->
  Ref = make_ref(),
  Id = {{expired, Tuple}, Ref},
  OnLocked = fun (S, {{expired, T}, _R} = I) ->
    S ! {expired, T},
    unlock(I)
  end,
  lock_then_work(Server, Id, OnLocked).

%%--------------------------------------------------------------------
%% @doc
%% Read a Tuple from the Tuplespace based upon a template
%% This is a blocking call.
%%
%% @spec in(Template, Caller) -> done.
%% @end
%%--------------------------------------------------------------------
-spec in(Template :: tuple(), Caller :: pid()) -> done.

in(Template, Caller) when is_tuple(Template), is_pid(Caller) ->
  MatchHead = make_matchhead(Template),
  TemplateList = tuple_to_list(Template),
  Guard = make_guard(TemplateList),
  Ref = make_ref(),
  selector(in, [any, false] ++ TemplateList, MatchHead, Guard, Caller, Ref).


%%--------------------------------------------------------------------
%% @doc
%% Get a Tuple from the Tuplespace based upon a template
%% This is a non-blocking call so will return null if no match.
%%
%% @spec inp(Template) -> {ok, Tuple} | {ok, null}
%% @end
%%--------------------------------------------------------------------
-spec inp(Template :: tuple(), Caller :: pid()) -> nolock | true.

inp(Template, Caller) when is_tuple(Template), is_pid(Caller) ->
  MatchHead = make_matchhead(Template),
  TemplateList = tuple_to_list(Template),
  Guard = make_guard(TemplateList),
  Ref = make_ref(),
  locate(inp, [any, false] ++ TemplateList, MatchHead, Guard, Caller, Ref).

%%--------------------------------------------------------------------
%% @doc
%% Read a Tuple from the Tuplespace based upon a template but leave it
%% in the Tuplespace
%% This is a blocking call..
%%
%% @spec rd(Template, Caller) -> done.
%% @end
%%--------------------------------------------------------------------
-spec rd(Template :: tuple(), Caller :: pid()) -> done.

rd(Template, Caller) when is_tuple(Template), is_pid(Caller) ->
  Ref = make_ref(),
  MatchHead = make_matchhead(Template),
  TemplateList = tuple_to_list(Template),
  Guard = make_guard(TemplateList),
  selector(rd, [any, false] ++ TemplateList, MatchHead, Guard, Caller, Ref).

%%--------------------------------------------------------------------
%% @doc
%% Read a Tuple from the Tuplespace based upon a template
%% This is a non-blocking call so will return null if no match.
%%
%% @spec rdp(Template) -> {ok, Tuple} | {ok, null}
%% @end
%%--------------------------------------------------------------------
-spec rdp(Template :: term(), Caller :: pid()) -> nolock | true.

rdp(Template, Caller) when is_tuple(Template), is_pid(Caller) ->
  MatchHead = make_matchhead(Template),
  TemplateList = tuple_to_list(Template),
  Guard = make_guard(TemplateList), Ref = make_ref(),
  locate(rdp, [any, false] ++ TemplateList, MatchHead, Guard, Caller, Ref).

%%--------------------------------------------------------------------
%% @doc
%% Execute a Specification of a Tuple and when executed in parallel
%% add the Tuple to the tuplespace
%%
%% @spec eval(Specification, State) -> ok
%% @end
%%--------------------------------------------------------------------
-spec eval(Specification :: tuple()) -> Tuple :: tuple().

eval(Specification) when is_tuple(Specification) ->
  F = fun
    (E) when is_function(E, 0) ->
      E();
    (E) ->
      E
  end,
  L = tuple_to_list(Specification),
  out(list_to_tuple(utilities:pmap(F, L))).

%%--------------------------------------------------------------------
%% @doc
%% Counts the number of Tuples int the Tuplespace that match a Template
%%
%% @spec count(Template) -> Count
%% @end
%%--------------------------------------------------------------------
-spec count(Template :: tuple(), Server :: pid() | port() | {atom(), atom()}) -> nolock | true.

count(Template, Server) when is_tuple(Template), is_pid(Server) ->
  MatchHead = make_matchhead(Template),
  TemplateList = tuple_to_list(Template),
  Guard = make_guard(TemplateList),
  Ref = make_ref(),
  Id = {{count, TemplateList, MatchHead, Guard}, Ref},
  OnLocked = fun (S, {{_M, TL, MH, G}, R} = I) ->
    Matches = query_all(S, MH, G, R, [any, false] ++ TL),
    S ! {self(), done, count, length(Matches)},
    unlock(I)
  end,
  lock_then_work(Server, Id, OnLocked).

%% =========== INTERNAL PRIVATE FUNCTIONS ==============================

selector(Mode, TemplateList, MatchHead, Guard, Server, Ref) ->
  Id = {{Mode, TemplateList, MatchHead, Guard}, Ref},
  OnLocked = fun (S, {{M, TL, MH, G}, R} = I) ->
    case (query_one(S, MH, G, R, TL)) of
      [] ->
        unlock(I),
        random_wait(),
        selector(M, TL, MH, G, S, R);
      [H | _] ->
        Key = lists:nth(1, H),
        S ! {self(), dirty, Key},
        receive
          {dirtied, S} ->
            unlock(I),
            S ! {self(), done, M, list_to_tuple(H)},
            S ! {self(), clean, Key};

          {finished, S} -> done
        end
    end
  end,
  lock_then_work(Server, Id, OnLocked).

random_wait() ->
  WaitPeriod = random:uniform(?WAIT_MAX - ?WAIT_MIN) + ?WAIT_MIN - 1,
  timer:sleep(WaitPeriod).

make_matchhead(Template) ->
  MatchHead = list_to_tuple([list_to_atom("$" ++ integer_to_list(I)) || I <- lists:seq(1, size(Template) + ?RESERVED_SLOT_COUNT)]),
  MatchHead.

find_all(Server, MatchHead, Guard, Ref) ->
  MatchSpec = [{MatchHead, Guard, ['$$']}],
  Server ! {self(), Ref, query, MatchSpec},
  receive
    {selected, Ref, Selections} ->
      Selections
  end.

find_one(Server, MatchHead, Guard, Ref) ->
  MatchSpec = [{MatchHead, Guard, ['$$']}],
  Server ! {self(), Ref, find_one, MatchSpec},
  receive
    {selected, Ref, Selection} ->
      Selection
  end.

make_guard(TemplateList) ->
  Mapper = fun(E, I) -> guard(E, I + ?RESERVED_SLOT_COUNT) end,
  BareGuard = utilities:each_with_index(Mapper, TemplateList),
  Stripper = fun(Elem) -> Elem /= {} end,
  lists:filter(Stripper, BareGuard).

guard(Elem, Index) when integer =:= Elem, is_integer(Index) ->
  {is_integer, list_to_atom("$" ++ integer_to_list(Index))};

guard(Elem, Index) when int =:= Elem, is_integer(Index) ->
  {is_integer, list_to_atom("$" ++ integer_to_list(Index))};

guard(Elem, Index) when float =:= Elem, is_integer(Index) ->
  {is_float, list_to_atom("$" ++ integer_to_list(Index))};

guard(Elem, Index) when binary =:= Elem, is_integer(Index) ->
  {is_binary, list_to_atom("$" ++ integer_to_list(Index))};

guard(Elem, Index) when atom =:= Elem, is_integer(Index) ->
  {is_atom, list_to_atom("$" ++ integer_to_list(Index))};

guard([_Elem], Index) when is_integer(Index) ->
  {is_list, list_to_atom("$" ++ integer_to_list(Index))};

guard({record, Name}, Index) when is_atom(Name), is_integer(Index) ->
  {is_tuple, list_to_atom("$" ++ integer_to_list(Index))};

guard(Elem, Index) when any =:= Elem, is_integer(Index) ->
  {};

guard(Elem, Index) when string =:= Elem, is_integer(Index) ->
  {};

guard(Elem, Index) when is_function(Elem, 1), is_integer(Index) ->
  {};

guard(Elem, Index) when is_integer(Index) ->
  {'==', list_to_atom("$" ++ integer_to_list(Index)), Elem}.

find_all_matches(_FunsList, []) ->
  [];

find_all_matches(FunsList, TupleList) when is_list(FunsList), is_list(TupleList) ->
  Found = true,
  Bail = fun(Tuple) -> match(FunsList, Tuple, Found) end,
  Matches = lists:takewhile(Bail, TupleList),
  Matches.

mapper(Elem) when is_function(Elem, 1) -> Elem;
mapper(Elem) when integer =:= Elem -> fun(I) -> is_integer(I) end;
mapper(Elem) when int =:= Elem -> fun(I) -> is_integer(I) end;
mapper(Elem) when string =:= Elem -> fun(S) -> io_lib:printable_list(S) end;
mapper(Elem) when float =:= Elem -> fun(F) -> is_float(F) end;
mapper(Elem) when binary =:= Elem -> fun(B) -> is_binary(B) end;
mapper(Elem) when atom =:= Elem -> fun(A) -> is_atom(A) end;
mapper(Elem) when any =:= Elem -> fun(_A) -> true end;

mapper([{integer, N}]) ->
  fun(L) ->
    Pred = fun(E) -> is_integer(E) end,
    is_list(L) andalso (N =:= length(L) andalso lists:all(Pred, L))
  end;
mapper([{int, N}]) ->
  fun(L) ->
    Pred = fun(E) -> is_integer(E) end,
    is_list(L) andalso (N =:= length(L) andalso lists:all(Pred, L))
  end;
mapper([{float, N}]) ->
  fun(L) ->
    Pred = fun(E) -> is_float(E) end,
    is_list(L) andalso (N =:= length(L) andalso lists:all(Pred, L))
  end;
mapper([{string, N}]) ->
  fun(L) ->
    Pred = fun(E) -> io_lib:printable_list(E) end,
    is_list(L) andalso (N =:= length(L) andalso lists:all(Pred, L))
  end;
mapper([{binary, N}]) ->
  fun(L) ->
    Pred = fun(E) -> is_binary(E) end,
    is_list(L) andalso (N =:= length(L) andalso lists:all(Pred, L))
  end;
mapper([{atom, N}]) ->
  fun(L) ->
    Pred = fun(E) -> is_atom(E) end,
    is_list(L) andalso (N =:= length(L) andalso lists:all(Pred, L))
  end;
mapper([{any, N}]) ->
  fun(L) ->
    is_list(L) andalso N =:= length(L)
  end;
mapper({record, Name}) -> fun(R) -> is_record(R, Name) end;

mapper(Elem) -> fun(S) -> S =:= Elem end.

funky(TemplateList) ->
  Mapper = fun(E) -> mapper(E) end,
  lists:map(Mapper, TemplateList).

match([], [], Acc) ->
  Acc;

match(TemplateFuns = [TH | TT], TupleList = [H | T], Acc) ->
  case length(TemplateFuns) == length(TupleList) of
    true ->
      Matched = Acc and TH(H),
      match(TT, T, Matched);
    false ->
      Acc and false
  end.

locate(Mode, TemplateList, MatchHead, Guard, Server, Ref) when is_list(TemplateList), is_pid(Server) ->
  Id = {{Mode, TemplateList, MatchHead, Guard}, Ref},
  OnLocked = fun (S, {{M, TL, MH, G}, R} = I) ->
    case query_one(S, MH, G, R, TL) of
      [] ->
        unlock(I),
        S ! {self(), done, M, null},
        receive
          {finished, S} -> done
        end;

      [H | _] ->
        Key = lists:nth(1, H),
        S ! {self(), dirty, Key},
        receive
          {dirtied, S} ->
            unlock(I),
            S ! {self(), done, M, list_to_tuple(H)},
            S ! {self(), clean, Key}
end
    end
  end,
  lock_then_work(Server, Id, OnLocked).

query_all(Server, MatchHead, Guard, Ref, TemplateList) ->
  Selections = find_all(Server, MatchHead, Guard, Ref),
  TemplateFuns = funky(TemplateList),
  Matches = find_all_matches(TemplateFuns, Selections),
  Matches.

query_one(Server, MatchHead, Guard, Ref, TemplateList) ->
  Selections = find_one(Server, MatchHead, Guard, Ref),
  TemplateFuns = funky(TemplateList),
  Matches = find_all_matches(TemplateFuns, Selections),
  Matches.

lock_then_work(Server, Id, OnLocked) when is_function(OnLocked, 2) ->
  case lock(Id) of
    true ->
      OnLocked(Server, Id);

    false ->
      lock_then_work(Server, Id, OnLocked)
  end.

lock(Id) ->
  Nodes = [node()],
  global:set_lock(Id, Nodes, 0).

unlock(Id) ->
  Nodes = [node()],
  global:del_lock(Id, Nodes).