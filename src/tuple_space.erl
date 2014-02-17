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
-export([out/1, in/2, inp/1, rd/2, rdp/1, eval/1, count/1]).

%% Definitions
-define(WAIT, 50).


%%--------------------------------------------------------------------
%% @doc
%% Add a Tuple into the Tuplespace
%%
%% @spec out(Tuple) -> ok
%% @end
%%--------------------------------------------------------------------
-spec(out(Tuple :: term())
      -> ok).

out(Tuple) ->
  %% Augment Tuple with UUID
  Uuid = uuid:to_string(simple,uuid:uuid4()),
  ToStore = list_to_tuple([Uuid] ++ tuple_to_list(Tuple)),
  ets:insert(tuples, ToStore),
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Read a Tuple from the Tuplespace based upon a template
%% This is a blocking call so we can pass a timeout value additionally.
%%
%% @spec in(Template, Caller) -> {ok, Tuple} | {noreply, Template, Timeout}
%% @end
%%--------------------------------------------------------------------
-spec in(Template :: term(), Caller :: pid()) -> {ok, Tuple :: term()}.

in(Template, Caller) ->
  MatchHead = list_to_tuple([list_to_atom("$" ++ integer_to_list(I)) || I <- lists:seq(1, size(Template) + 1)]),
  TemplateList = tuple_to_list(Template),
  Guard = make_guard(TemplateList),
  selector(in, [any] ++ TemplateList, MatchHead, Guard, Caller, []).

%%--------------------------------------------------------------------
%% @doc
%% Get a Tuple from the Tuplespace based upon a template
%% This is a non-blocking call so will return undefined if no match.
%%
%% @spec inp(Template) -> {ok, Tuple} | {ok, undefined}
%% @end
%%--------------------------------------------------------------------
-spec inp(Template :: tuple()) -> {ok, Tuple :: tuple() | undefined}.

inp(Template) when is_tuple(Template) ->
  match(Template).

match(Template) when is_tuple(Template) ->
  MatchHead = list_to_tuple([list_to_atom("$" ++ integer_to_list(I)) || I <- lists:seq(1, size(Template) + 1)]),
  TemplateList = tuple_to_list(Template),
  Guard = make_guard(TemplateList),
  Selections = find_all_selections(MatchHead, Guard),
  %% Second pass to deal with string and functions
  TemplateFuns = funky([any] ++ TemplateList),
  Matches = find_all_matches(TemplateFuns, Selections),
  case Matches of
    [] -> undefined;
    [H | _] ->
      list_to_tuple(tl(H))
  end.

%%--------------------------------------------------------------------
%% @doc
%% Read a Tuple from the Tuplespace based upon a template but leave it
%% in the Tuplespace
%% This is a blocking call so we can pass a timeout value additionally.
%%
%% @spec rd(Template, Caller) -> {ok, Tuple} | {noreply, Template, Timeout}
%% @end
%%--------------------------------------------------------------------
-spec rd(Template :: term(), Caller :: pid()) -> {ok, Tuple :: term()}.

rd(Template, Caller) ->
  MatchHead = list_to_tuple([list_to_atom("$" ++ integer_to_list(I)) || I <- lists:seq(1, size(Template) + 1)]),
  TemplateList = tuple_to_list(Template),
  Guard = make_guard(TemplateList),
  selector(rd, [any] ++ TemplateList, MatchHead, Guard, Caller, []).

%%--------------------------------------------------------------------
%% @doc
%% Read a Tuple from the Tuplespace based upon a template
%% This is a non-blocking call so will return undefined if no match.
%%
%% @spec rdp(Template) -> {ok, Tuple} | {ok, undefined}
%% @end
%%--------------------------------------------------------------------
-spec rdp(Template :: term()) -> {ok, Tuple :: term() | undefined}.

rdp(Template) ->
  match(Template).

%%--------------------------------------------------------------------
%% @doc
%% Execute a Specification of a Tuple and when executed in parallel
%% add the Tuple to the tuplespace
%%
%% @spec eval(Specification, State) -> ok
%% @end
%%--------------------------------------------------------------------
-spec eval(Specification :: tuple()) -> ok.

eval(Specification) when is_tuple(Specification) ->
  F = fun
    (E) when is_function(E, 0) ->
      E();
    (E) ->
      E
  end,
  L = tuple_to_list(Specification),
  Tuple = list_to_tuple(utilities:pmap(F, L)),
  out(Tuple).

%%--------------------------------------------------------------------
%% @doc
%% Counts the number of Tuples int the Tuplespace that match a Template
%%
%% @spec count(Template) -> Count
%% @end
%%--------------------------------------------------------------------
-spec count(Template :: tuple()) -> number().

count(Template) ->
  MatchHead = list_to_tuple([list_to_atom("$" ++ integer_to_list(I)) || I <- lists:seq(1, size(Template) + 1)]),
  TemplateList = tuple_to_list(Template),
  Guard = make_guard(TemplateList),
  Selections = find_all_selections(MatchHead, Guard),
  %% Second pass to deal with string and functions
  TemplateFuns = funky([any] ++ TemplateList),
  Matches = find_all_matches(TemplateFuns, Selections),
  length(Matches).

%% =========== INTERNAL PRIVATE FUNCTIONS ==============================

selector(Mode, TemplateList, MatchHead, Guard, Server, []) ->
  Selections = find_all_selections(MatchHead, Guard),
  %% Second pass to deal with string and functions
  TemplateFuns = funky(TemplateList),
  Matches = find_all_matches(TemplateFuns, Selections),
  timer:sleep(?WAIT),
  selector(Mode, TemplateList, MatchHead, Guard, Server, Matches);

selector(Mode, _TemplateList, _MatchHead, _Guard, Server, [H|_]) ->
  Server ! {self(), done, Mode, list_to_tuple(H)}.

find_all_selections(MatchHead, Guard) ->
  MatchSpec = [{MatchHead, Guard, ['$$']}],
  ets:select(tuples, MatchSpec).

make_guard(TemplateList) ->
  Mapper = fun (E, I) -> guard(E, I + 1) end,
  BareGuard = utilities:each_with_index(Mapper, TemplateList),
  Stripper = fun (Elem) -> Elem /= {} end,
  lists:filter(Stripper, BareGuard).

guard(Elem, Index) when integer =:= Elem, is_integer(Index) ->
  {is_integer, list_to_atom("$" ++ integer_to_list(Index))};

guard(Elem, Index) when int =:= Elem, is_integer(Index) ->
  {is_integer, list_to_atom("$" ++ integer_to_list(Index))};

guard(Elem, Index) when atom =:= Elem, is_integer(Index) ->
  {is_atom, list_to_atom("$" ++ integer_to_list(Index))};

guard(Elem, Index) when float =:= Elem, is_integer(Index) ->
  {is_float, list_to_atom("$" ++ integer_to_list(Index))};

guard(Elem, Index) when binary =:= Elem, is_integer(Index) ->
  {is_binary, list_to_atom("$" ++ integer_to_list(Index))};

guard(Elem, Index) when any =:= Elem, is_integer(Index) ->
  {};

guard(Elem, Index) when string =:= Elem, is_integer(Index) ->
  {};

guard(Elem, Index) when is_function(Elem, 1), is_integer(Index) ->
  {};

guard(Elem, Index) when is_integer(Index) ->
  {'==', list_to_atom("$" ++ integer_to_list(Index)), Elem}.

find_all_matches([], _TupleList) ->
  [];

find_all_matches(_FunsList, []) ->
  [];

find_all_matches(FunsList, TupleList) when is_list(FunsList), is_list(TupleList) ->
  Found = true,
  Bail = fun(Tuple) -> match(FunsList, Tuple, Found) end,
  Matches = lists:takewhile(Bail, TupleList),
  Matches.

mapper(Elem) when is_function(Elem, 1) -> Elem;
mapper(Elem) when integer =:= Elem -> fun (I) -> is_integer(I) end;
mapper(Elem) when int =:= Elem -> fun (I) -> is_integer(I) end;
mapper(Elem) when string =:= Elem -> fun (S) -> io_lib:printable_list(S) end;
mapper(Elem) when float =:= Elem -> fun (F) -> is_float(F) end;
mapper(Elem) when binary =:= Elem -> fun (B) -> is_binary(B) end;
mapper(Elem) when atom =:= Elem -> fun (A) -> is_atom(A) end;
mapper(Elem) when any =:= Elem -> fun (_A) -> true end;
mapper(Elem) -> fun (S) -> S =:= Elem end.

funky(TemplateList) ->
  Mapper = fun (E) -> mapper(E) end,
  lists:map(Mapper, TemplateList).

match([], [], Acc) ->
  Acc;

match(TemplateFuns = [TH|TT], TupleList = [H|T], Acc) ->
  case length(TemplateFuns) == length(TupleList) of
    true ->
      Matched = Acc and TH(H),
      match(TT, T, Matched);
    false ->
      Acc and false
  end.