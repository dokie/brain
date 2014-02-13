%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 28. Jan 2014 15:04
%%%-------------------------------------------------------------------
-module(tuple_space).
-author("mike").

-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([stop/0, out/1, in/1, in/2, inp/1, rd/1, rd/2, rdp/1, eval/1]).
-export([do_in/2, do_rd/2, state/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(WAIT, 50).

-record(state, {tuples, requests}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop() -> atom()).

stop() ->
  gen_server:call(?SERVER, stop).

%%--------------------------------------------------------------------
%% @doc
%% Places a Tuple into the Tuplespace
%%
%% @end
%%--------------------------------------------------------------------
-spec(out(Tuple :: tuple()) -> {ok}).

out(Tuple) when is_tuple(Tuple) ->
  gen_server:call(?SERVER, {out, Tuple}).

%%--------------------------------------------------------------------
%% @doc
%% Gets a Tuple from the Tuplespace that matches a Template
%% and remmoves it. This is a Blocking Call
%%
%% @end
%%--------------------------------------------------------------------
-spec(in(Template :: tuple()) -> {term(), tuple()} | {noreply, term(), timeout()}).

in(Template) when is_tuple(Template) ->
  gen_server:call(?SERVER, {in, Template}, infinity).

-spec(in(Template :: tuple(), Timeout :: timeout()) -> {term(), tuple()} | {noreply, term(), timeout()}).

in(Template, Timeout) when is_tuple(Template), is_integer(Timeout), Timeout > 0 ->
  gen_server:call(?SERVER, {in, Template}, Timeout).

%%--------------------------------------------------------------------
%% @doc
%% Gets a Tuple from the Tuplespace that matches a Template
%% Non Blocking Call which returns undefined if no Tuple matches
%%
%% @end
%%--------------------------------------------------------------------
-spec(inp(Template :: tuple()) -> {term(), tuple()} | {noreply, term(), timeout()}).

inp(Template) when is_tuple(Template) ->
  gen_server:call(?SERVER, {inp, Template}).

%%--------------------------------------------------------------------
%% @doc
%% Reads a Tuple from the Tuplespace that matches a Template
%% and leaves it there. This is a Blocking Call
%%
%% @end
%%--------------------------------------------------------------------
-spec(rd(Template :: tuple()) -> {term(), tuple()} | {noreply, term(), timeout()}).

rd(Template) when is_tuple(Template) ->
  gen_server:call(?SERVER, {rd, Template}, infinity).

-spec(rd(Template :: tuple(), Timeout :: timeout()) -> {term(), tuple()} | {noreply, term(), timeout()}).

rd(Template, Timeout) when is_tuple(Template), is_integer(Timeout), Timeout > 0 ->
  gen_server:call(?SERVER, {rd, Template}, Timeout).

%%--------------------------------------------------------------------
%% @doc
%% Reads a Tuple from the Tuplespace that matches a Template
%% and leaves it there - Blocking Call which returns undefined if no Tuple matches
%%
%% @end
%%--------------------------------------------------------------------
-spec(rdp(Template :: tuple()) -> {term(), tuple()} | {noreply, term(), timeout()}).

rdp(Template) when is_tuple(Template) ->
  gen_server:call(?SERVER, {rdp, Template}).

%%--------------------------------------------------------------------
%% @doc
%% Evaluates a tuple of fun's to produce an actual Tuple to store in the
%% Tuplespace. The execution is done in parallel.
%%
%% @end
%%--------------------------------------------------------------------
-spec(eval(Specification :: tuple()) -> {ok} | {noreply, term(), timeout()}).

eval(Specification) when is_tuple(Specification) ->
  gen_server:call(?SERVER, {eval, Specification}).

%%--------------------------------------------------------------------
%% @doc
%% Gets the Server State
%%
%% @end
%%--------------------------------------------------------------------
-spec(state() -> {term(), tuple()} | {noreply, term(), timeout()}).

state() ->
  gen_server:call(?SERVER, get_state).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{tuples = ets:new(tuples, [bag, named_table]),
              requests = ets:new(requests, [bag, named_table])}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call({out, Tuple}, _From, State) ->
  {Reply, NewState} = do_out(Tuple, State),
  {reply, Reply, NewState};

handle_call({in, Template}, From, State) ->
  process_flag(trap_exit, true),
  Pid = spawn_link(?MODULE, do_in, [Template, self()]),
  ets:insert(requests, {Pid, From}),
  {noreply, State};

handle_call({inp, Template}, _From, State) ->
  Reply = do_inp(Template),
  {reply, Reply, State};

handle_call({rd, Template}, From, State) ->
  process_flag(trap_exit, true),
  Pid = spawn_link(?MODULE, do_rd, [Template, self()]),
  ets:insert(requests, {Pid, From}),
  {noreply, State};

handle_call({rdp, Template}, _From, State) ->
  Reply = do_rdp(Template),
  {reply, Reply, State};

handle_call({eval, Specification}, _From, State) ->
  {Reply, NewState} = do_eval(Specification, State),
  {reply, Reply, NewState};

handle_call(stop, _From, State) ->
  {stop, normal,ok, State};

handle_call(get_state, _From, State) ->
  {reply, State, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_info({Worker, done, in, Tuple}, State) ->
  Request = ets:lookup(requests, Worker),
  ets:delete(tuples, element(1, Tuple)), %% Remove from Tuplespace
  Stripped = list_to_tuple(tl(tuple_to_list(Tuple))), %% Strip off UUID
  reply_to_request(Request, State, Stripped),
  {noreply, State};

handle_info({Worker, done, rd, Tuple}, State) ->
  Request = ets:lookup(requests, Worker),
  Stripped = list_to_tuple(tl(tuple_to_list(Tuple))), %% Strip off UUID
  reply_to_request(Request, State, Stripped),
  {noreply, State};

handle_info({'EXIT', _Pid, normal}, State) ->
  {noreply, State};

handle_info({'EXIT', Pid, _}, State) ->
  InRequest = ets:lookup(requests, Pid),
  ok = reply_to_request(InRequest, State, undefined),
  {noreply, State}.

reply_to_request([], _State, _Reply) ->
  cleanup_request([], _State);

reply_to_request([{Worker, Client}], State, Reply) ->
  cleanup_request([{Worker, Client}], State),
  gen_server:reply(Client, Reply),
  ok.

cleanup_request([], _State) ->
  ok;

cleanup_request([{Worker, _}], _State) ->
  ets:delete(requests, Worker),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add a Tuple into the Tuplespace
%%
%% @spec do_out(Tuple) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(do_out(Tuple :: term(), State :: #state{})
      -> {ok, NewState :: #state{}}).

do_out(Tuple, State) ->
  %% Augment Tuple with UUID
  Uuid = uuid:to_string(simple,uuid:uuid4()),
  ToStore = list_to_tuple([Uuid] ++ tuple_to_list(Tuple)),
  ets:insert(tuples, ToStore),
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Read a Tuple from the Tuplespace based upon a template
%% This is a blocking call so we can pass a timeout value additionally.
%%
%% @spec do_in(Template, Server) -> {ok, Tuple} | {noreply, Template, Timeout}
%% @end
%%--------------------------------------------------------------------
-spec do_in(Template :: term(), Server :: pid()) -> {ok, Tuple :: term()}.

do_in(Template, Server) ->
  MatchHead = list_to_tuple([list_to_atom("$" ++ integer_to_list(I)) || I <- lists:seq(1, size(Template) + 1)]),
  TemplateList = tuple_to_list(Template),
  Guard = make_guard(TemplateList),
  finder(in, MatchHead, Guard, Server, []).

finder(Mode, MatchHead, Guard, Server, []) ->
  Matches = find_all_matches(MatchHead, Guard),
  timer:sleep(?WAIT),
  finder(Mode, MatchHead, Guard, Server, Matches);

finder(Mode, _MatchHead, _Guard, Server, [H|_]) ->
  Server ! {self(), done, Mode, list_to_tuple(H)}.

find_all_matches(MatchHead, Guard) ->
  MatchSpec = [{MatchHead, Guard, ['$$']}],
  ets:select(tuples, MatchSpec).

make_guard(TemplateList) ->
  Mapper = fun (E, I) -> match_spec(E, I + 1) end,
  utilities:each_with_index(Mapper, TemplateList).

match_spec(Elem, Index) when integer =:= Elem, is_integer(Index) ->
  {is_integer, list_to_atom("$" ++ integer_to_list(Index))};

match_spec(Elem, Index) when int =:= Elem, is_integer(Index) ->
  {is_integer, list_to_atom("$" ++ integer_to_list(Index))};

match_spec(Elem, Index) when atom =:= Elem, is_integer(Index) ->
  {is_atom, list_to_atom("$" ++ integer_to_list(Index))};

match_spec(Elem, Index) when float =:= Elem, is_integer(Index) ->
  {is_float, list_to_atom("$" ++ integer_to_list(Index))};

match_spec(Elem, Index) when binary =:= Elem, is_integer(Index) ->
  {is_binary, list_to_atom("$" ++ integer_to_list(Index))};

match_spec(Elem, Index) when is_integer(Index) ->
  {'==', list_to_atom("$" ++ integer_to_list(Index)), Elem}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get a Tuple from the Tuplespace based upon a template
%% This is a non-blocking call so will return undefined if no match.
%%
%% @spec do_inp(Template) -> {ok, Tuple} | {ok, undefined}
%% @end
%%--------------------------------------------------------------------
-spec do_inp(Template :: tuple()) -> {ok, Tuple :: tuple() | undefined}.

do_inp(Template) when is_tuple(Template) ->
  do_match(Template).

do_match(Template) when is_tuple(Template) ->
  MatchHead = list_to_tuple([list_to_atom("$" ++ integer_to_list(I)) || I <- lists:seq(1, size(Template) + 1)]),
  TemplateList = tuple_to_list(Template),
  Guard = make_guard(TemplateList),
  Matches = find_all_matches(MatchHead, Guard),
  case Matches of
    [] -> undefined;
    [H | _] ->
      list_to_tuple(tl(H))
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Read a Tuple from the Tuplespace based upon a template but leave it
%% in the Tuplespace
%% This is a blocking call so we can pass a timeout value additionally.
%%
%% @spec do_rd(Template, Server) -> {ok, Tuple} | {noreply, Template, Timeout}
%% @end
%%--------------------------------------------------------------------
-spec do_rd(Template :: term(), Server :: pid()) -> {ok, Tuple :: term()}.

do_rd(Template, Server) ->
  MatchHead = list_to_tuple([list_to_atom("$" ++ integer_to_list(I)) || I <- lists:seq(1, size(Template) + 1)]),
  TemplateList = tuple_to_list(Template),
  Guard = make_guard(TemplateList),
  finder(rd, MatchHead, Guard, Server, []).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Read a Tuple from the Tuplespace based upon a template
%% This is a non-blocking call so will return undefined if no match.
%%
%% @spec do_rdp(Template) -> {ok, Tuple} | {ok, undefined}
%% @end
%%--------------------------------------------------------------------
-spec do_rdp(Template :: term()) -> {ok, Tuple :: term() | undefined}.

do_rdp(Template) ->
  do_match(Template).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Execute a Specification of a Tuple and when executed in parallel
%% add the Tuple to the tuplespace
%%
%% @spec do_eval(Specification, State) -> ok
%% @end
%%--------------------------------------------------------------------
-spec do_eval(Specification :: tuple(), State :: #state{}) -> {ok, NewState :: #state{}}.

do_eval(Specification, State) when is_tuple(Specification) ->
  F = fun
    (E) when is_function(E, 0) ->
      E();
    (E) ->
      E
  end,
  L = tuple_to_list(Specification),
  Tuple = list_to_tuple(utilities:pmap(F, L)),
  do_out(Tuple, State).