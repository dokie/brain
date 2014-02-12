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

-record(state, {tuples = [], in_client, rd_client, in_worker, rd_worker, server}).

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
  {ok, #state{tuples = [], server = self()}}.

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
  NewState = State#state{in_client = From},
  process_flag(trap_exit, true),
  Pid = spawn_link(?MODULE, do_in,[Template, NewState]),
  NewState2 = NewState#state{in_worker = Pid},
  {noreply, NewState2};

handle_call({inp, Template}, _From, State) ->
  Reply = do_inp(Template, State),
  {reply, Reply, State};

handle_call({rd, Template}, From, State) ->
  NewState = State#state{rd_client = From},
  process_flag(trap_exit, true),
  Pid = spawn_link(?MODULE, do_rd,[Template, NewState]),
  NewState2 = NewState#state{rd_worker = Pid},
  {noreply, NewState2};

handle_call({rdp, Template}, _From, State) ->
  Reply = do_rdp(Template, State),
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
handle_info({_From, done, in, Tuple}, State) ->
  Client = State#state.in_client,
  NewState = State#state{tuples = lists:delete(Tuple, State#state.tuples), in_client = undefined, in_worker = undefined},
  gen_server:reply(Client, Tuple),
  {noreply, NewState};

handle_info({_From, done, rd, Tuple}, State) ->
  Client = State#state.rd_client,
  NewState = State#state{tuples = lists:delete(Tuple, State#state.tuples), rd_client = undefined, rd_worker = undefined},
  gen_server:reply(Client, Tuple),
  {noreply, NewState};

handle_info({'EXIT', _Pid, normal}, State) ->
  {noreply, State};

handle_info({'EXIT', Pid, _}, State) ->
  InWorker = State#state.in_worker,
  RdWorker = State#state.rd_worker,
  case Pid of
    InWorker ->
      InClient = State#state.in_client,
      InState = State#state{in_client = undefined, in_worker = undefined},
      gen_server:reply(InClient, undefined),
      {noreply, InState};
    RdWorker ->
      RdClient = State#state.rd_client,
      RdState = State#state{rd_client = undefined, rd_worker = undefined},
      gen_server:reply(RdClient, undefined),
      {noreply, RdState}
  end.


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
  NewState = State#state{tuples = [Tuple | State#state.tuples]},
  {ok, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Read a Tuple from the Tuplespace based upon a template
%% This is a blocking call so we can pass a timeout value additionally.
%%
%% @spec do_in(Template, State) -> {ok, Tuple} | {noreply, Template, Timeout}
%% @end
%%--------------------------------------------------------------------
-spec do_in(Template :: term(), State :: #state{}) -> {ok, Tuple :: term()}.

do_in(Template, State) ->
  TemplateList = tuple_to_list(Template),
  TemplateFuns = funky(TemplateList),
  finder(in, TemplateFuns, State#state.server, []).

finder(Mode, TFuns, Server, []) ->
  NewState = ?MODULE:state(),
  Matches = find_all_matches(TFuns, NewState),
  timer:sleep(?WAIT),
  finder(Mode, TFuns, Server, Matches);

finder(Mode, _TFuns, Server, [H|_]) ->
  Server ! {self(), done, Mode, H}.

find_all_matches(TFuns, State) ->
  Found = true,
  Bail = fun(Tuple) -> match(TFuns, tuple_to_list(Tuple), Found) end,
  Matches = lists:takewhile(Bail, State#state.tuples),
  Matches.

mapper(Elem) when is_function(Elem, 1) -> Elem;
mapper(Elem) when integer =:= Elem -> fun (I) -> is_integer(I) end;
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get a Tuple from the Tuplespace based upon a template
%% This is a non-blocking call so will return undefined if no match.
%%
%% @spec do_inp(Template, State) -> {ok, Tuple} | {ok, undefined}
%% @end
%%--------------------------------------------------------------------
-spec do_inp(Template :: term(), State :: #state{}) -> {ok, Tuple :: term() | undefined}.

do_inp(Template, State) ->
  TemplateList = tuple_to_list(Template),
  TemplateFuns = funky(TemplateList),
  Matches = find_all_matches(TemplateFuns, State),
  case Matches of
    [] -> undefined;
    [H|_] -> H
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Read a Tuple from the Tuplespace based upon a template but leave it
%% in the Tuplespace
%% This is a blocking call so we can pass a timeout value additionally.
%%
%% @spec do_rd(Template, State) -> {ok, Tuple} | {noreply, Template, Timeout}
%% @end
%%--------------------------------------------------------------------
-spec do_rd(Template :: term(), State :: #state{}) -> {ok, Tuple :: term()}.

do_rd(Template, State) ->
  TemplateList = tuple_to_list(Template),
  TemplateFuns = funky(TemplateList),
  finder(rd, TemplateFuns, State#state.server, []).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Read a Tuple from the Tuplespace based upon a template
%% This is a non-blocking call so will return undefined if no match.
%%
%% @spec do_rdp(Template, State) -> {ok, Tuple} | {ok, undefined}
%% @end
%%--------------------------------------------------------------------
-spec do_rdp(Template :: term(), State :: #state{}) -> {ok, Tuple :: term() | undefined}.

do_rdp(Template, State) ->
  TemplateList = tuple_to_list(Template),
  TemplateFuns = funky(TemplateList),
  Matches = find_all_matches(TemplateFuns, State),
  case Matches of
    [] -> undefined;
    [H|_] -> H
  end.

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