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
-export([stop/0, out/1, in/1, in/2]).
-export([do_in/2, state/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {tuples = [], client, in_worker, server}).

%%%===================================================================
%%% API
%%%===================================================================

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
-spec(out(Tuple :: tuple()) -> {atom(), term()}).

out(Tuple) ->
  gen_server:call(?SERVER, {out, Tuple}).

%%--------------------------------------------------------------------
%% @doc
%% Gets a Tuple from the Tuplespace that matches a Template
%% Blocking Call
%%
%% @end
%%--------------------------------------------------------------------
-spec(in(Template :: tuple()) -> {term(), tuple()} | {noreply, term(), timeout()}).

in(Template) ->
  gen_server:call(?SERVER, {in, Template}).

-spec(in(Template :: tuple(), Timeout :: timeout()) -> {term(), tuple()} | {noreply, term(), timeout()}).

in(Template, Timeout) ->
  gen_server:call(?SERVER, {in, Template}, Timeout).

%%--------------------------------------------------------------------
%% @doc
%% Gets the Server State
%%
%% @end
%%--------------------------------------------------------------------
-spec(state() -> {term(), tuple()} | {noreply, term(), timeout()}).

state() ->
  gen_server:call(?SERVER, get_state).

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
  NewState = State#state{client = From},
  Pid = spawn_link(?MODULE, do_in,[Template, NewState]),
  NewState2 = NewState#state{in_worker = Pid},
  {noreply, NewState2};

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
handle_info({_From, done, Tuple}, State) ->
  Client = State#state.client,
  NewState = State#state{tuples = lists:delete(Tuple, State#state.tuples), client = undefined, in_worker = undefined},
  gen_server:reply(Client, Tuple),
  {noreply, NewState}.


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
%% @spec do_in(Template) -> {ok, Tuple} | {noreply, Template, Timeout}
%% @end
%%--------------------------------------------------------------------
-spec do_in(Template :: term(), State :: #state{}) -> {ok, Tuple :: term()}.

do_in(Template, State) ->
  TemplateList = tuple_to_list(Template),
  TemplateFuns = funky(TemplateList),
  finder(TemplateFuns, State#state.server, []).

finder(TFuns, Server, []) ->
  Found = true,
  NewState = ?MODULE:state(),
  Bail = fun(Tuple) -> match(TFuns, tuple_to_list(Tuple), Found) end,
  Matches = lists:takewhile(Bail, NewState#state.tuples),
  timer:sleep(50),
  finder(TFuns, Server, Matches);

finder(_TFuns, Server, [H|_]) ->
  Server ! {self(), done, H}.

funky(TemplateList) ->
  Mapper = fun(E) ->
    if
      is_function(E) -> E;
      true -> fun (S) -> S =:= E end
    end
  end,
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