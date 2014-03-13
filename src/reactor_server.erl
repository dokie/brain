%%%-------------------------------------------------------------------
%%% @author dokie
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 27. Feb 2014 22:12
%%%-------------------------------------------------------------------
-module(reactor_server).
-author("dokie").

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/2, run/2, wait_for_reactants/4]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER(JobName, ReactorName), utilities:atom_concat(JobName, ReactorName)).

-record(state, {job_name, reactor_name, reactor, reactor_state}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(run(JobName :: atom(), ReactorName :: atom()) -> no_return()).

run(JobName, ReactorName) ->
  gen_server:cast(?SERVER(JobName, ReactorName), run).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(JobName :: term(),
  {ReactorName :: atom(), ReactorModule :: module(), Options :: list(tuple())}) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link() ->
  {ok, #state{}}.

start_link(JobName, {ReactorName, ReactorModule, Options}) ->
  gen_server:start_link({local, ?SERVER(JobName, ReactorName)}, ?MODULE,
    [JobName, ReactorName, ReactorModule, Options], []).

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

init([JobName, ReactorName, ReactorModule, Options]) ->
  {ok, ReactantTemplates} = ReactorModule:init(Options),
  {ok, #state{job_name = JobName, reactor_name = ReactorName, reactor = ReactorModule, reactor_state = ReactantTemplates}}.

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

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

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

handle_cast(run,
    S = #state{job_name = JobName, reactor_name = ReactorName, reactor = Reactor, reactor_state = ReactantTemplates}) ->
  start_listener(JobName, ReactorName, Reactor, ReactantTemplates),
  {noreply, S};

handle_cast({react, Reactants},
    S = #state{job_name = JobName, reactor_name = ReactorName, reactor = Reactor, reactor_state = ReactantTemplates}) ->
  Reactor:react(self(), Reactants),
  start_listener(JobName, ReactorName, Reactor, ReactantTemplates),
  {noreply, S};

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

handle_info({products, Products}, State) ->
  OutMap = fun
    (Product) when is_tuple(Product) ->
      tuple_space_server:out(Product)
  end,
  utilities:pmap(OutMap, Products),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

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
-spec(start_listener(JobName :: term(), ReactorName :: term(), Reactor :: module(), ReactantTemplates :: list(tuple()))
      -> no_return()).

start_listener(JobName, ReactorName, Reactor, ReactantTemplates) ->
  spawn_link(?MODULE, wait_for_reactants, [JobName, ReactorName, Reactor, ReactantTemplates]).

-spec(wait_for_reactants(JobName :: term(), ReactorName :: atom(),
    Reactor :: module(), ReactantTemplates :: list(tuple())) -> no_return()).

wait_for_reactants(JobName, ReactorName, Reactor, ReactantTemplates) when is_atom(Reactor), is_list(ReactantTemplates) ->
  InMap = fun
    (ReactantTemplate) when is_tuple(ReactantTemplate) ->
      tuple_space_server:in(ReactantTemplate)
  end,
  Reactants = utilities:pmap(InMap, ReactantTemplates),
  %% Send Reactants to reactor process
  gen_server:cast(?SERVER(JobName, ReactorName), {react, Reactants}).