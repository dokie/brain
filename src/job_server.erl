%%%-------------------------------------------------------------------
%%% @author dokie
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 08. Mar 2014 20:25
%%%-------------------------------------------------------------------
-module(job_server).
-author("dokie").

-behaviour(gen_server).

%% API
-export([start_link/3]).
-export([build_factory/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER(JobName), utilities:atom_concat(JobName, ?MODULE)).

-define(SHELL_FACTORY_SPEC(JobName),
  {factory_sup,
    {factory_sup, start_link, [JobName]},
    permanent,
    infinity,
    supervisor,
    [factory_sup]}).


-record(state, {factory_sup, refs, job_name, job_spec}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(build_factory(JobName :: atom(), {FactoryName :: atom(), FactoryModule :: module(),
  FactoryOpts :: list(term())}) -> {ok, pid()}).
build_factory(JobName, Factory = {_FactoryName, _FactoryModule, _FactoryOpts}) ->
  gen_server:call(?SERVER(JobName), {build_factory, Factory}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(JobName :: term(), Sup :: pid(), JobSpec :: tuple()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(JobName, Sup, JobSpec) ->
  gen_server:start_link({local, ?SERVER(JobName)}, ?MODULE, {JobName, JobSpec, Sup}, []).

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
init({JobName, JobSpec, Sup}) ->
  self() ! {start_factory_supervisor, Sup, JobName},
  {ok, #state{refs = gb_sets:empty(), job_name = JobName, job_spec = JobSpec}}.

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
handle_call({build_factory, Factory = {_FactoryName, _FactoryModule, _FactoryOpts}}, _From,
    State = #state{factory_sup = Sup, job_name = JobName, refs = Refs}) ->
  {ok, Pid} = supervisor:start_child(Sup, [JobName, Factory]),
  Ref = monitor(process, Pid),
  NewState = State#state{refs = gb_sets:add(Ref, Refs)},
  {reply, {ok, Pid}, NewState};

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

handle_info({start_factory_supervisor, Sup, JobName}, State = #state{}) ->
  {ok, Pid} = supervisor:start_child(Sup, ?SHELL_FACTORY_SPEC(JobName)),
  link(Pid),
  {noreply, State#state{factory_sup = Pid}};

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
