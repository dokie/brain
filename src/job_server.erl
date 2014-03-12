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
-export([specify_job/1, run_job/1]).

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


-record(state, {factory_sup, factories, job_name, job_spec}).

%%%===================================================================
%%% API
%%%===================================================================

specify_job(JobName) ->
  gen_server:call(?SERVER(JobName), specify_job).

run_job(JobName) ->
  gen_server:call(?SERVER(JobName), run_job).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(JobName :: term(), JobSup :: pid(), JobSpec :: tuple()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link(JobName, JobSup, JobSpec) ->
  gen_server:start_link({local, ?SERVER(JobName)}, ?MODULE, {JobName, JobSpec, JobSup}, []).

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
  {ok, S :: #state{}} | {ok, S :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init({JobName, JobSpec = {factories, _FactorySpecs}, JobSup}) ->
  self() ! {start_factory_supervisor, JobSup, JobName},
  {ok, #state{factories = gb_sets:empty(), job_name = JobName, job_spec = JobSpec}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    S :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call(specify_job, _From, State = #state{job_spec = JobSpec}) ->
  {factories, FactorySpecs} = JobSpec,
  BuildFactory = fun (FactorySpec,
      S = #state{factory_sup = FactorySup, job_name = JobName, factories = FactoryRefs}) ->
      {ok, FactoryPid} = supervisor:start_child(FactorySup, [JobName, FactorySpec]),
      FactoryRef = monitor(process, FactoryPid),
      NewState = S#state{factories = gb_sets:add(FactoryRef, FactoryRefs)},
      NewState
    end,
  FinalState = lists:foldl(BuildFactory, State, FactorySpecs),
  {reply, ok, FinalState};

handle_call(run_job, _From, State = #state{job_spec = JobSpec}) ->
  {factories, FactorySpecs} = JobSpec,
  RunFactory = fun ({FactoryName, _, _ }, S = #state{job_name = JobName}) ->
    factory_server:run(JobName, FactoryName),
    S
  end,
  FinalState = lists:foldl(RunFactory, State, FactorySpecs),
  {reply, ok, FinalState};

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

handle_info({start_factory_supervisor, JobSupervisor, JobName}, State = #state{}) ->
  {ok, FactorySupervisor} = supervisor:start_child(JobSupervisor, ?SHELL_FACTORY_SPEC(JobName)),
  link(FactorySupervisor),
  {noreply, State#state{factory_sup = FactorySupervisor}};

handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{factories = FactoryRefs}) ->
  case gb_sets:is_element(Ref, FactoryRefs) of
    true ->
      handle_down_factory(Ref, S);
    false -> %% Not our responsibility, yet
      {noreply, S}
  end;

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
handle_down_factory(FactoryRef,
    S = #state{factory_sup = FactorySupervisor, factories = FactoryRefs, job_name = JobName}) ->
  {ok, Factory} = supervisor:start_child(FactorySupervisor, ?SHELL_FACTORY_SPEC(JobName)),
  NewFactoryRef = erlang:monitor(process, Factory),
  NewFactoryRefs = gb_sets:insert(NewFactoryRef, gb_sets:delete(FactoryRef, FactoryRefs)),
  {noreply, S#state{factories = NewFactoryRefs}}.