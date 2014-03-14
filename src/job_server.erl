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

-define(SHELL_GENERATOR_SPEC(JobName),
  {generator_sup,
    {generator_sup, start_link, [JobName]},
    permanent,
    infinity,
    supervisor,
    [generator_sup]}).

-define(SHELL_REACTOR_SPEC(JobName),
  {reactor_sup,
    {reactor_sup, start_link, [JobName]},
    permanent,
    infinity,
    supervisor,
    [reactor_sup]}).

-define(SHELL_EXTRACTOR_SPEC(JobName),
  {extractor_sup,
    {extractor_sup, start_link, [JobName]},
    permanent,
    infinity,
    supervisor,
    [extractor_sup]}).

-record(state, {factory_sup, factories, generator_sup, generators, reactor_sup, reactors,
  extractor_sup, extractors, job_name, job_spec}).

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

init({JobName, JobSpec = {factories, _FactorySpecs, generators, _GeneratorSpecs,
  reactors, _ReactorSpecs, extractors, _ExtractorSpecs}, JobSup}) ->
  self() ! {start_supervisors, JobSup, JobName},
  {ok, #state{factories = gb_sets:empty(), generators = gb_sets:empty(), reactors = gb_sets:empty(),
    extractors = gb_sets:empty(), job_name = JobName, job_spec = JobSpec}}.

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

handle_call(specify_job, _From, OriginalState = #state{job_spec = JobSpec}) ->
  {factories, FactorySpecs, generators, GeneratorSpecs, reactors, ReactorSpecs,
    extractors, ExtractorSpecs} = JobSpec,
  BuildFactory = fun (FactorySpec,
      S = #state{factory_sup = FactorySup, job_name = JobName, factories = FactoryRefs}) ->
      {ok, FactoryPid} = supervisor:start_child(FactorySup, [JobName, FactorySpec]),
      FactoryRef = monitor(process, FactoryPid),
      NewState = S#state{factories = gb_sets:add(FactoryRef, FactoryRefs)},
      NewState
    end,
  FactoryState = lists:foldl(BuildFactory, OriginalState, FactorySpecs),
  BuildGenerator = fun (GeneratorSpec,
      S = #state{generator_sup  = GeneratorSup, job_name = JobName, generators = GeneratorRefs}) ->
    {ok, GeneratorPid} = supervisor:start_child(GeneratorSup, [JobName, GeneratorSpec]),
    GeneratorRef = monitor(process, GeneratorPid),
    NewState = S#state{generators = gb_sets:add(GeneratorRef, GeneratorRefs)},
    NewState
  end,
  GeneratorState = lists:foldl(BuildGenerator, FactoryState, GeneratorSpecs),
  BuildReactor = fun (ReactorSpec,
      S = #state{reactor_sup = ReactorSup, job_name = JobName, reactors = ReactorRefs}) ->
    {ok, ReactorPid} = supervisor:start_child(ReactorSup, [JobName, ReactorSpec]),
    ReactorRef = monitor(process, ReactorPid),
    NewState = S#state{reactors = gb_sets:add(ReactorRef, ReactorRefs)},
    NewState
  end,
  ReactorState = lists:foldl(BuildReactor, GeneratorState, ReactorSpecs),
  BuildExtractor = fun (ExtractorSpec,
      S = #state{extractor_sup = ExtractorSup, job_name = JobName, extractors = ExtractorRefs}) ->
    {ok, ExtractorPid} = supervisor:start_child(ExtractorSup, [JobName, ExtractorSpec]),
    ExtractorRef = monitor(process, ExtractorPid),
    NewState = S#state{extractors = gb_sets:add(ExtractorRef, ExtractorRefs)},
    NewState
  end,
  FinalState = lists:foldl(BuildExtractor, ReactorState, ExtractorSpecs),
  {reply, ok, FinalState};

handle_call(run_job, _From, State = #state{job_spec = JobSpec}) ->
  {factories, FactorySpecs, generators, GeneratorSpecs, reactors, ReactorSpecs,
   extractors, ExtractorSpecs} = JobSpec,
  RunFactory = fun ({FactoryName, _, _}, S = #state{job_name = JobName}) ->
    factory_server:run(JobName, FactoryName),
    S
  end,
  FactoryState = lists:foldl(RunFactory, State, FactorySpecs),
  RunGenerator = fun ({GeneratorName, _, _}, S = #state{job_name = JobName}) ->
    generator_server:run(JobName, GeneratorName),
    S
  end,
  GeneratorState = lists:foldl(RunGenerator, FactoryState, GeneratorSpecs),
  RunReactor = fun ({ReactorName, _, _}, S = #state{job_name = JobName}) ->
    reactor_server:run(JobName, ReactorName),
    S
  end,
  ReactorState = lists:foldl(RunReactor, GeneratorState, ReactorSpecs),
  RunExtractor = fun ({ExtractorName, _, _}, S = #state{job_name = JobName}) ->
    extractor_server:run(JobName, ExtractorName),
    S
  end,
  FinalState = lists:foldl(RunExtractor, ReactorState, ExtractorSpecs),
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

handle_info({start_supervisors, JobSupervisor, JobName}, State = #state{}) ->
  {ok, FactorySupervisor} = supervisor:start_child(JobSupervisor, ?SHELL_FACTORY_SPEC(JobName)),
  link(FactorySupervisor),
  {ok, GeneratorSupervisor} = supervisor:start_child(JobSupervisor, ?SHELL_GENERATOR_SPEC(JobName)),
  link(GeneratorSupervisor),
  {ok, ReactorSupervisor} = supervisor:start_child(JobSupervisor, ?SHELL_REACTOR_SPEC(JobName)),
  link(ReactorSupervisor),
  {ok, ExtractorSupervisor} = supervisor:start_child(JobSupervisor, ?SHELL_EXTRACTOR_SPEC(JobName)),
  link(ExtractorSupervisor),
  {noreply, State#state{factory_sup = FactorySupervisor, generator_sup = GeneratorSupervisor,
    reactor_sup = ReactorSupervisor, extractor_sup = ExtractorSupervisor}};

handle_info({'DOWN', Ref, process, _Pid, _},
    S = #state{factories = FactoryRefs, generators = GeneratorRefs,
      reactors = ReactorRefs, extractors = ExtractorRefs}) ->
  case gb_sets:is_element(Ref, FactoryRefs) of
    true ->
      handle_down_factory(Ref, S);
    false -> %% Not our responsibility, yet
      case gb_sets:is_element(Ref, GeneratorRefs) of
        true ->
          handle_down_generator(Ref, S);
        false -> %% Not our responsibility, yet
          case gb_sets:is_element(Ref, ReactorRefs) of
            true ->
              handle_down_reactor(Ref, S);
            false -> %% Not our responsibility, yet
              case gb_sets:is_element(Ref, ExtractorRefs) of
                true ->
                  handle_down_extractor(Ref, S);
                false -> %% Not our responsibility, yet
                  {noreply, S}
                end
          end
      end
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

handle_down_generator(GeneratorRef,
    S = #state{generator_sup = GeneratorSupervisor, generators = GeneratorRefs, job_name = JobName}) ->
  {ok, Generator} = supervisor:start_child(GeneratorSupervisor, ?SHELL_GENERATOR_SPEC(JobName)),
  NewGeneratorRef = erlang:monitor(process, Generator),
  NewGeneratorRefs = gb_sets:insert(NewGeneratorRef, gb_sets:delete(GeneratorRef, GeneratorRefs)),
  {noreply, S#state{generators = NewGeneratorRefs}}.

handle_down_reactor(ReactorRef,
    S = #state{reactor_sup = ReactorSupervisor, reactors = ReactorRefs, job_name = JobName}) ->
  {ok, Reactor} = supervisor:start_child(ReactorSupervisor, ?SHELL_REACTOR_SPEC(JobName)),
  NewReactorRef = erlang:monitor(process, Reactor),
  NewReactorRefs = gb_sets:insert(NewReactorRef, gb_sets:delete(ReactorRef, ReactorRefs)),
  {noreply, S#state{reactors = NewReactorRefs}}.

handle_down_extractor(ExtractorRef,
    S = #state{extractor_sup = ExtractorSupervisor, extractors = ExtractorRefs, job_name = JobName}) ->
  {ok, Extractor} = supervisor:start_child(ExtractorSupervisor, ?SHELL_EXTRACTOR_SPEC(JobName)),
  NewExtractorRef = erlang:monitor(process, Extractor),
  NewExtractorRefs = gb_sets:insert(NewExtractorRef, gb_sets:delete(ExtractorRef, ExtractorRefs)),
  {noreply, S#state{extractors = NewExtractorRefs}}.
