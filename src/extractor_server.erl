%%%-------------------------------------------------------------------
%%% @author dokie
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 27. Feb 2014 21:26
%%%-------------------------------------------------------------------
-module(extractor_server).
-author("dokie").

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/2, run/2, wait_for_extractants/4]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER(JobName, ExtractorName), utilities:atom_concat(JobName, ExtractorName)).

-record(state, {job_name, extractor_name, extractor, extractor_state, listener}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(run(JobName :: atom(), ExtractorName :: atom()) -> no_return()).
run(JobName, ExtractorName) ->
  gen_server:cast(?SERVER(JobName, ExtractorName), run).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(JobName :: term(),
    {ExtractorName :: atom(), ExtractorModule :: module(), Options :: list(tuple())} | none()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link() ->
  {ok, #state{}}.

start_link(JobName, {ExtractorName, ExtractorModule, Options}) ->
  gen_server:start_link({local, ?SERVER(JobName, ExtractorName)}, ?MODULE,
    [JobName, ExtractorName, ExtractorModule, Options], []).

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

init([JobName, ExtractorName, ExtractorModule, Options]) ->
  {ok, {ExtractantTemplates, ExtractorState}} = ExtractorModule:init(Options),
  {ok, #state{job_name = JobName, extractor_name = ExtractorName, extractor = ExtractorModule,
    extractor_state = {ExtractantTemplates, ExtractorState}, listener = null}}.

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
  S = #state{job_name = JobName, extractor_name = ExtractorName, extractor = Extractor,
  extractor_state = {ExtractantTemplates, _ExtractorState}, listener = Listener}) ->
  if
    null =:= Listener ->
      NewState = S#state{listener = start_listener(JobName, ExtractorName, Extractor, ExtractantTemplates)},
      {noreply, NewState};
    true ->
      {noreply, S}
  end;

handle_cast({extract, Extractants},
    S = #state{job_name = JobName, extractor_name = ExtractorName, extractor = Extractor,
      extractor_state = {ExtractantTemplates, ExtractorState}}) ->
  Extractor:extract(self(), {Extractants, ExtractorState}),
  start_listener(JobName, ExtractorName, Extractor, ExtractantTemplates),
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
-spec(start_listener(JobName :: term(), ExtractorName :: atom(), Extractor :: module(), ExtractantTemplates :: list(tuple()))
      -> no_return()).
start_listener(JobName, ExtractorName, Extractor, ExtractantTemplates) ->
  spawn_link(?MODULE, wait_for_extractants, [JobName, ExtractorName, Extractor, ExtractantTemplates]).

-spec(wait_for_extractants(JobName :: term(), ExtractorName :: atom(), Extractor :: module(), ExtractantTemplates :: list(tuple())) -> no_return()).
wait_for_extractants(JobName, ExtractorName, Extractor, ExtractantTemplates) when is_atom(Extractor), is_list(ExtractantTemplates) ->
  InMap = fun
    (ExtractantTemplate) when is_tuple(ExtractantTemplate) ->
      tuple_space_server:in(ExtractantTemplate)
  end,
  Extractants = utilities:pmap(InMap, ExtractantTemplates),
  %% Send Extractants to extractor process
  gen_server:cast(?SERVER(JobName, ExtractorName), {extract, Extractants}).