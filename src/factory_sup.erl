%%%-------------------------------------------------------------------
%%% @author dokie
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 08. Mar 2014 19:48
%%%-------------------------------------------------------------------
-module(factory_sup).
-author("dokie").

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(JobName :: term(),
    Factory :: {FactoryName :: term(), FactoryModule :: module(), FactoryOpts :: [term()]}) ->
      {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(JobName, Factory = {_FactoryName, _FactoryModule, _FactoryOpts}) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, {JobName, Factory}).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init({JobName, {FactoryName, FactoryModule, FactoryOpts}}) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  {ok, {SupFlags, [?CHILD(factory_server, worker,
    [JobName, {FactoryName, FactoryModule, FactoryOpts}])]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
