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
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Factory :: {FactoryName :: term(), _FactoryModule :: module(), FactoryOpts :: [term()]}) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Factory = {_FactoryName, _FactoryModule, _FactoryOpts}) ->
  supervisor:start_link(?MODULE, Factory).

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
init({FactoryName, FactoryModule, FactoryOpts}) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  {ok, {SupFlags, [{factory_server, {FactoryName, FactoryModule, FactoryOpts}, temporary, 5000, worker,
    [FactoryName, FactoryModule, FactoryOpts]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
