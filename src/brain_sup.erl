-module(brain_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_job/2, stop_job/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_job(JobName, JobSpec) ->
  ChildSpec = {JobName,
    {job_sup, start_link, [JobName, JobSpec]},
    permanent, 10500, supervisor, [brain_sup]},
  supervisor:start_child(brain_sup, ChildSpec).

stop_job(JobName) ->
  supervisor:terminate_child(brain_sup, JobName),
  supervisor:delete_child(brain_sup, JobName).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10},
      [
        ?CHILD(tuple_space_server, worker)
      ]} }.

