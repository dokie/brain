-module(brain_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, specify_job/2, run_job/1, complete_job/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec(specify_job(JobName :: atom(), Job :: dict()) -> ok).
specify_job(JobName, Job) ->
  JobSpec = expand(Job),
  ChildSpec = {JobName,
    {job_sup, start_link, [JobName, JobSpec]},
    permanent, 10500, supervisor, [brain_sup]},
  supervisor:start_child(brain_sup, ChildSpec),
  job_server:specify_job(JobName).

-spec(expand(Job :: dict()) -> tuple()).
expand(Job) ->
  ok.

-spec(complete_job(JobName :: atom()) -> ok).
complete_job(JobName) ->
  supervisor:terminate_child(brain_sup, JobName),
  supervisor:delete_child(brain_sup, JobName).

-spec(run_job(JobName :: atom()) -> ok).
run_job(JobName) ->
  job_server:run_job(JobName),
  ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10},
      [
        ?CHILD(tuple_space_server, worker)
      ]} }.

