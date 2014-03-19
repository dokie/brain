%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 12. Mar 2014 09:17
%%%-------------------------------------------------------------------
-module(brain).
-author("mike").

%% API
%% brain app
-export([start/0, stop/0]).

%% Tuplespace Server

%% brain supervisor
-export([specify_job/2, run_job/1, complete_job/1]).

%% job server

%% =========== brain app ===================
start() ->
  application:start(brain).

stop() ->
  application:stop(brain).
%% =========== brain supervisor ============
-spec(specify_job(JobName :: atom(), JobSpec :: dict()) -> ok).
specify_job(JobName, JobSpec) ->
  brain_sup:specify_job(JobName, JobSpec).

-spec(run_job(JobName :: atom()) -> ok).
run_job(JobName) ->
  brain_sup:run_job(JobName).

-spec(complete_job(JobName :: atom()) -> ok).
complete_job(JobName) ->
  brain_sup:complete_job(JobName).
%% =========== job server ===================
