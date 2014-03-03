%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 28. Jan 2014 15:04
%%%-------------------------------------------------------------------
-module(tuple_space_server).
-author("mike").

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([stop/0, out/1, in/1, in/2, inp/1, rd/1, rd/2, rdp/1, eval/1, count/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(tuplespace, {tuples, requests}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop() -> atom()).

stop() ->
  gen_server:call(?SERVER, stop).

%%--------------------------------------------------------------------
%% @doc
%% Places a Tuple into the Tuplespace
%%
%% @end
%%--------------------------------------------------------------------
-spec(out(Tuple :: tuple()) -> {ok}).

out(Tuple) when is_tuple(Tuple) ->
  gen_server:call(?SERVER, {out, Tuple}).

%%--------------------------------------------------------------------
%% @doc
%% Gets a Tuple from the Tuplespace that matches a Template
%% and removes it. This is a Blocking Call
%%
%% @end
%%--------------------------------------------------------------------
-spec(in(Template :: tuple()) -> {term(), tuple()} | {noreply, term(), timeout()}).

in(Template) when is_tuple(Template) ->
  gen_server:call(?SERVER, {in, Template}, infinity).

-spec(in(Template :: tuple(), Timeout :: timeout()) -> {term(), tuple()} | {noreply, term(), timeout()}).

in(Template, Timeout) when is_tuple(Template), is_integer(Timeout), Timeout > 0 ->
  gen_server:call(?SERVER, {in, Template}, Timeout).

%%--------------------------------------------------------------------
%% @doc
%% Gets a Tuple from the Tuplespace that matches a Template
%% Non Blocking Call which returns undefined if no Tuple matches
%%
%% @end
%%--------------------------------------------------------------------
-spec(inp(Template :: tuple()) -> {term(), tuple()} | {noreply, term(), timeout()}).

inp(Template) when is_tuple(Template) ->
  gen_server:call(?SERVER, {inp, Template}).

%%--------------------------------------------------------------------
%% @doc
%% Reads a Tuple from the Tuplespace that matches a Template
%% and leaves it there. This is a Blocking Call
%%
%% @end
%%--------------------------------------------------------------------
-spec(rd(Template :: tuple()) -> {term(), tuple()} | {noreply, term(), timeout()}).

rd(Template) when is_tuple(Template) ->
  gen_server:call(?SERVER, {rd, Template}, infinity).

-spec(rd(Template :: tuple(), Timeout :: timeout()) -> {term(), tuple()} | {noreply, term(), timeout()}).

rd(Template, Timeout) when is_tuple(Template), is_integer(Timeout), Timeout > 0 ->
  gen_server:call(?SERVER, {rd, Template}, Timeout).

%%--------------------------------------------------------------------
%% @doc
%% Reads a Tuple from the Tuplespace that matches a Template
%% and leaves it there - Blocking Call which returns undefined if no Tuple matches
%%
%% @end
%%--------------------------------------------------------------------
-spec(rdp(Template :: tuple()) -> {term(), tuple()} | {noreply, term(), timeout()}).

rdp(Template) when is_tuple(Template) ->
  gen_server:call(?SERVER, {rdp, Template}).

%%--------------------------------------------------------------------
%% @doc
%% Evaluates a tuple of fun's to produce an actual Tuple to store in the
%% Tuplespace. The execution is done in parallel.
%%
%% @end
%%--------------------------------------------------------------------
-spec(eval(Specification :: tuple()) -> {ok} | {noreply, term(), timeout()}).

eval(Specification) when is_tuple(Specification) ->
  gen_server:call(?SERVER, {eval, Specification}).

%%--------------------------------------------------------------------
%% @doc
%% Counts the number of Tuples int the Tuplespace that match a Template
%%
%% @end
%%--------------------------------------------------------------------
-spec(count(Template :: tuple()) -> {number()} | {noreply, term(), timeout()}).

count(Template) when is_tuple(Template) ->
  gen_server:call(?SERVER, {count, Template}).

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
  {ok, State :: #tuplespace{}} | {ok, State :: #tuplespace{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #tuplespace{tuples = ets:new(tuples, [bag, named_table]),
              requests = ets:new(requests, [bag, named_table])}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #tuplespace{}) ->
  {reply, Reply :: term(), NewState :: #tuplespace{}} |
  {reply, Reply :: term(), NewState :: #tuplespace{}, timeout() | hibernate} |
  {noreply, NewState :: #tuplespace{}} |
  {noreply, NewState :: #tuplespace{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #tuplespace{}} |
  {stop, Reason :: term(), NewState :: #tuplespace{}}).

handle_call({out, Tuple}, _From, State) ->
  ToStore = tuple_space:out(Tuple),
  ets:insert(tuples, ToStore),
  {reply, ok, State};

handle_call({in, Template}, From, State) ->
  process_flag(trap_exit, true),
  Pid = spawn_link(tuple_space, in, [Template, self()]),
  ets:insert(requests, {Pid, From}),
  {noreply, State};

handle_call({inp, Template}, From, State) ->
  process_flag(trap_exit, true),
  Pid = spawn_link(tuple_space, inp, [Template, self()]),
  ets:insert(requests, {Pid, From}),
  {noreply, State};

handle_call({rd, Template}, From, State) ->
  process_flag(trap_exit, true),
  Pid = spawn_link(tuple_space, rd, [Template, self()]),
  ets:insert(requests, {Pid, From}),
  {noreply, State};

handle_call({rdp, Template}, From, State) ->
  process_flag(trap_exit, true),
  Pid = spawn_link(tuple_space, rdp, [Template, self()]),
  ets:insert(requests, {Pid, From}),
  {noreply, State};

handle_call({eval, Specification}, _From, State) ->
  ToStore = tuple_space:eval(Specification),
  ets:insert(tuples, ToStore),
  {reply, ok, State};

handle_call(stop, _From, State) ->
  {stop, normal,ok, State};

handle_call({count, Template}, From, State) ->
  process_flag(trap_exit, true),
  Pid = spawn_link(tuple_space, count, [Template, self()]),
  ets:insert(requests, {Pid, From}),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #tuplespace{}) ->
  {noreply, NewState :: #tuplespace{}} |
  {noreply, NewState :: #tuplespace{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #tuplespace{}}).
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
-spec(handle_info(Info :: timeout() | term(), State :: #tuplespace{}) ->
  {noreply, NewState :: #tuplespace{}} |
  {noreply, NewState :: #tuplespace{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #tuplespace{}}).

handle_info({Worker, _Ref, done, in, Tuple}, State) ->
  Request = ets:lookup(requests, Worker),
  Stripped = list_to_tuple(tl(tuple_to_list(Tuple))), %% Strip off UUID
  reply_to_request(Request, State, Stripped),
  ets:delete(tuples, element(1, Tuple)), %% Remove from Tuplespace
  {noreply, State};

handle_info({Worker, _Ref, done, inp, undefined}, State) ->
  Request = ets:lookup(requests, Worker),
  reply_to_request(Request, State, undefined),
  {noreply, State};

handle_info({Worker, _Ref, done, inp, Tuple}, State) ->
  Request = ets:lookup(requests, Worker),
  Stripped = list_to_tuple(tl(tuple_to_list(Tuple))), %% Strip off UUID
  reply_to_request(Request, State, Stripped),
  ets:delete(tuples, element(1, Tuple)), %% Remove from Tuplespace
  {noreply, State};

handle_info({Worker, Ref, query, Spec}, State) ->
  Selections = ets:select(tuples, Spec),
  Worker ! {selected, Ref, Selections},
  {noreply, State};

handle_info({Worker, _Ref, done, rd, Tuple}, State) ->
  Request = ets:lookup(requests, Worker),
  Stripped = list_to_tuple(tl(tuple_to_list(Tuple))), %% Strip off UUID
  reply_to_request(Request, State, Stripped),
  {noreply, State};

handle_info({Worker, _Ref, done, rdp, undefined}, State) ->
  Request = ets:lookup(requests, Worker),
  reply_to_request(Request, State, undefined),
  {noreply, State};

handle_info({Worker, _Ref, done, rdp, Tuple}, State) ->
  Request = ets:lookup(requests, Worker),
  Stripped = list_to_tuple(tl(tuple_to_list(Tuple))), %% Strip off UUID
  reply_to_request(Request, State, Stripped),
  {noreply, State};

handle_info({Worker, _Ref, done, count, Count}, State) ->
  Request = ets:lookup(requests, Worker),
  reply_to_request(Request, State, Count),
  {noreply, State};

handle_info({'EXIT', _Pid, normal}, State) ->
  {noreply, State};

handle_info({'EXIT', Pid, _}, State) ->
  InRequest = ets:lookup(requests, Pid),
  ok = reply_to_request(InRequest, State, undefined),
  {noreply, State}.

reply_to_request([], _State, _Reply) ->
  cleanup_request([], _State);

reply_to_request([{Worker, Client}], State, Reply) ->
  ok = cleanup_request([{Worker, Client}], State),
  gen_server:reply(Client, Reply),
  ok.

cleanup_request([], _State) ->
  ok;

cleanup_request([{Worker, _}], _State) ->
  ets:delete(requests, Worker),
  ok.

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
    State :: #tuplespace{}) -> term()).
terminate(_Reason, _State) ->
  ets:delete(requests),
  ets:delete(tuples),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #tuplespace{},
    Extra :: term()) ->
  {ok, NewState :: #tuplespace{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.