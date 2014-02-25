%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 25. Feb 2014 08:23
%%%-------------------------------------------------------------------
-module(gen_generator).
-author("mike").

%% API
-export([start_link/3, init/4]).
-export([loop/3]).
-export([system_continue/3, system_terminate/4, write_debug/3, system_code_change/4]).

-spec(start_link(GeneratorName :: atom(), GeneratorModule :: module(), Options :: list()) -> {ok, pid()}).
start_link(GeneratorName, GeneratorModule, Options) ->
  proc_lib:start_link(?MODULE, init, [self(), GeneratorName, GeneratorModule, Options]).

-spec(init(Parent :: pid(), GeneratorName :: atom(), Generator :: module(), Opts :: list()) -> no_return()).
init(Parent, GeneratorName, Generator, Opts) ->
  register(GeneratorName, self()),
  {ok, InitialState} = Generator:init(Opts),
  Dbg = sys:debug_options([]),
  proc_lib:init_ack(Parent, {ok, self()}),
  Listener = start_listener(self(), Dbg, Generator, InitialState),
  Generator:generate(Listener, InitialState),
  Listener.

start_listener(Parent, Dbg, Generator, GeneratorState) ->
  spawn(?MODULE, loop, [Parent, Dbg, [Generator, GeneratorState]]).

system_continue(Parent, Deb, State) ->
  io:format("Continue!~n"),
  loop(Parent, Deb, State).

system_terminate(Reason, _Parent, _Deb, [_Generator, __GeneratorState]) ->
  io:format("Terminate!~n"),
  exit(Reason).

system_code_change(State, _Module, _OldVsn, _Extra) ->
  io:format("Changed code!~n"),
  {ok, State}.

%% =========== INTERNAL PRIVATE FUNCTIONS ==============================
loop(Parent, Debug, ServerState = [Generator, _InitialState]) ->

  receive
    {generated, Product, GeneratedState} ->
      ok = tuple_space_server:out(Product),
      NewServerState = [Generator, GeneratedState],
      Generator:generate(self(), GeneratedState),
      loop(Parent, Debug, NewServerState);

    stop ->
      exit(normal);

    {system, From, Request} ->
      sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, ServerState);

    Msg ->
      % Let's print unknown messages.
      sys:handle_debug(Debug, fun ?MODULE:write_debug/3, ?MODULE, {in, Msg}),
      loop(Parent, Debug, ServerState)
  end.

write_debug(Dev, Event, Name) ->
  io:format(Dev, "~p event = ~p~n", [Name, Event]).
