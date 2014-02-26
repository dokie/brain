%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 18. Feb 2014 12:23
%%%-------------------------------------------------------------------
-module(gen_reactor).
-author("mike").

%% API
-export([start_link/3, init/4, wait_for_reactants/3]).
-export([loop/3]).
-export([system_continue/3, system_terminate/4, write_debug/3, system_code_change/4]).

-spec(start_link(ReactorName :: atom(), ReactorModule :: module(), Options :: list()) -> {ok, pid()}).
start_link(ReactorName, ReactorModule, Options) ->
  proc_lib:start_link(?MODULE, init, [self(), ReactorName, ReactorModule, Options]).

-spec(init(Parent :: pid(), ReactorName :: atom(), Reactor :: module(), Opts :: list()) -> no_return()).
init(Parent, ReactorName, Reactor, Opts) ->
  register(ReactorName, self()),
  ok = Reactor:init(Opts),
  ReactantTemplates = Reactor:reactants(),
  Listener = start_listener(Reactor, ReactantTemplates),
  Dbg = sys:debug_options([]),
  proc_lib:init_ack(Parent, {ok, self()}),
  loop(Parent, Dbg, [Reactor, Listener, ReactantTemplates]).

start_listener(Reactor, ReactantTemplates) ->
  spawn(?MODULE, wait_for_reactants, [self(), Reactor, ReactantTemplates]).

-spec(wait_for_reactants(Parent :: pid(), Reactor :: module(), ReactantTemplates :: list(tuple())) -> no_return()).
wait_for_reactants(Parent, Reactor, ReactantTemplates) when is_pid(Parent), is_atom(Reactor), is_list(ReactantTemplates) ->
  InMap = fun
    (ReactantTemplate) when is_tuple(ReactantTemplate) ->
      tuple_space_server:in(ReactantTemplate)
      end,
  Reactants = utilities:pmap(InMap, ReactantTemplates),
  %% Send Reactants to reactor process
  Parent ! {react, self(), Reactants},
  receive
    {reacted, _From} ->
      ok
  end.

system_continue(Parent, Deb, State) ->
  io:format("Continue!~n"),
  loop(Parent, Deb, State).

system_terminate(Reason, _Parent, _Deb, [_Reactor, Listener, _ReactantTemplates]) ->
  io:format("Terminate!~n"),
  exit(Listener, normal),
  exit(Reason).

system_code_change(State, _Module, _OldVsn, _Extra) ->
  io:format("Changed code!~n"),
  {ok, State}.

%% =========== INTERNAL PRIVATE FUNCTIONS ==============================
loop(Parent, Debug, State = [Reactor, Listener, ReactantTemplates]) ->
  receive
    {react, Listener, Reactants} ->
      Listener ! {reacted, self()},
      spawn(Reactor, react, [self(), Reactants]),
      loop(Parent, Debug, State);

    {products, Products} ->
      OutMap = fun
        (Product) when is_tuple(Product) ->
          tuple_space_server:out(Product)
      end,
      utilities:pmap(OutMap, Products),
      NewListener = start_listener(Reactor, ReactantTemplates),
      NewState = [Reactor, NewListener, ReactantTemplates],
      loop(Parent, Debug, NewState);

    {system, From, Request} ->
      sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
    Msg ->
      % Let's print unknown messages.
      sys:handle_debug(Debug, fun ?MODULE:write_debug/3, ?MODULE, {in, Msg}),
      loop(Parent, Debug, State)
  end.

write_debug(Dev, Event, Name) ->
  io:format(Dev, "~p event = ~p~n", [Name, Event]).
