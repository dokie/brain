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
-export([start_link/3, stop/2, init/4, wait_for_reactants/3]).
-export([loop/4]).

-spec(start_link(ReactorName :: atom(), ReactorModule :: module(), Options :: list()) -> {ok, pid()}).
start_link(ReactorName, ReactorModule, Options) ->
  Server = proc_lib:start_link(?MODULE, init, [self(), ReactorName, ReactorModule, Options]),
  {ok, Server}.

-spec(stop(Server :: pid(), ReactorName :: atom()) -> ok).
stop(Server, ReactorName) ->
  Server ! {stop, ReactorName},
  ok.

-spec(init(Parent :: pid(), ReactorName :: atom(), Reactor :: module(), Opts :: list()) -> no_return()).
init(Parent, ReactorName, Reactor, Opts) ->
  register(ReactorName, self()),
  ok = Reactor:init(Opts),
  ReactantTemplates = Reactor:reactants(),
  Listener = start_listener(Reactor, ReactantTemplates),
  Dbg = sys:debug_options([]),
  proc_lib:init_ack(Parent, {ok, self()}),
  loop(Parent, Reactor, Dbg, [Listener, ReactantTemplates]).

start_listener(Reactor, ReactantTemplates) ->
  spawn(?MODULE, wait_for_reactants, [self(), Reactor, ReactantTemplates]).

-spec(wait_for_reactants(Parent :: pid(), Reactor :: module(), ReactantTemplates :: list(tuple())) -> no_return()).
wait_for_reactants(Parent, Reactor, ReactantTemplates) when is_pid(Parent), is_atom(Reactor), is_list(ReactantTemplates) ->
  InMap = fun
    (ReactantTemplate) when is_tuple(ReactantTemplate) ->
      tuple_space_server:in(ReactantTemplate)
      end,
  Reactants = utilities:pmap(InMap, ReactantTemplates),
  %% Send Reactants to generic reactor process
  Parent ! {react, self(), Reactants},
  receive
    {reacted, _From} ->
      ok
  end.

%% =========== INTERNAL PRIVATE FUNCTIONS ==============================
loop(_Parent, Reactor, _Debug, [Listener, ReactantTemplates]) ->
  receive
    {react, Listener, Reactants} ->
        Products = Reactor:react(Reactants),
        Listener ! {reacted, self()},
        OutMap = fun
          (Product) when is_tuple(Product) ->
            tuple_space_server:out(Product)
        end,
        utilities:pmap(OutMap, Products),
        NewListener = start_listener(Reactor, ReactantTemplates),
        NewState = [NewListener, ReactantTemplates],
        loop(_Parent, Reactor, _Debug, NewState);
    {stop, ReactorName} ->
      unregister(ReactorName),
      % Stop the listener
      exit(Listener, normal),
      exit(shutdown)
  end.
