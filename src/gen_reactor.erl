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
-export([start/3, init/1, loop/2, stop/2]).

-spec(start(ReactorName :: atom(), ReactorModule :: module(), Options :: list()) -> {ok, pid()}).
start(ReactorName, ReactorModule, Options) ->
  init([ReactorName, ReactorModule, Options]).

-spec(init(Config :: array()) -> {ok, pid()}).
init([Name, Mod, Opts]) ->
  register(Name, self()),
  ok = Mod:init(Opts),
  Reactants = Mod:reactants(),
  Pid = spawn(?MODULE, loop, [Mod, Reactants]),
  {ok, Pid}.

-spec(loop(Mod :: module(), Reactants :: list(tuple())) -> no_return()).
loop(Mod, Reactants) when is_atom(Mod), is_list(Reactants) ->
  InMap = fun
    (Reactant) when is_tuple(Reactant) ->
      tuple_space_server:in(Reactant)
      end,
  Elements = utilities:pmap(InMap, Reactants),
  Products = Mod:reaction(Elements),
  OutMap = fun
    (Product) when is_tuple(Product) ->
      tuple_space_server:out(Product)
      end,
  utilities:pmap(OutMap, Products),
  loop(Mod, Reactants).

stop(Name, Pid) ->
  unregister(Name),
  exit(Pid, normal).
