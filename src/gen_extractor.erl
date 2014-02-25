%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 18. Feb 2014 12:23
%%%-------------------------------------------------------------------
-module(gen_extractor).
-author("mike").

%% API
-export([start_link/3, init/4, wait_for_extractants/3]).
-export([loop/3]).
-export([system_continue/3, system_terminate/4, write_debug/3, system_code_change/4]).

-spec(start_link(ExtractorName :: atom(), ExtractorModule :: module(), Options :: list()) -> {ok, pid()}).
start_link(ExtractorName, ExtractorModule, Options) ->
  proc_lib:start_link(?MODULE, init, [self(), ExtractorName, ExtractorModule, Options]).

-spec(init(Parent :: pid(), ExtractorName :: atom(), Extractor :: module(), Opts :: list()) -> no_return()).
init(Parent, ExtractorName, Extractor, Opts) ->
  register(ExtractorName, self()),
  ok = Extractor:init(Opts),
  ExtractantTemplates = Extractor:extractants(),
  Listener = start_listener(Extractor, ExtractantTemplates),
  Dbg = sys:debug_options([]),
  proc_lib:init_ack(Parent, {ok, self()}),
  loop(Parent, Dbg, [Extractor, Listener, ExtractantTemplates]).

start_listener(Extractor, ExtractantTemplates) ->
  spawn(?MODULE, wait_for_extractants, [self(), Extractor, ExtractantTemplates]).

-spec(wait_for_extractants(Parent :: pid(), Ectractor :: module(), ExtractantTemplates :: list(tuple())) -> no_return()).
wait_for_extractants(Parent, Extractor, ExtractantTemplates) when is_pid(Parent), is_atom(Extractor), is_list(ExtractantTemplates) ->
  InMap = fun
    (ExtractantTemplate) when is_tuple(ExtractantTemplate) ->
      tuple_space_server:in(ExtractantTemplate)
      end,
  Extractants = utilities:pmap(InMap, ExtractantTemplates),
  %% Send Extractants to extractor process
  Parent ! {extract, self(), Extractants},
  receive
    {extracted, _From} ->
      ok
  end.

system_continue(Parent, Deb, State) ->
  io:format("Continue!~n"),
  loop(Parent, Deb, State).

system_terminate(Reason, _Parent, _Deb, [_Extractor, Listener, _ExtractantTemplates]) ->
  io:format("Terminate!~n"),
  exit(Listener, normal),
  exit(Reason).

system_code_change(State, _Module, _OldVsn, _Extra) ->
  io:format("Changed code!~n"),
  {ok, State}.

%% =========== INTERNAL PRIVATE FUNCTIONS ==============================
loop(Parent, Debug, State = [Extractor, Listener, ExtractantTemplates]) ->
  receive
    {extract, Listener, Extractants} ->
        Extractor:extract(Extractants),
        Listener ! {extracted, self()},
        NewListener = start_listener(Extractor, ExtractantTemplates),
        NewState = [Extractor, NewListener, ExtractantTemplates],
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
