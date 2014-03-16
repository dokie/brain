%%%-------------------------------------------------------------------
%%% @author dokie
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2014 12:04
%%%-------------------------------------------------------------------
-module(reactor).
-author("dokie").

%% Behaviour
-callback init(Options :: list(term())) -> {ok, {Templates :: list(tuple()), State :: term()}}.

-callback react(From :: atom() | pid() | port() | {atom(),atom()}, {Reactants :: list(tuple()), State :: term()}) -> ok.


