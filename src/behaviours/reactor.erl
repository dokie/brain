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
-callback init(Options :: list(term())) -> Templates :: list(tuple()).

-callback react(From :: pid(), Reactants :: list(tuple())) -> no_return().


