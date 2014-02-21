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
-callback init(Options :: list(term())) -> ok | tuple(error, Reason :: string()).

-callback reactants() -> Templates :: list(tuple()).

-callback reaction(Reactants :: list(tuple())) -> Products :: list(tuple()) | tuple(error, Reason :: string()).


