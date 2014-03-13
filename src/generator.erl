%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 25. Feb 2014 08:23
%%%-------------------------------------------------------------------
-module(generator).
-author("mike").

%% Behaviour
-callback init(Options :: list(term())) -> {ok, State :: term()} | tuple(error, Reason :: string()).

-callback run(From :: pid(), State :: term()) -> no_return().
