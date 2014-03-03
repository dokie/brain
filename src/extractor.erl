%%%-------------------------------------------------------------------
%%% @author dokie
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2014 12:04
%%%-------------------------------------------------------------------
-module(extractor).
-author("dokie").

%% Behaviour
-callback init(Options :: list(term())) -> Extractants :: list(tuple()).

-callback extract(Extractants :: list(tuple())) -> ok.