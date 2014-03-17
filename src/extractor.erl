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
-callback init(Options :: list(term())) -> {ok, {Extractants :: list(tuple()), State :: term()}}.

-callback extract(From :: atom() | pid() | port() | {atom(),atom()}, {Extractants :: list(tuple()), State :: term()}) -> ok.