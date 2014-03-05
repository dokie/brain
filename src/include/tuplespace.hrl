%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 05. Mar 2014 15:06
%%%-------------------------------------------------------------------
-author("mike").

-record(tupulo, {id, fields = []}).
-record(requests, {pid, from}).