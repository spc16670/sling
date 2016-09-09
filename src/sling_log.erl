%%%-------------------------------------------------------------------
%%% @author szymon.czaja
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Sep 2016 13:24
%%%-------------------------------------------------------------------
-module(sling_log).
-author("szymon.czaja").

%% API
-export([
	info/2
]).

info(Msg, Params) ->
	io:fwrite(Msg, Params).

