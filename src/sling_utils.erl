%%%-------------------------------------------------------------------
%%% @author szymon.czaja
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Sep 2016 13:24
%%%-------------------------------------------------------------------
-module(sling_utils).
-author("szymon.czaja").

%% API
-export([
    get_env/1
]).


get_env(Key) ->
    case application:get_env(sling, Key) of
        {ok,Val} -> Val;
        _ -> undefined
    end.