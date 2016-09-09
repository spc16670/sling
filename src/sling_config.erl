%%%-------------------------------------------------------------------
%%% @author szymon.czaja
%%% @copyright (C) 2016, IONAS SOFTWARE LTD
%%% @doc
%%%
%%% @end
%%% Created : 08. Sep 2016 13:24
%%%-------------------------------------------------------------------
-module(sling_config).
-author("szymon.czaja").

-define(APP_NAME, sling).
-define(ENV_PROPERTY_FLAG, <<"ENV">>).

%% API
-compile(export_all).

app_name() ->
	?APP_NAME.

get_env(Key) ->
	get_env(Key, undefined).

get_env(Key, Default) ->
    case application:get_env(?APP_NAME, Key) of
        {ok, ?ENV_PROPERTY_FLAG} ->
			KeyStr = sling_utils:ensure_string(Key),
			case os:getenv(KeyStr) of
				false ->
					application:set_env(?APP_NAME, Key, Default),
					Default;
				Val ->
					BinVal = sling_utils:ensure_binary(Val), 
					application:set_env(?APP_NAME, Key, BinVal),
					Val
			end;
        {ok, Val} -> 
			Val;
        _ -> 
			Default
    end.

get_server_domain() ->
	get_env(server_domain).
	
get_dkim_selector() ->
	get_env(dkim_txt_dns_selector).

