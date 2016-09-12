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
-export([
	
	%% General
	app_name/0
	,get_env/1
	,get_env/2

	%% Specific
	,get_server_domain/0
	,get_dkim_selector/0
	,get_dkim_private_key/0
	,get_dkim_public_key/0
]).

%%
%% @doc
%%
app_name() ->
	?APP_NAME.

%%
%% @doc
%%
get_env(Key) ->
	get_env(Key, undefined).

get_env(Key, Default) ->
    case application:get_env(?APP_NAME, Key) of
        {ok, ?ENV_PROPERTY_FLAG} ->
			KeyStr = sling_utils:ensure_string(Key),
			case os:getenv(KeyStr) of
				false ->
					set_env(Key, Default), 
					get_env(Key, Default);
				Val ->
					BinVal = sling_utils:ensure_binary(Val),
					set_env(Key, BinVal), 
					get_env(Key, Default)
			end;
        {ok, Val} -> 
			Val;
        _ -> 
			Default
    end.

%%
%% @private
%%

set_env(Key, Val) when Key =:= dkim_private_key ->
	PrivKey = sling_crypto:read_key_file(pem, Val),
	application:set_env(?APP_NAME, Key, PrivKey);
set_env(Key, Val) when Key =:= dkim_public_key ->
	PubKey = sling_crypto:read_key_file(pem, Val),	
	application:set_env(?APP_NAME, Key, PubKey);
set_env(Key, Val) ->
	application:set_env(?APP_NAME, Key, Val).


%%
%% @doc
%%
get_server_domain() ->
	get_env(server_domain).
	
get_dkim_selector() ->
	get_env(dkim_txt_dns_selector).

get_dkim_private_key() ->
	get_env(dkim_private_key).

get_dkim_public_key() ->
	get_env(dkim_public_key).





