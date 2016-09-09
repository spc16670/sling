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
	ensure_string/1
	,ensure_binary/1
	,ensure_ip_string/1	
	,ensure_ip_tuple/1
]).

ensure_string(Val) when is_binary(Val) ->
	binary_to_list(Val);
ensure_string(Val) when is_atom(Val) ->
	atom_to_list(Val);
ensure_string(Val) when is_list(Val) ->
	Val.

ensure_binary(Val) when is_list(Val) ->
	list_to_binary(Val);
ensure_binary(Val) when is_binary(Val) ->
	Val.

ensure_ip_string(IP) when is_tuple(IP) ->
	inet_parse:ntoa(IP);	
ensure_ip_string(IP) when is_list(IP) ->
	IP.

ensure_ip_tuple(IP) when is_tuple(IP) ->
	IP;	
ensure_ip_tuple(IP) when is_list(IP) ->
	case inet_parse:address(IP) of
		{ok, Tuple} -> 
			Tuple;
		Error -> 
			Error
	end.


