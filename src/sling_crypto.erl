%%%-------------------------------------------------------------------
%%% @author szymon.czaja
%%% @copyright (C) 2016, IONAS SOFTWARE LTD
%%% @doc
%%% http://erlang.org/doc/apps/public_key/using_public_key.html
%%% @end
%%% Created : 08. Sep 2016 13:24
%%%-------------------------------------------------------------------
-module(sling_crypto).
-author("szymon.czaja").


%% API
-export([

	verify_dkim/2
	,verify_dkim/3
	,sign_dkim/1
	
	,read_key_file/2
	,key_to_entry/2

]).

verify_dkim(Msg, Signature) when is_binary(Msg), is_binary(Signature) ->
	PublicKey = sling_config:get_dkim_public_key(),
	verify_dkim(Msg, Signature, PublicKey).
	
verify_dkim(Msg, Signature, PublicKey) when is_binary(Msg), is_binary(Signature) ->
	public_key:verify(Msg, sha256, Signature, PublicKey).
		

%%
%% @doc 
%%
sign_dkim(Msg) when is_binary(Msg) ->
	PrivData = sling_config:get_dkim_private_key(),
	PrivKey = sling_crypto:key_to_entry(pem, PrivData),
	public_key:sign(Msg, sha256, PrivKey).


%%
%% @doc Expects unprotected PEM encoded files with one entry.
%%
read_key_file(Type, Path) when Type =:= pem, is_binary(Path) ->
	PathStr = sling_utils:ensure_string(Path),
	{ok, PemData} = file:read_file(PathStr),
	PemData.

%%
%% @doc
%%
key_to_entry(Type, Data) when Type =:= pem, is_binary(Data) ->
	[PemEntry] = public_key:pem_decode(Data),
	public_key:pem_entry_decode(PemEntry);

key_to_entry(Type, Data) when Type =:= base64, is_binary(Data) ->
	UnBase = base64:mime_decode(Data),
	{_, _, Der} = public_key:der_decode('SubjectPublicKeyInfo', UnBase),
	public_key:der_decode('RSAPublicKey', Der).



