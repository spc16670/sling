%%%-------------------------------------------------------------------
%%% @author szymon.czaja
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Sep 2016 13:24
%%%-------------------------------------------------------------------
-module(sling_dns).
-author("szymon.czaja").

%% https://github.com/erlang/otp/blob/maint/lib/kernel/src/inet_dns.hrl
-include_lib("kernel/src/inet_dns.hrl").

%% API
-export([
	init/0

	%% Lookup helpers
	,mx_lookup/1
	,a_lookup/1
	,ptr_lookup/1
	,spf_lookup/1
	,dkim_lookup/2
	,dmarc_lookup/1

	%% Tests
	,forward_confirmed/0
	,forward_confirmed/1
	,spf_exists/0
	,spf_exists/1
	,dkim_configured/0
	,dkim_configured/2
	,dmarc_exists/0
	,dmarc_exists/1

	,dns_config_validated/0
]).

%% ----------------------------------------------------------------------------

%%
%% @doc
%%
-spec init() -> ok.

init() ->
	case whereis(inet_db) of
		P when is_pid(P) ->
			ok;
		_ -> 
			inet_db:start()
	end,
	case lists:keyfind(nameserver, 1, inet_db:get_rc()) of
		false ->
			% we got no nameservers configured, suck in resolv.conf
			inet_config:do_load_resolv(os:type(), longnames);
		_ ->
			ok
	end.

%% ----------------------------------------------------------------------------

%%
%% @doc 
%%
-spec dns_config_validated() -> true | false.

dns_config_validated() ->
	ok = sling_dns:init(),
	maybe_forward_confirmed(forward_confirmed()).
	
maybe_forward_confirmed(true) ->
	maybe_spf_exists(spf_exists());
maybe_forward_confirmed(false) ->
	false.

maybe_spf_exists(true) ->
	maybe_dkim_configured(dkim_configured());
maybe_spf_exists(false) ->
	false.

maybe_dkim_configured(true) ->
	maybe_dmarc_exists(dmarc_exists());
maybe_dkim_configured(false) ->
	false.

maybe_dmarc_exists(true) ->
	sling_log:info("~nDNS configuration check successful.~n",[]),
	true;
maybe_dmarc_exists(false) ->
	sling_log:info("~nDNS configuration check failed.~n",[]),
	false.

%% ----------------------------------------------------------------------------

%%
%% @doc
%%
-spec mx_lookup(string()) -> { ok, Any } | { error, Any }.

mx_lookup(ServerDomain) ->
	case inet_res:nslookup(ServerDomain, in, mx) of
		{ok, Record} ->
			case Record#'dns_rec'.'anlist' of
				[] ->
					{ error, <<"Empty MX answer">> };	
				DnsRecs ->
					Results = lists:foldl(fun(R, Acc) -> 
						RData = R#'dns_rr'.'data',
						Acc ++ [RData]
					end, [], DnsRecs),
					Sorted = lists:sort(fun({Pref, _Name}, {Pref2, _Name2}) -> 
						Pref =< Pref2 
					end, Results),
					{ok, Sorted}	
			end;
		Error -> 
			Error
	end.

%%
%% @doc
%%
-spec a_lookup(string()) -> { ok, Any } | { error, Any }.

a_lookup(Hostname) ->
	case inet_res:nslookup(Hostname, in, a) of
		{ok, Record} ->
			case Record#'dns_rec'.'anlist' of
				[] ->
					{ error, <<"Empty A answer">> };
				DnsRecs ->
					[FirstAnswer | _] = DnsRecs,
					IP = FirstAnswer#'dns_rr'.'data',	
					{ok, IP}
			end;
		Error ->
			Error
	end.

%%
%% @doc
%%
-spec spf_lookup(string()) -> { ok, Any } | { error, Any }.

spf_lookup(Hostname) ->
	case inet_res:nslookup(Hostname, in, txt) of
		{ok, Record} ->
			case Record#'dns_rec'.'anlist' of
				[] ->
					{ error, <<"Empty TXT answer">> };
				DnsRecs ->
					Results = lists:foldl(fun(Txt, Acc) -> 
						Data = Txt#'dns_rr'.'data',	
						if Data =:= [] ->
							Acc;
						true ->
							[DataStr | _] = Data,
							case string:str(DataStr,"spf") of
								0 ->
									Acc;
								_No ->
									Acc ++ [DataStr]
							end
						end
					end, [], DnsRecs),
					if Results =:= [] ->
						{error, <<"No SPF entry detected">>};
					true ->
						{ok, Results}
					end
			end;
		Error ->
			Error
	end.

%%
%% @doc
%%
-spec dkim_lookup(string(), string() | binary()) 
	-> { ok, Any } | { error, Any }.

dkim_lookup(Domain, Selector) when is_list(Domain) ->
	SelectorStr = sling_utils:ensure_string(Selector),
	FullSelector = SelectorStr ++ "._domainkey." ++ Domain,
	case inet_res:nslookup(FullSelector, in, txt) of
		{ok, Record} ->
			case Record#'dns_rec'.'anlist' of
				[] ->
					{ error, <<"Empty DKIM answer">> };
				DnsRecs ->
					[FirstAnswer | _] = DnsRecs,
					DKIM = FirstAnswer#'dns_rr'.'data',
					{ok, DKIM}	
			end;
		Error ->
			Error
	end.

%%
%% @doc
%%
-spec dmarc_lookup(string()) -> { ok, Any } | { error, Any }.

dmarc_lookup(Domain) when is_list(Domain) ->
	FullSelector = "_dmarc." ++ Domain,
	case inet_res:nslookup(FullSelector, in, txt) of
		{ok, Record} ->
			case Record#'dns_rec'.'anlist' of
				[] ->
					{ error, <<"Empty DMARC answer">> };
				DnsRecs ->
					[FirstAnswer | _] = DnsRecs,
					DMARC = FirstAnswer#'dns_rr'.'data',
					{ok, DMARC}	
			end;
		Error ->
			Error
	end.


%%
%% @doc
%%
-spec ptr_lookup(tuple() | string()) -> { ok, Any } | { error, Any }. 

ptr_lookup(IP) ->
	IPStr = sling_utils:ensure_ip_string(IP),
	case inet_res:nslookup(IPStr, in, ptr) of
		{ok, Record} ->
			case Record#'dns_rec'.'anlist' of
				[] ->
					{ error, <<"Empty PTR answer">> };
				DnsRecs ->
					[FirstAnswer | _] = DnsRecs,
					Domain = FirstAnswer#'dns_rr'.'data',
					{ok, Domain}
			end;
		Error ->
			Error
	end.

%% ----------------------------------------------------------------------------

%%
%% @doc Runs forward confirmed test also referred to as 'iprev' Authentication
%% Method in https://tools.ietf.org/html/rfc5451#section-3
%%
%% https://en.wikipedia.org/wiki/Forward-confirmed_reverse_DNS
%%

-spec forward_confirmed() -> true | false.

forward_confirmed() ->
	ServerDomain = sling_config:get_server_domain(),
	sling_log:info("Configured server domain: ~p~n",[ServerDomain]),
	forward_confirmed(ServerDomain).

forward_confirmed(ServerDomain) ->
	if ServerDomain =:= undefined ->
		Result = { error, <<"Server domain undefined">> },
		maybe_successful(Result);
	true ->
		DomainStr = sling_utils:ensure_string(ServerDomain),
		case mx_lookup(DomainStr) of
			{ok, Sorted} ->
				[FirstRecord | _ ] = Sorted,
				forward_confirmed(DomainStr, FirstRecord);
			Error ->
				maybe_successful(Error)
		end
	end.

forward_confirmed(ServerDomain, DnsEntry) ->
	sling_log:info("Running forward confirmed test for ~p~n",[ServerDomain]),
	{_Priority, AnswerDomain} = DnsEntry,
	sling_log:info("DNS MX lookup domain ~p~n",[AnswerDomain]),
	Result = case a_lookup(AnswerDomain) of
		{ok, IP} ->
			IPStr = sling_utils:ensure_ip_string(IP),
			sling_log:info("~p maps to ~p~n",[AnswerDomain, IPStr]),
			case ptr_lookup(IPStr) of 
				{ok, PtrDomain} ->
					sling_log:info("PTR lookup returned ~p~n",[PtrDomain]),
					if AnswerDomain =:= PtrDomain ->
						{ok, <<"Forward Confirmed test successful">> };
					true ->
						sling_log:info("Testing if the IP ~p of MX ~p domain "
							"matches the IP address pointed by sub-domain of "
							"~p~n" ,[IPStr, AnswerDomain, PtrDomain]),
						match_domain_ip_to_a_lookup(PtrDomain, IP)
					end;
				Error ->
					Error
			end;
		Error -> 
			Error 
	end,
	maybe_successful(Result).
		

%%
%% @private Used by the @see forward_confirmed/2 for deep verification check.
%%
-spec match_domain_ip_to_a_lookup(string(), tuple()) 
	-> {ok, Any} | {error, Any}.

match_domain_ip_to_a_lookup(Domain, IP) ->
	case inet_res:lookup(Domain, in, a, [{recurse, true}]) of
		[] -> 
			{ error, <<"Empty A answer">> };
		Results when is_list(Results) ->
			IpTuple = sling_utils:ensure_ip_tuple(IP),
			Matched = lists:foldl(fun(Ip, Acc) ->
				case Ip of
					IpTuple -> 
						Acc ++ [Ip];
					_ -> 
						Acc
				end		
			end, [], Results),
			if Matched =:= [] ->
				{ error, <<"No IP match">>};
			true ->
				[ MatchedIP | _ ] = Matched, 
				{ok, MatchedIP}
			end;
		Error ->
			Error
	end.


%% ----------------------------------------------------------------------------

%%
%% @doc
%%

spf_exists() ->
	ServerDomain = sling_config:get_server_domain(),
	spf_exists(ServerDomain).

spf_exists(Domain) ->
	DomainStr = sling_utils:ensure_string(Domain),
	maybe_successful(spf_lookup(DomainStr)).
		
%% ----------------------------------------------------------------------------

%%
%% @doc
%%

dkim_configured() ->
	ServerDomain = sling_config:get_server_domain(),
	Selector = sling_config:get_dkim_selector(),
	dkim_configured(ServerDomain, Selector).

dkim_configured(Domain, Selector) ->
	DomainStr = sling_utils:ensure_string(Domain),
	SelectorStr = sling_utils:ensure_string(Selector),
	sling_log:info("Running DKIM configuration test for ~p using ~p selector~n"
		,[DomainStr, SelectorStr]),
	Result = case dkim_lookup(DomainStr, SelectorStr) of
		{ok, [Dkim]} ->
			DkimConfig = split_dkim(Dkim),
			K = proplists:get_value("k", DkimConfig),
			P = proplists:get_value("p", DkimConfig),
			sling_log:info("DNS records configured with ~p key: ~p~n",[K, P]),
			reconcile_public_key(K, P);
		{ok, MultiValue} ->
			{error, {multivalue, MultiValue}};
		Error ->
			Error
	end,
	maybe_successful(Result).

%%
%% @private
%%

split_dkim(Dkim) ->
	DkimStr = sling_utils:ensure_string(Dkim),
	Tokens = re:split(DkimStr, " ", [{ return, list }]),
	lists:foldl(fun(Token, Acc) -> 
		case re:split(Token, "=", [{ return, list }]) of
			[Key, Val] ->
				Val1 = string:strip(Val, right, $;),
				Acc ++ [{Key, Val1}];
			_ -> 
				Acc
		end
	end, [], Tokens).


%%
%% @private 
%%

reconcile_public_key(Type, PublicKey) when Type =:= "rsa" ->
	sling_log:info("Reconciling public key DNS entry against local private key..~n",[]),
	PubKeyBin = sling_utils:ensure_binary(PublicKey),
	Pub = sling_crypto:key_to_entry(base64, PubKeyBin),
	Msg = <<"asdf">>,
	Sig = sling_crypto:sign_dkim(Msg),
	Verified = sling_crypto:verify_dkim(Msg, Sig, Pub),
	{ok, { <<"DKIM key reconciliation result">>, Verified } };
reconcile_public_key(Type, _PublicKey) ->
	{error, { unmatched, Type }}.


%% ----------------------------------------------------------------------------

%%
%% @doc
%%
-spec dmarc_exists() -> true | false.

dmarc_exists() ->
	ServerDomain = sling_config:get_server_domain(),
	dmarc_exists(ServerDomain).

-spec dmarc_exists(string() | binary()) -> true | false.

dmarc_exists(Domain) ->
	DomainStr = sling_utils:ensure_string(Domain),
	maybe_successful(dmarc_lookup(DomainStr)). 
	

%% ----------------------------------------------------------------------------

%%
%% @private
%%
maybe_successful({ok, Result}) ->
	sling_log:info("Test succeded ~p ~n",[Result]),
	true;
maybe_successful({error, Error}) ->
	sling_log:info("Test failed ~p ~n",[Error]),
	false.
	


