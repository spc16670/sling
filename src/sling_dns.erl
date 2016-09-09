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
	,mx_lookup/1
	,a_lookup/1
	,ptr_lookup/1
	,spf_lookup/1
	,dkim_lookup/2
	,forward_confirmed/0
	,forward_confirmed/1
	,spf_exists/0
	,spf_exists/1
	,dkim_configured/0
	,dkim_configured/2
]).

%% ----------------------------------------------------------------------------
%% init
%%
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

%%
%%
%%
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
%%
%%
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
%%
%%
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
%%
%%
dkim_lookup(Domain, Selector) ->
	DomainStr = sling_utils:ensure_string(Domain),
	SelectorStr = sling_utils:ensure_string(Selector),
	FullSelector = SelectorStr ++ "._domainkey." ++ DomainStr,
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
%%
%%
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
		{ error, <<"Server domain undefined">> };
	true ->
		DomainStr = sling_utils:ensure_string(ServerDomain),
		case mx_lookup(DomainStr) of
			{ok, Sorted} ->
				[FirstRecord | _ ] = Sorted,
				forward_confirmed(DomainStr, FirstRecord);
			Error ->
				Error
		end
	end.

forward_confirmed(ServerDomain, DnsEntry) ->
	sling_log:info("Running forward confirmed test for ~p~n",[ServerDomain]),
	{_Priority, AnswerDomain} = DnsEntry,
	sling_log:info("DNS MX lookup domain ~p~n",[AnswerDomain]),
	case a_lookup(AnswerDomain) of
		{ok, IP} ->
			IPStr = sling_utils:ensure_ip_string(IP),
			sling_log:info("~p maps to ~p~n",[AnswerDomain, IPStr]),
			case ptr_lookup(IPStr) of 
				{ok, PtrDomain} ->
					sling_log:info("PTR lookup returned ~p~n",[PtrDomain]),
					if AnswerDomain =:= PtrDomain ->
						sling_log:info("Forward Confirmed test successful~n",[]),
						true;
					true ->
						sling_log:info("Testing if the IP ~p of MX ~p domain "
							"matches the IP address pointed by sub-domain of "
							"~p~n" ,[IPStr, AnswerDomain, PtrDomain]),
						case match_domain_ip_to_a_lookup(PtrDomain, IP) of
							{ok, MatchedIP} ->
								MatchedIPStr = sling_utils:ensure_ip_string(MatchedIP),
								sling_log:info("IP ~p pointed by MX ~p matches the IP "
								"pointed by PTR ~p~n", [MatchedIPStr, AnswerDomain, PtrDomain]),
								sling_log:info("Forward Confirmed test successful~n",[]),
								true;
							Error ->
								sling_log:info("Forward Confirmed test failed ~p ~n",[Error]),
								false
						end
					end;
				Error ->
					Error
			end;
		Error -> 
			Error 
	end.	
		

match_domain_ip_to_a_lookup(Domain,IP) ->
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
				{ ok , MatchedIP }
			end;
		Error ->
			{ error, Error }
	end.


%% ----------------------------------------------------------------------------

spf_exists() ->
	ServerDomain = sling_config:get_server_domain(),
	spf_exists(ServerDomain).

spf_exists(Domain) ->
	DomainStr = sling_utils:ensure_string(Domain),
	case spf_lookup(DomainStr) of 
		{ok, Records} ->
			sling_log:info("SPF lookup successful: ~p~n",[Records]),
			true;
		{error, Reason} ->
			sling_log:info("SPF lookup failed: ~p~n",[Reason]),
			false
	end.
		
%% ----------------------------------------------------------------------------

dkim_configured() ->
	ServerDomain = sling_config:get_server_domain(),
	Selector = sling_config:get_dkim_selector(),
	dkim_configured(ServerDomain, Selector).

dkim_configured(Domain, Selector) ->
	DomainStr = sling_utils:ensure_string(Domain),
	SelectorStr = sling_utils:ensure_string(Selector),
	sling_log:info("Running DKIM configuration test for ~p using ~p selector~n"
		,[DomainStr, SelectorStr]),
	case dkim_lookup(DomainStr, SelectorStr) of
		{ok, DKIM} ->
			DKIM;
		Error ->
			Error
	end.


		



