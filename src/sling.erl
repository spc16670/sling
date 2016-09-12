%%%-------------------------------------------------------------------
%%% @author szymon.czaja
%%% @copyright (C) 2016, IONAS SOFTWARE LTD
%%% @doc
%%%
%%% @end
%%% Created : 08. Sep 2016 13:24
%%%-------------------------------------------------------------------
-module(sling).
-author("szymon.czaja").

%% API
-export([
	send_email/2
	,send_dkim/4	
]).

%%
%% @doc
%%

send_email(Email, Options) ->
	spawn(fun() ->
		{_From, To, _SignedMailBody} = Email,
			case gen_smtp_client:send_blocking(Email, Options) of
				{ok, Receipt} ->
					io:fwrite("Message to ~p delivered successfully ~p~n",[To, Receipt]);
				{error, Type, ErrMsg} ->
					io:fwrite("Message delivery to ~p failed ~p ~p~n",[To, Type, ErrMsg]);
				{exit, ExitReason} ->
					io:fwrite("Message delivery to ~p exited ~p~n",[To, ExitReason]);
				Receipt ->
					io:fwrite("Message to ~p delivered successfully ~p~n",[To, Receipt])
			end
	end).

%%
%% @doc
%%

send_dkim(Subject, Body, ToName, ToEmail) ->
	%ServerDomain = sling_config:get_server_domain(),
	case sling_email:dkim_email(Subject, Body, ToName, ToEmail) of
		{ok, { From, To, SignedMailBody, FQDM } } ->
			Options = [
				{relay, FQDM }
				%% ,{tls, always} will be auto-negotiated if available
			],
			sling_log:info("Sending to ~p using ~p relay. ~n",[To, FQDM]),
			Email = {From, [To], SignedMailBody},
			send_email(Email, Options),
			sling_log:info("Message to ~p scheduled for sending. ~n",[To]),
			ok;
		{error,_ErrMsg} ->
			error
	end.


