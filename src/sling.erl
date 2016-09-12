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
	
]).

send_email(Email, Options) ->
  spawn(fun() ->
    {_From, To, _SignedMailBody} = Email,
    case gen_smtp_client:send_blocking(Email, Options) of
      {ok, Receipt} ->
        io:fwrite("Message to ~p delivered successfully ~p~n",[To, Receipt]);
      {error, Type, ErrMsg} ->
        io:fwrite("Message delivery to ~p failed ~p ~p~n",[To, Type, ErrMsg]);
      {exit, ExitReason} ->
        io:fwrite("Message delivery to ~p exited ~p~n",[To, ExitReason])
    end
  end).

send_dkim(Subject,Body,ToName,ToEmail) ->
	ServerDomain = soil_utls:get_env(server_domain),
	case dkim_email(Subject,Body,ToName,ToEmail) of
		{ok, { From, To, SignedMailBody, FQDM } } ->
			Options = [
				{relay, FQDM }
				, {port, 25}
				, {hostname, ServerDomain }
				, {ssl, true}
			],
			soil_log:log("Sending to ~p using ~p relay. ~n",[To, FQDM]),
			Email = {From, [To], SignedMailBody},
			send_email(Email, Options),
			soil_log:log("Message to ~p scheduled for sending. ~n",[To]),
			ok;
		{error,_ErrMsg} ->
			error
	end.


dkim_email(Subject, Body, ToName, ToEmail) ->
  ToStr = binary_to_list(ToEmail),
  case soil_utls:validate_email(ToStr) of
    {true,FQDM} ->
      PrivKey = soil_utls:get_env(email_dkim_key),
      DkimTxtDnsSelector = soil_utls:get_env(dkim_txt_dns_selector),
      NoReplyUser = soil_utls:get_env(email_no_reply_user),
      NoReplyUsername = proplists:get_value(username,NoReplyUser),
      NoReplyName = proplists:get_value(name,NoReplyUser),
      ServerDomain = soil_utls:get_env(server_domain),
      DKIMOptions = [
        {s,DkimTxtDnsSelector}
        ,{d, ServerDomain}
        ,{private_key, {pem_plain, PrivKey}}
      ],
      NoReplyFromEmail = iolist_to_binary([ NoReplyUsername,<<"@">>,ServerDomain ]),
      NoReplyFrom = iolist_to_binary([
        NoReplyName, <<" <">>, NoReplyFromEmail, <<">">>
      ]),
      FullToName = iolist_to_binary([
        ToName, <<" <">>, ToEmail, <<">">>
      ]),

      SignedMailBody = mimemail:encode({ <<"text">>, <<"html">>,[
          {<<"Subject">>, Subject}
          ,{<<"From">>, NoReplyFrom}
          ,{<<"To">>, FullToName }
        ], [], Body },[{dkim, DKIMOptions}]),

      FromStr = binary_to_list(NoReplyFromEmail),
      {ok, { FromStr, ToStr, SignedMailBody, FQDM } };
    {false,_FQDM} ->
      Msg = <<"Could not determine relay domain">>,
      soil_log:log("~p ~p ~p ~n",[Msg,ToName,ToEmail]),
      { error, <<"Could not validate determine the relay domain">> }
  end.



