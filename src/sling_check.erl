%%%-------------------------------------------------------------------
%%% @author szymon.czaja
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Sep 2016 12:30
%%%-------------------------------------------------------------------
-module(sling_check).
-author("szymon.czaja").

%% API
-export([
    ptr/0
]).


ptr() ->
    ok.


mx_lookup(Domain) ->
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
    end,
    case inet_res:lookup(Domain, in, mx) of
        [] ->
            [];
        Result ->
            lists:sort(fun({Pref, _Name}, {Pref2, _Name2}) -> Pref =< Pref2 end, Result)
    end.

