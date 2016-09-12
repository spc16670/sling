-module(sling_app).

-behaviour(application).

%% Application callbacks
-export([
	start/2
	, stop/1
]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	case sling_dns:dns_config_validated() of
		true ->
			application:ensure_started(asn1),
			application:ensure_started(crypto),
			application:ensure_started(public_key),
			application:ensure_started(ssl),
    		sling_sup:start_link();
		false ->
			{error, <<"System check failed.">>}
	end.

stop(_State) ->
    ok.



