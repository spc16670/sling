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
    		sling_sup:start_link();
		false ->
			{error, <<"System check failed.">>}
	end.

stop(_State) ->
    ok.



