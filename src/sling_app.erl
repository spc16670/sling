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
	case run_config_check() of
		true ->
    		sling_sup:start_link();
		false ->
			{error, <<"System check failed.">>}
	end.

stop(_State) ->
    ok.

run_config_check() ->
	ok = sling_dns:init(),
	maybe_forward_confirmed(sling_dns:forward_confirmed()).
	
maybe_forward_confirmed(true) ->
	true;
maybe_forward_confirmed(false) ->
	false.



