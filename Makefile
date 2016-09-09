REBAR = ./rebar

.PHONY: all compile test clean get-deps build-plt dialyze

all: compile

compile:
	$(REBAR) compile

test: 
	export ERL_FLAGS="-config sling.config"; $(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

get-deps:
	@$(REBAR) get-deps

build-plt:
	@$(REBAR) build-plt

dialyze: compile
	@$(REBAR) dialyze
