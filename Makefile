#!/usr/bin/make
REBAR=./rebar
all: deps compile
deps:
	@$(REBAR) get-deps
compile:
	@$(REBAR) compile
test:
	@$(REBAR) skip_deps=true eunit ct
clean:
	@$(REBAR) clean

