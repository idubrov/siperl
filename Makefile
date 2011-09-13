#!/usr/bin/make
REBAR=./rebar

.PHONY : all deps compile test clean
all: deps compile
deps:
	@$(REBAR) get-deps update-deps
compile:
	@$(REBAR) compile
test: deps
	@$(REBAR) skip_deps=true eunit ct
clean:
	@$(REBAR) clean
check: compile
	@dialyzer apps/sip/ebin --verbose -Wunmatched_returns -Werror_handling -Wrace_conditions
