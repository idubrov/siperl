#!/usr/bin/make
REBAR=./rebar

.PHONY : all deps compile test clean
all: deps compile
deps:
	@$(REBAR) get-deps update-deps
compile:
	@$(REBAR) compile
test: deps
	-@$(REBAR) skip_deps=true eunit ct
	erl -noshell -pa ebin deps/*/ebin -run ct_surefire to_surefire_xml $(CURDIR)/apps/sip/logs $(CURDIR)/apps/sip/logs -s init stop
clean:
	@$(REBAR) clean
check: compile dialyzer
dialyzer:
	@dialyzer --no_check_plt \
	          -Wunmatched_returns \
	          -Werror_handling \
	          -Wrace_conditions \
	          --quiet \
	          apps/sip/ebin \
	          demos/hang/ebin \
	          demos/busy/ebin
