#!/bin/sh

run_test -pa deps/*/ebin/ -pa apps/*/ebin -cover apps/sip/test/cover.spec -spec $@
