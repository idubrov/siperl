#!/bin/sh

run_test -pa deps/*/ebin/ -pa apps/*/ebin -spec $1
