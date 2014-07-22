PROJECT = folsomline

ERL := $(shell which erl)
HOSTNAME := $(shell hostname)
ERLC_OPTS = '+{parse_transform, lager_transform}'

DEPS = lager folsom
dep_lager = https://github.com/basho/lager 2.0.3
dep_folsom = https://github.com/boundary/folsom 0.8.1

.PHONY: run
include erlang.mk

run: app
	exec ${ERL} -pa ${PWD}/ebin ${PWD}/deps/*/ebin -name ${PROJECT}@${HOSTNAME} -config ${PWD}/etc/app.config -s ${PROJECT}
