PROJECT = fticker
ERLC_OPTS = '+{parse_transform, lager_transform}'

DEPS = lager folsom folsomline list_as_table
dep_lager = https://github.com/basho/lager 2.0.3
dep_folsom = https://github.com/boundary/folsom 0.8.1
dep_folsomline = https://github.com/eiri/folsomline 0.1.2
dep_list_as_table = https://github.com/eiri/list_as_table 0.1.0

include ../erlang.mk

ERL := $(shell which erl)
HOSTNAME := $(shell hostname)

run: app
	exec ${ERL} -pa ${PWD}/ebin -pa ${PROJECT}/deps/*/ebin -name ${PROJECT}@${HOSTNAME} -config ${PWD}/etc/app.config -s ${PROJECT}
