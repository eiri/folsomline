PROJECT = folsomline
ERLC_OPTS = '+{parse_transform, lager_transform}'

DEPS = lager folsom
dep_lager = https://github.com/basho/lager 2.0.3
dep_folsom = https://github.com/boundary/folsom 0.8.1

include erlang.mk
