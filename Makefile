PROJECT = trails

DEPS = lager sync
dep_lager = git git://github.com/basho/lager.git 2.1.1
dep_sync =  git git://github.com/inaka/sync.git  0.1.3

include erlang.mk

SHELL_OPTS = -s sync
