PROJECT = trails

DEPS = lager sync xref_runner
dep_lager = git git://github.com/basho/lager.git 2.1.1
dep_sync =  git git://github.com/inaka/sync.git  0.1.3
dep_xref_runner = git git://github.com/inaka/xref_runner.git 0.2.2

include erlang.mk

SHELL_OPTS = -s sync
