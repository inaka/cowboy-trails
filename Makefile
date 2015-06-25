PROJECT = trails

DEPS = cowboy

dep_cowboy = git git://github.com/extend/cowboy.git 1.0.1

SHELL_DEPS = sync

dep_sync =  git git://github.com/inaka/sync.git  0.1.3

TEST_DEPS = xref_runner

dep_xref_runner = git git://github.com/inaka/xref_runner.git 0.2.2

include erlang.mk

SHELL_OPTS = -s sync

# Commont Test Config
CT_DEPS = xref_runner

CT_OPTS = -cover test/trails.coverspec -erl_args
