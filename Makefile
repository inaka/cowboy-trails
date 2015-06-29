PROJECT = trails

DEPS = cowboy cowlib ranch

dep_cowboy = git git://github.com/extend/cowboy.git 1.0.1
dep_cowlib = git https://github.com/ninenines/cowlib.git 1.0.0
dep_ranch = git https://github.com/ninenines/ranch.git 1.0.0

SHELL_DEPS = sync

dep_sync =  git git://github.com/inaka/sync.git  0.1.3

TEST_DEPS = xref_runner cowboy

dep_xref_runner = git git://github.com/inaka/xref_runner.git 0.2.2

PLT_APPS := cowboy
DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Werror_handling \
                 -Wrace_conditions

include erlang.mk

SHELL_OPTS = -s sync

# Commont Test Config
CT_DEPS = xref_runner cowboy

CT_OPTS = -cover test/trails.coverspec -erl_args
