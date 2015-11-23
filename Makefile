PROJECT = trails

DEPS = cowboy cowlib ranch
TEST_DEPS = xref_runner
SHELL_DEPS = sync

dep_cowboy = git https://github.com/ninenines/cowboy.git 1.0.1
dep_cowlib = git https://github.com/ninenines/cowlib.git 1.0.0
dep_ranch  = git https://github.com/ninenines/ranch.git 1.0.0
dep_sync =  git https://github.com/rustyio/sync.git  9c78e7b
dep_xref_runner = git https://github.com/inaka/xref_runner.git 0.2.2

PLT_APPS := cowboy
DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Werror_handling \
                 -Wrace_conditions

include erlang.mk

SHELL_OPTS = -s sync

CT_OPTS = -cover test/trails.coverspec -erl_args
