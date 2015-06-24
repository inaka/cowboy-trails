PROJECT = trails

SHELL_DEPS = sync

dep_sync =  git git://github.com/inaka/sync.git  0.1.3
dep_xref_runner = git git://github.com/inaka/xref_runner.git 0.2.2

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'

SHELL_OPTS = -s sync

# Commont Test Config
CT_DEPS = xref_runner

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'
CT_OPTS = -cover test/trails.coverspec -erl_args
