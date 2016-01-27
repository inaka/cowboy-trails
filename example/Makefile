PROJECT = example

DEPS = mixer

dep_mixer   = git https://github.com/inaka/mixer.git  0.1.3

LOCAL_DEPS = cowboy_trails

SHELL_DEPS = sync

dep_sync =  git https://github.com/inaka/sync.git  0.1.3

include erlang.mk

ERLC_OPTS += +'{parse_transform}'

SHELL_OPTS = -name ${PROJECT}@`hostname` -s ${PROJECT} -config rel/sys.config -s sync
