PROJECT = example

DEPS = cowboy mixer cowboy_trails

dep_cowboy = git https://github.com/ninenines/cowboy  1.0.1
dep_mixer = git https://github.com/opscode/mixer.git 0.1.3
dep_cowboy_trails = git https://github.com/inaka/cowboy-trails 75bf34ba

include erlang.mk

ERLC_OPTS += +'{parse_transform}'

SHELL_OPTS = -name ${PROJECT}@`hostname` -s ${PROJECT} -config rel/sys.config
