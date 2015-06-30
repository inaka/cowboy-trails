PROJECT = example

DEPS = cowboy cowlib ranch mixer cowboy_trails jiffy

dep_cowboy = git https://github.com/ninenines/cowboy  1.0.1
dep_cowlib = git https://github.com/ninenines/cowlib.git 1.0.0
dep_ranch = git https://github.com/ninenines/ranch.git 1.0.0
dep_mixer = git https://github.com/opscode/mixer.git 0.1.3
dep_jiffy  = git git://github.com/davisp/jiffy.git 0.11.3
dep_cowboy_trails = git https://github.com/inaka/cowboy-trails 75bf34ba25788cccbd3292f55aaa865f00b2c256

include ../erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

CT_SUITES = root_handler
CT_OPTS = -erl_args -config rel/sys.config

SHELL_OPTS = -name ${PROJECT}@`hostname` -s ${PROJECT} -config rel/sys.config
