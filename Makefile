PROJECT = trails

DEPS = cowboy ranch
TEST_DEPS = xref_runner katana mixer elvis meck
SHELL_DEPS = sync

dep_cowboy = git https://github.com/ninenines/cowboy.git 1.0.4
dep_ranch = git https://github.com/ninenines/ranch.git 1.2.0
dep_sync =  git https://github.com/rustyio/sync.git  9c78e7b
dep_xref_runner = git https://github.com/inaka/xref_runner.git 0.2.2
dep_mixer = git https://github.com/inaka/mixer.git 0.1.4
dep_elvis = git https://github.com/inaka/elvis.git b69eea4
dep_katana = git https://github.com/inaka/erlang-katana.git 07efe94
dep_meck = git https://github.com/eproxus/meck 0.8.3

include erlang.mk

CT_OPTS = -cover test/trails.coverspec -erl_args

LOCAL_DEPS := xmerl tools compiler syntax_tools common_test inets crypto ssl public_key test_server dialyzer wx
DIALYZER_DIRS := ebin/ test/
DIALYZER_OPTS := --verbose --statistics -Wunmatched_returns

ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

TEST_ERLC_OPTS += +debug_info

SHELL_OPTS = -s sync

quicktests: app
	@$(MAKE) --no-print-directory app-build test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
	$(verbose) mkdir -p $(CURDIR)/logs/
	$(gen_verbose) $(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS)

test-build-plt: ERLC_OPTS=$(TEST_ERLC_OPTS)
test-build-plt:
	@$(MAKE) --no-print-directory test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
	$(gen_verbose) touch ebin/test

plt-all: PLT_APPS := $(TEST_DEPS)
plt-all: test-deps test-build-plt plt

dialyze-all: app test-build-plt dialyze
