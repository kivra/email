PROJECT = email

# Options
COMPILE_FIRST = email_adapter
EUNIT_OPTS    = [verbose]
ERLC_OPTS    ?= -Werror +debug_info +warn_export_all +warn_export_vars \
                +warn_shadow_vars +warn_obsolete_guard -DS2_USE_LAGER \
                #+warn_missing_spec
TEST_DEPS = meck

# Dependencies
dep_meck = git@github.com:eproxus/meck.git 0.8.2

# Standard targets
include erlang.mk

# eof
