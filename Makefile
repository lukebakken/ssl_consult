PROJECT = ssl_consult
PROJECT_DESCRIPTION = Utility function to read .config files with SSL settings
PROJECT_VERSION = 1.0.0

DIALYZER_OPTS += --src -r test -Wunmatched_returns -Werror_handling
EUNIT_OPTS = no_tty, {report, {eunit_progress, [colored, profile]}}
include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)
