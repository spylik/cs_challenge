PROJECT = cs_challenge

# -------------------------------------------------------------------
# Speedup fetching

REBAR_GIT_CLONE_OPTIONS += --depth 1
export REBAR_GIT_CLONE_OPTIONS

# --------------------------------------------------------------------
# Compilation.
# --------------------------------------------------------------------

# default mode
ERLC_OPTS += +warn_export_all +warn_export_vars +warn_unused_import +warn_untyped_record
ERLC_OPTS += +warn_missing_spec_all
ERLC_OPTS += -Werror
ERLC_OPTS += +debug_info

# --------------------------------------------------------------------
#  # Dependencies.
# --------------------------------------------------------------------

dep_thoas = git https://github.com/kawbo/thoas			feature/fix_typespec_of_json_term
dep_cowboy = git https://github.com/ninenines/cowboy	2.12.0

DEPS = thoas cowboy relx

SHELL_DEPS = sync

DEP_PLUGINS = cowboy relx

PLT_APPS = ssl crypto stdlib inets public_key thoas ranch

# --------------------------------------------------------------------
#  # TDD Shell mode
# --------------------------------------------------------------------

SHELL_OPTS = -mode interactive -kernel shell_history enabled -pa ebin/ test/ -I -env ERL_LIBS deps -env LOGGER_CHARS_LIMIT unlimited -env LOGGER_DEPTH unlimited -eval 'sync:start(), sync:onsync(fun(Mods) -> [eunit:test(Mod) || Mod <- Mods] end), [lists:map(fun(Module) -> code:ensure_loaded(list_to_atom(lists:takewhile(fun(X) -> X /= 46 end, lists:subtract(Module,Dir)))) end, filelib:wildcard(Dir++"*.erl")) || Dir <- ["src/","test/"], ok =:= filelib:ensure_dir(Dir)]'

# --------------------------------------------------------------------
# Configuration for RELX
# --------------------------------------------------------------------

RELX = deps/relx/relx

# --------------------------------------------------------------------
# We using erlang.mk
# --------------------------------------------------------------------

include erlang.mk
