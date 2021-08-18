# Typical usage: `make --makefile parallel-tezt.Makefile --jobs`
# The goal of this dedicated Makefile is to run all Tezt (regression) tests in parallel to speed things up.
# The way it works is by generating a Makefile target for each Tezt test, then declare a parent target `tezt-parallel` that depends on all these targets.
# This is not in the main Makefile because to list Tezt tests, we run a `dune exec` command, which would be executed everytime a `make` command is run, even if it is unrelated to Tezt tests.
# This is also at the root and not in the `tezt` directory so as to not mess with the current working directory (CWD) code that relies on `tezt` being launched from the root of the Tezos repository.
# Note that in parallel mode, you may see warnings like "[warn] Leftover temporary file from previous run: /run/user/1000/tezt-26298".
# This is expected as the leftover check is "dumb" and doesn't know those files will be cleaned by another concurrent Tezt process.

.PHONY: all
all: tezt-parallel

# Use `:=` to prevent reexpanding its value everytime it is used
# We replace spaces in test names with a symbol, otherwise this is interpreted as list separators by Make.
# This must be replaced back when passing to the `--test` argument BUT NOT BEFORE or it would break the target names.
test-names := $(shell dune exec tezt/tests/main.exe -- --list-tsv | cut -f 2 | sort -u | sed 's/ /(SPACE)/g' | sed 's/=/(EQUAL)/g' | sed 's/%/(PERCENT)/g' | sed 's/:/(COLON)/g')
targets = $(foreach file-name, $(test-names), $(file-name).tezt)
# Iterate on all tests, associate a good starting port to it, then call "make-tezt-target" on the pair
n-test-names = $(words $(test-names))
starting-ports = $(shell for i in `seq 1 $(n-test-names)` ; do echo $$((19732 + ((i - 1) * 10))); done)
indices = $(shell for i in `seq 1 $(n-test-names)` ; do echo $$i; done)

# "make-tezt-target file-name starting-port" generates a target "file-name.tezt" that runs this test file with the given starting port, to avoid the same port being used by concurrent tests.
# Technically you can invoke `make --makefile parallel-tezt.Makefile foo.ml.tezt` to invoke a single Tezt file `foo.ml`, though in practice this defeats the whole point of this Makefile.
define make-tezt-target

.PHONY: $(1).tezt
$(1).tezt: build-tezt
	./_build/default/tezt/tests/main.exe --test "$(subst (SPACE), ,$(subst (EQUAL),=,$(subst (PERCENT),%,$(subst (COLON),:,$(1)))))" --starting-port $(2)

endef

# Generate a target for each Tezt test, with a different starting port, to avoid collisions
$(foreach i,$(indices),$(eval $(call make-tezt-target,$(word $(i),$(test-names)),$(word $(i),$(starting-ports)))))

# Each Tezt file name is converted into a target, enabling Make parallelization capabilities with "-j"
.PHONY: tezt-parallel
tezt-parallel: $(targets)

# Dune does not handle elegantly concurrent executions, so we build once with Dune, then use the executable directly, instead of calling `dune exec`
.PHONY: build-tezt
build-tezt:
	@dune build tezt/tests/main.exe
