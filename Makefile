PACKAGES_SUBPROJECT:=$(patsubst %.opam,%,$(notdir $(shell find src vendors -name \*.opam -print)))
PACKAGES:=$(patsubst %.opam,%,$(notdir $(wildcard opam/*.opam)))

define directory_of_version
src/proto_$(shell echo $1 | tr -- - _)
endef

# Opam is not present in some build environments. We don't strictly need it.
# Those environments set TEZOS_WITHOUT_OPAM.
ifndef TEZOS_WITHOUT_OPAM
current_opam_version := $(shell opam --version)
endif

include scripts/version.sh

COVERAGE_REPORT := _coverage_report
COBERTURA_REPORT := _coverage_report/cobertura.xml
PROFILE?=dev
VALID_PROFILES=dev release static

# See the documentation of [~release_status] in [manifest/manifest.mli].
RELEASED_EXECUTABLES := $(shell cat script-inputs/released-executables)
EXPERIMENTAL_EXECUTABLES := $(shell cat script-inputs/experimental-executables)

# Executables that developers need at the root of the repository but that
# are not useful for users.
# - scripts/snapshot_alpha.sh expects octez-protocol-compiler to be at the root.
# - Some tests expect octez-snoop to be at the root.
DEV_EXECUTABLES := $(shell cat script-inputs/dev-executables)

ALL_EXECUTABLES := $(RELEASED_EXECUTABLES) $(EXPERIMENTAL_EXECUTABLES) $(DEV_EXECUTABLES)

#Define octez only executables.
OCTEZ_ONLY_RELEASED_EXECUTABLES := $(shell cat script-inputs/octez-released-executables)
OCTEZ_ONLY_EXPERIMENTAL_EXECUTABLES := $(shell cat script-inputs/octez-experimental-executables)
OCTEZ_ONLY_EXECUTABLES := $(OCTEZ_ONLY_RELEASED_EXECUTABLES) $(OCTEZ_ONLY_EXPERIMENTAL_EXECUTABLES)

#Define octez layer1 only executables by excluding the EVM-node and teztale tools.
OCTEZ_ONLY_LAYER1_EXECUTABLES := $(filter-out etherlink-governance-observer octez-evm-node octez-teztale-archiver octez-teztale-server octez-teztale-snitch octez-tezindex octez-smart-rollup-wasm-debugger octez-smart-rollup-node,$(RELEASED_EXECUTABLES) $(EXPERIMENTAL_EXECUTABLES))

TEZTALE_EXECUTABLES := $(shell cat script-inputs/teztale-experimental-executables)

TEZINDEX_EXECUTABLES := $(shell cat script-inputs/tezindex-experimental-executables)

EVM_EXECUTABLES := $(shell cat script-inputs/etherlink-experimental-executables)

# Set of Dune targets to build, in addition to OCTEZ_EXECUTABLES, in
# the `build` target's Dune invocation. This is used in the CI to
# build the TPS evaluation tool, Octogram and the Tezt test suite in the
# 'build_x86_64-dev-exp-misc' job.
BUILD_EXTRA ?=

# set a default parallelism of dune
# to be instantiated in the CI to avoid OOMs (e.g. "-j 12")
DUNE_BUILD_JOBS ?=

# See first mention of TEZOS_WITHOUT_OPAM.
ifndef TEZOS_WITHOUT_OPAM
ifeq ($(filter ${opam_version_major}.%,${current_opam_version}),)
$(error Unexpected opam version (found: ${current_opam_version}, expected: ${opam_version_major}.*))
endif
endif

ifeq ($(filter ${VALID_PROFILES},${PROFILE}),)
$(error Unexpected dune profile (got: ${PROFILE}, expecting one of: ${VALID_PROFILES}))
endif

# This check ensures that the `OCTEZ_EXECUTABLES` variable contains a subset of
# the `ALL_EXECUTABLE` variable. The `OCTEZ_EXECUTABLES` variable is used
# internally to select a subset of which executables to build.
# The reason for the `foreach` is so that we support both newlines and spaces.
ifneq ($(filter ${ALL_EXECUTABLES},${OCTEZ_EXECUTABLES}),$(foreach executable,${OCTEZ_EXECUTABLES},${executable}))
$(error Unexpected list of executables to build, make sure environment variable OCTEZ_EXECUTABLES is unset)
endif

# Where to copy executables.
# Used when building Docker images to help with the COPY instruction.
OCTEZ_BIN_DIR?=.

VALID_OCTEZ_BIN_DIRS=. bin

ifeq ($(filter ${VALID_OCTEZ_BIN_DIRS},${OCTEZ_BIN_DIR}),)
$(error Unexpected value for OCTEZ_BIN_DIR (got: ${OCTEZ_BIN_DIR}, expecting one of: ${VALID_OCTEZ_BIN_DIRS}))
endif

# See first mention of TEZOS_WITHOUT_OPAM.
ifdef TEZOS_WITHOUT_OPAM
current_ocaml_version := $(shell ocamlc -version)
else
current_ocaml_version := $(shell opam exec -- ocamlc -version)
endif

# Default target.
#
# Note that you can override the list of executables to build on the command-line, e.g.:
#
#     make OCTEZ_EXECUTABLES='octez-node octez-client'
#
# This is more efficient than 'make octez-node octez-client'
# because it only calls 'dune' once.
#
# Targets 'all', 'release', 'experimental-release' and 'static' define
# different default lists of executables to build but they all can be
# overridden from the command-line.
.PHONY: all
all:
	@$(MAKE) build OCTEZ_EXECUTABLES?="$(ALL_EXECUTABLES)"

.PHONY: release
release:
	@$(MAKE) build PROFILE=release OCTEZ_EXECUTABLES?="$(RELEASED_EXECUTABLES)"

.PHONY: octez
octez:
	@$(MAKE) build PROFILE=release OCTEZ_EXECUTABLES?="$(OCTEZ_ONLY_EXECUTABLES)"

.PHONY: octez-layer1
octez-layer1:
	@$(MAKE) build OCTEZ_EXECUTABLES?="$(OCTEZ_ONLY_LAYER1_EXECUTABLES)"

.PHONY: teztale
teztale:
	@$(MAKE) build OCTEZ_EXECUTABLES?="${TEZTALE_EXECUTABLES}"

.PHONY: tezindex
tezindex:
	@$(MAKE) build OCTEZ_EXECUTABLES?="${TEZINDEX_EXECUTABLES}"

.PHONY: experimental-release
experimental-release:
	@$(MAKE) build PROFILE=release OCTEZ_EXECUTABLES?="$(RELEASED_EXECUTABLES) $(EXPERIMENTAL_EXECUTABLES)"

.PHONY: build-additional-tezt-test-dependency-executables
build-additional-tezt-test-dependency-executables:
	@dune build contrib/octez_injector_server/octez_injector_server.exe

.PHONY: strip
strip: all
	@chmod +w $(ALL_EXECUTABLES)
	@strip -s $(ALL_EXECUTABLES)
	@chmod -w $(ALL_EXECUTABLES)

.PHONY: static
static:
	@$(MAKE) build PROFILE=static OCTEZ_EXECUTABLES?="$(RELEASED_EXECUTABLES)"

.PHONY: build-parameters
build-parameters:
	@dune build --profile=$(PROFILE) $(COVERAGE_OPTIONS) @copy-parameters

.PHONY: $(ALL_EXECUTABLES)
$(ALL_EXECUTABLES): check-slim-mode check-custom-flags
	dune build $(DUNE_BUILD_JOBS) $(COVERAGE_OPTIONS) --profile=$(PROFILE) _build/install/default/bin/$@
	cp -f _build/install/default/bin/$@ ./

# If slim mode is active, kaitai updates should fail, as some protocol encoding
# will be missing.
# This check is disabled if file scripts/slim-mode.sh is not available,
# which may be the case in Docker images or tarballs for instance.
.PHONY: kaitai-fail-slim-mode
kaitai-fail-slim-mode:
	@if [ -f scripts/slim-mode.sh ]; then scripts/slim-mode.sh fail; fi || (echo "Cannot check kaitai struct files, slim mode is active."; exit 1)

.PHONY: kaitai-struct-files-update
kaitai-struct-files-update: kaitai-fail-slim-mode
	@dune exe client-libs/bin_codec_kaitai/codec.exe dump kaitai specs in client-libs/kaitai-struct-files/files

.PHONY: kaitai-struct-files
kaitai-struct-files: kaitai-fail-slim-mode
	@$(MAKE) kaitai-struct-files-update
	@$(MAKE) -C client-libs/kaitai-struct-files/

.PHONY: check-kaitai-struct-files
check-kaitai-struct-files: kaitai-fail-slim-mode
	@git diff --exit-code HEAD -- client-libs/kaitai-struct-files/files || (echo "Cannot check kaitai struct files, some changes are uncommitted"; exit 1)
	@dune build client-libs/bin_codec_kaitai/codec.exe
	@rm client-libs/kaitai-struct-files/files/*.ksy
	@_build/default/client-libs/bin_codec_kaitai/codec.exe dump kaitai specs in client-libs/kaitai-struct-files/files 2>/dev/null
	@git add client-libs/kaitai-struct-files/files/*.ksy
	@git diff --exit-code HEAD -- client-libs/kaitai-struct-files/files/ || (echo "Kaitai struct files mismatch. Update the files with `make kaitai-struct-files-update`."; exit 1)

.PHONY: validate-kaitai-struct-files
validate-kaitai-struct-files: kaitai-fail-slim-mode
	@$(MAKE) check-kaitai-struct-files
	@./client-libs/kaitai-struct-files/scripts/kaitai_e2e.sh client-libs/kaitai-struct-files/files 2>/dev/null || \
	 (echo "To see the full log run: \"./client-libs/kaitai-struct-files/scripts/kaitai_e2e.sh client-libs/kaitai-struct-files/files client-libs/kaitai-struct-files/input\""; exit 1)

# If slim mode is active, print a message before building anything.
# This check is disabled if file scripts/slim-mode.sh is not available,
# which may be the case in Docker images or tarballs for instance.
.PHONY: check-slim-mode
check-slim-mode:
	@if [ -f scripts/slim-mode.sh ]; then scripts/slim-mode.sh check; fi

# If custom flags are active, print a message before building anything.
# This check is disabled if file scripts/custom-flags.sh is not available,
# which may be the case in Docker images or tarballs for instance.
.PHONY: check-custom-flags
check-custom-flags:
	@if [ -f scripts/custom-flags.sh ]; then scripts/custom-flags.sh check; fi

# Remove the old names of executables.
# Depending on the commit you are updating from (v14.0, v15 or some version of master),
# the exact list can vary. We just remove all of them.
# Don't try to generate this list from *_EXECUTABLES: this list should not evolve as
# we add new executables, and this list should contain executables that were built
# before (e.g. old protocol daemons) but that are no longer built.
.PHONY: clean-old-names
clean-old-names:
	@rm -f tezos-node
	@rm -f tezos-validator
	@rm -f tezos-client
	@rm -f tezos-admin-client
	@rm -f tezos-signer
	@rm -f tezos-codec
	@rm -f tezos-protocol-compiler
	@rm -f tezos-proxy-server
	@rm -f octez-proxy-server
	@rm -f tezos-baker-012-Psithaca
	@rm -f tezos-accuser-012-Psithaca
	@rm -f tezos-baker-013-PtJakart
	@rm -f tezos-accuser-013-PtJakart
	@rm -f tezos-tx-rollup-node-013-PtJakart
	@rm -f tezos-baker-015-PtLimaPt
	@rm -f tezos-accuser-015-PtLimaPt
	@rm -f tezos-tx-rollup-node-015-PtLimaPt
	@rm -f tezos-baker-alpha
	@rm -f tezos-accuser-alpha
	@rm -f tezos-smart-rollup-node-alpha
	@rm -f tezos-snoop
	@rm -f tezos-dal-node
# octez-validator should stay in this list for Octez 16.0 because we
# removed the executable
	@rm -f octez-validator
	@rm -f octez-baker-012-Psithaca
	@rm -f octez-accuser-012-Psithaca
	@rm -f octez-baker-013-PtJakart
	@rm -f octez-accuser-013-PtJakart
	@rm -f octez-tx-rollup-node-013-PtJakart
	@rm -f octez-baker-015-PtLimaPt
	@rm -f octez-accuser-015-PtLimaPt
	@rm -f octez-tx-rollup-node-015-PtLimaPt
	@rm -f octez-smart-rollup-node-PtMumbai
	@rm -f octez-smart-rollup-node-PtNairob
	@rm -f octez-smart-rollup-node-Proxford
	@rm -f octez-smart-rollup-node-alpha

# See comment of clean-old-names for an explanation regarding why we do not try
# to generate the symbolic links from *_EXECUTABLES.
.PHONY: build
build: check-slim-mode check-custom-flags clean-old-names
ifneq (${current_ocaml_version},${ocaml_version})
	$(error Unexpected ocaml version (found: ${current_ocaml_version}, expected: ${ocaml_version}))
endif
ifeq (${OCTEZ_EXECUTABLES},)
	$(error The build target requires OCTEZ_EXECUTABLES to be specified. Please use another target (e.g. 'make' or 'make release') and make sure that environment variable OCTEZ_EXECUTABLES is unset)
endif
# [dune.sh] is a wrapper around [dune] that does not change the core build logic
# but enables cache monitoring when DUNE_CACHE_INFO=true.
# In CI (detected via GITLAB_CI), use dune.sh for cache metrics reporting.
# For local development, use dune directly.
# More information about GITLAB_CI detection available at
# https://docs.gitlab.com/ee/ci/variables/predefined_variables.html
	@DUNE=$${GITLAB_CI:+./scripts/ci/dune.sh}; \
	$${DUNE:-dune} build --profile=$(PROFILE) $(DUNE_BUILD_JOBS) $(COVERAGE_OPTIONS) \
		$(foreach b, $(OCTEZ_EXECUTABLES), _build/install/default/bin/${b}) \
		$(BUILD_EXTRA) \
		@copy-parameters
	@mkdir -p $(OCTEZ_BIN_DIR)/
	@cp -f $(foreach b, $(OCTEZ_EXECUTABLES), _build/install/default/bin/${b}) $(OCTEZ_BIN_DIR)/

# List protocols, i.e. directories proto_* in src with a TEZOS_PROTOCOL file.
TEZOS_PROTOCOL_FILES=$(wildcard src/proto_*/lib_protocol/TEZOS_PROTOCOL)
PROTOCOLS=$(patsubst %/lib_protocol/TEZOS_PROTOCOL,%,${TEZOS_PROTOCOL_FILES})

.PHONY: all.pkg
all.pkg:
	@dune build --profile=$(PROFILE) \
	    $(patsubst %.opam,%.install, $(shell find src vendors -name \*.opam -print))

$(addsuffix .pkg,${PACKAGES_SUBPROJECT}): %.pkg:
	@dune build --profile=$(PROFILE) \
	    $(patsubst %.opam,%.install, $(shell find src vendors -name $*.opam -print))

$(addsuffix .pkg,${PACKAGES}): %.pkg:
	dune build --profile=$(PROFILE) $(patsubst %.opam,%.install,$*.opam)

$(addsuffix .test,${PACKAGES_SUBPROJECT}): %.test:
	@dune build --profile=$(PROFILE) \
	    @$(patsubst %/$*.opam,%,$(shell find src vendors -name $*.opam))/runtest

$(addsuffix .test,${PACKAGES}): %.test:
	@echo "'make $*.test' is no longer supported"

.PHONY: coverage-report
coverage-report:
	@bisect-ppx-report html --tree --ignore-missing-files -o ${COVERAGE_REPORT} --coverage-path ${COVERAGE_OUTPUT}
	@echo "Report should be available in file://$(shell pwd)/${COVERAGE_REPORT}/index.html"

.PHONY: coverage-report-summary
coverage-report-summary:
	@bisect-ppx-report summary --coverage-path ${COVERAGE_OUTPUT}

.PHONY: coverage-report-cobertura
coverage-report-cobertura:
	@bisect-ppx-report cobertura --ignore-missing-file --coverage-path ${COVERAGE_OUTPUT} ${COBERTURA_REPORT}
	@echo "Cobertura report should be available in ${COBERTURA_REPORT}"

.PHONY: enable-time-measurement
enable-time-measurement:
	@$(MAKE) all DUNE_INSTRUMENT_WITH=tezos-time-measurement

.PHONY: test-protocol-compile
test-protocol-compile:
	@dune build --profile=$(PROFILE) $(COVERAGE_OPTIONS) @runtest_compile_protocol
	@dune build --profile=$(PROFILE) $(COVERAGE_OPTIONS) @runtest_out_of_opam

PROTO_DIRS := $(shell find src/ -maxdepth 1 -type d -path "src/proto_*" 2>/dev/null | LC_COLLATE=C sort)
NONPROTO_DIRS := $(shell find src/ -maxdepth 1 -mindepth 1 -type d -not -path "src/proto_*" 2>/dev/null | LC_COLLATE=C sort)
ETHERLINK_DIRS := etherlink/bin_node/test

OTHER_DIRS := $(shell find contrib/ ci/ client-libs/ -maxdepth 1 -mindepth 1 -type d 2>/dev/null | LC_COLLATE=C sort)

.PHONY: test-proto-unit
test-proto-unit:
	DUNE_PROFILE=$(PROFILE) \
		COVERAGE_OPTIONS="$(COVERAGE_OPTIONS)" \
		scripts/test_wrapper.sh test-proto-unit \
		$(addprefix @, $(addsuffix /runtest,$(PROTO_DIRS)))

.PHONY: test-lib-store-slow
test-lib-store-slow:
	DUNE_PROFILE=$(PROFILE) \
		COVERAGE_OPTIONS="$(COVERAGE_OPTIONS)" \
		dune exec src/lib_store/unix/test/slow/test_slow.exe -- --file test_slow.ml --info

.PHONY: test-lib-store-bench
test-lib-store-bench:
	DUNE_PROFILE=$(PROFILE) \
		COVERAGE_OPTIONS="$(COVERAGE_OPTIONS)" \
		dune exec src/lib_store/unix/test/bench/bench.exe -- --file bench.ml --info

.PHONY: test-nonproto-unit
test-nonproto-unit:
	DUNE_PROFILE=$(PROFILE) \
		COVERAGE_OPTIONS="$(COVERAGE_OPTIONS)" \
		scripts/test_wrapper.sh test-nonproto-unit \
		$(addprefix @, $(addsuffix /runtest,$(NONPROTO_DIRS)))

.PHONY: test-etherlink-unit
test-etherlink-unit:
	DUNE_PROFILE=$(PROFILE) \
		COVERAGE_OPTIONS="$(COVERAGE_OPTIONS)" \
		scripts/test_wrapper.sh test-etherlink-unit \
		$(addprefix @, $(addsuffix /runtest,$(ETHERLINK_DIRS)))

.PHONY: test-other-unit
test-other-unit:
	DUNE_PROFILE=$(PROFILE) \
		COVERAGE_OPTIONS="$(COVERAGE_OPTIONS)" \
		scripts/test_wrapper.sh test-other-unit \
		$(addprefix @, $(addsuffix /runtest,$(OTHER_DIRS)))

.PHONY: test-unit
test-unit: test-nonproto-unit test-proto-unit test-other-unit

.PHONY: test-unit-alpha
test-unit-alpha:
	@dune build --profile=$(PROFILE) @src/proto_alpha/lib_protocol/runtest

.PHONY: build-tezt
build-tezt:
	@dune build tezt

.PHONY: build-simulation-scenario
build-simulation-scenario:
	@dune build devtools/testnet_experiment_tools/
	@mkdir -p $(OCTEZ_BIN_DIR)/
	@cp -f _build/default/devtools/testnet_experiment_tools/simulation_scenario.exe $(OCTEZ_BIN_DIR)/simulation-scenario
	@cp -f _build/default/devtools/testnet_experiment_tools/safety_checker.exe $(OCTEZ_BIN_DIR)/safety-checker

.PHONY: test-tezt
test-tezt: build-additional-tezt-test-dependency-executables
	@dune exec --profile=$(PROFILE) $(COVERAGE_OPTIONS) tezt/tests/main.exe

.PHONY: test-tezt-i
test-tezt-i: build-additional-tezt-test-dependency-executables
	@dune exec --profile=$(PROFILE) $(COVERAGE_OPTIONS) tezt/tests/main.exe -- --info

.PHONY: test-tezt-c
test-tezt-c: build-additional-tezt-test-dependency-executables
	@dune exec --profile=$(PROFILE) $(COVERAGE_OPTIONS) tezt/tests/main.exe -- --commands

.PHONY: test-tezt-v
test-tezt-v: build-additional-tezt-test-dependency-executables
	@dune exec --profile=$(PROFILE) $(COVERAGE_OPTIONS) tezt/tests/main.exe -- --verbose

.PHONY: test-tezt-coverage
test-tezt-coverage: build-additional-tezt-test-dependency-executables
	@dune exec --profile=$(PROFILE) $(COVERAGE_OPTIONS) tezt/tests/main.exe -- --keep-going --test-timeout 1800

.PHONY: test-code
test-code: test-protocol-compile test-unit test-tezt

# This is as `make test-code` except we allow failure (prefix "-")
# because we still want the coverage report even if an individual
# test happens to fail.
.PHONY: test-coverage
test-coverage:
	-@$(MAKE) test-protocol-compile
	-@$(MAKE) test-unit
	-@$(MAKE) test-tezt

.PHONY: test-coverage-tenderbake
test-coverage-tenderbake:
	-@$(MAKE) test-unit-alpha

.PHONY: test-webassembly
test-webassembly:
	@dune build --profile=$(PROFILE) @src/lib_webassembly/bin/runtest-python

.PHONY: lint-opam-dune
lint-opam-dune:
	@dune build --profile=$(PROFILE) @runtest_dune_template

# Ensure that all unit tests are restricted to their opam package
# (change 'octez-distributed-internal' to one the most elementary packages of
# the repo if you add "internal" dependencies to octez-distributed-internal)
.PHONY: lint-tests-pkg
lint-tests-pkg:
	@(dune build -p octez-distributed-internal @runtest) || \
	{ echo "You have probably defined some tests in dune files without specifying to which 'package' they belong."; exit 1; }


TEST_DIRS := $(shell find src -name "test" -type d -print -o -name "test-*" -type d -print)
EXCLUDE_TEST_DIRS := $(addprefix --exclude-file ,$(addsuffix /,${TEST_DIRS}))

.PHONY: test
test: test-code

.PHONY: check-linting check-python-linting check-ocaml-linting check-opam-linting

check-linting:
	@scripts/lint.sh --check-scripts
	@scripts/lint.sh --check-ocamlformat
	@scripts/lint.sh --check-rust-toolchain
	@dune build --profile=$(PROFILE) @fmt

check-python-linting:
	@$(MAKE) -C docs lint

check-python-typecheck:
	@$(MAKE) -C docs typecheck

check-ocaml-linting:
	@./scripts/semgrep/lint-all-ocaml-sources.sh

check-opam-linting:
	@find . ! -path "./_opam/*" -name "*.opam" -exec opam lint {} +

.PHONY: fmt fmt-ocaml fmt-python
fmt: fmt-ocaml fmt-python fmt-shell

fmt-shell:
	scripts/lint.sh --format-shell

fmt-ocaml:
	@dune build --profile=$(PROFILE) @fmt --auto-promote

fmt-python:
	@$(MAKE) -C docs fmt

.PHONY: dpkg
dpkg: all
	export TIMESTAMP=$$(date '+%Y%m%d%H%M') ; \
	export CI_COMMIT_SHORT_SHA=$$(git rev-parse --short HEAD) ; \
	export CI_COMMIT_REF_NAME=$$(git rev-parse --abbrev-ref HEAD) ; \
	export CI_COMMIT_TAG=$$(git describe --exact-match --tags 2> /dev/null || git rev-parse --short HEAD) ; \
	./scripts/dpkg/make_dpkg.sh scripts/dpkg/B && \
	./scripts/dpkg/make_dpkg.sh scripts/dpkg/A

.PHONY: rpm
rpm: all
	export TIMESTAMP=$$(date '+%Y%m%d%H%M') ; \
	export CI_COMMIT_SHORT_SHA=$$(git rev-parse --short HEAD) ; \
	export CI_COMMIT_REF_NAME=$$(git rev-parse --abbrev-ref HEAD) ; \
	export CI_COMMIT_TAG=$$(git describe --exact-match --tags 2> /dev/null || git rev-parse --short HEAD) ; \
  ./scripts/rpm/make_rpm.sh

.PHONY: build-deps
build-deps:
	@./scripts/install_build_deps.sh

.PHONY: build-dev-deps
build-dev-deps:
	@./scripts/install_build_deps.sh --dev

.PHONY: lift-protocol-limits-patch
lift-protocol-limits-patch:
	@git apply -R ./src/bin_tps_evaluation/lift_limits.patch || true
	@git apply ./src/bin_tps_evaluation/lift_limits.patch

.PHONY: build-tps-deps
build-tps-deps:
	@./scripts/install_build_deps.sh --tps

.PHONY: build-tps
build-tps: lift-protocol-limits-patch all build-tezt
	@dune build ./src/bin_tps_evaluation
	@cp -f ./_build/default/src/bin_tps_evaluation/main_tps_evaluation.exe tezos-tps-evaluation
	@cp -f ./src/bin_tps_evaluation/tezos-tps-evaluation-benchmark-tps .
	@cp -f ./src/bin_tps_evaluation/tezos-tps-evaluation-estimate-average-block .
	@cp -f ./src/bin_tps_evaluation/tezos-tps-evaluation-gas-tps .

.PHONY: build-octogram
build-octogram: all
	@dune build ./src/bin_octogram
	@cp -f ./_build/default/src/bin_octogram/octogram_main.exe octogram

.PHONY: build-floodgate
build-floodgate:
	@dune build ./etherlink/bin_floodgate
	@cp -f ./_build/default/etherlink/bin_floodgate/main.exe floodgate

.PHONY: build-courier
build-courier:
	@dune build ./etherlink/bin_courier
	@cp -f ./_build/default/etherlink/bin_courier/main.exe courier

.PHONY: etherlink-outbox-monitor
etherlink-outbox-monitor:
	@dune build ./etherlink/bin_outbox_monitor
	@cp -f ./_build/default/etherlink/bin_outbox_monitor/main.exe $@

.PHONY: build-unreleased
build-unreleased: all
	@echo 'Note: "make build-unreleased" is deprecated. Just use "make".'

.PHONY: docker-image-build
docker-image-build:
	# Setting '--variants ""' creates only the build image.
	@./scripts/create_docker_image.sh --variants ""

.PHONY: docker-image-debug
docker-image-debug:
	@./scripts/create_docker_image.sh --variants "debug"

.PHONY: docker-image-bare
docker-image-bare:
	@./scripts/create_docker_image.sh --variants "bare"

.PHONY: docker-image-minimal
docker-image-minimal:
	@./scripts/create_docker_image.sh --variants "minimal"

.PHONY: docker-image
docker-image:
	@./scripts/create_docker_image.sh

.PHONY: install
install:
	@dune build --profile=$(PROFILE) @install
	@dune install

.PHONY: uninstall
uninstall:
	@dune uninstall

.PHONY: coverage-clean
coverage-clean:
	@-rm -Rf ${COVERAGE_OUTPUT}/*.coverage ${COVERAGE_REPORT}

.PHONY: pkg-common-clean
pkg-common-clean:
	@-rm -rf scripts/pkg-common/{baker,client,smartrollup}-binaries

.PHONY: dpkg-clean
dpkg-clean: pkg-common-clean
	@-rm -rf _dpkgstage *.deb

.PHONY: rpm-clean
rpm-clean: pkg-common-clean
	@-rm -rf _rpmbuild *.rpm

.PHONY: clean
clean: coverage-clean clean-old-names dpkg-clean rpm-clean
	@-dune clean
	@-rm -f ${ALL_EXECUTABLES}
	@-${MAKE} -C docs clean
	@-rm -f docs/api/tezos-{baker,endorser,accuser}-alpha.html docs/api/tezos-{admin-,}client.html docs/api/tezos-signer.html

.PHONY: build-kernels-deps
build-kernels-deps:
	$(MAKE) -f kernels.mk build-deps
	$(MAKE) -f etherlink.mk build-deps
	$(MAKE) -C src/riscv build-deps

.PHONY: build-kernels-dev-deps
build-kernels-dev-deps:
	$(MAKE) -f kernels.mk build-dev-deps
	$(MAKE) -f etherlink.mk build-dev-deps

.PHONY: build-kernels
build-kernels:
	$(MAKE) -f kernels.mk build
	$(MAKE) -f etherlink.mk build
	$(MAKE) -C src/riscv build

.PHONY: check-kernels
check-kernels:
	$(MAKE) -f kernels.mk check
	$(MAKE) -f etherlink.mk check
	$(MAKE) -C src/riscv check

.PHONY: test-kernels
test-kernels:
	$(MAKE) -f kernels.mk test
	$(MAKE) -f etherlink.mk test
	$(MAKE) -C src/riscv test

.PHONY: clean-kernels
clean-kernels:
	$(MAKE) -f kernels.mk clean
	$(MAKE) -f etherlink.mk clean
	$(MAKE) -C src/riscv clean

.PHONY: wasm_runtime_gen_files
wasm_runtime_gen_files::
	@echo 'Building etherlink/lib_wasm_runtime'
	@cd etherlink/lib_wasm_runtime; cargo build

octez-evm-node: wasm_runtime_gen_files

evm-node-static:
	@$(MAKE) PROFILE=static build OCTEZ_EXECUTABLES?="$(EVM_EXECUTABLES)"
