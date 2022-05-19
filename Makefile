PACKAGES_SUBPROJECT:=$(patsubst %.opam,%,$(notdir $(shell find src vendors -name \*.opam -print)))
PACKAGES:=$(patsubst %.opam,%,$(notdir $(shell find opam -name \*.opam -print)))

active_protocol_versions := $(shell cat active_protocol_versions)
tx_rollup_protocol_versions := $(shell cat tx_rollup_protocol_versions)
sc_rollup_protocol_versions := $(shell cat sc_rollup_protocol_versions)

define directory_of_version
src/proto_$(shell echo $1 | tr -- - _)
endef

# Opam is not present in some build environments. We don't strictly need it.
# Those environments set TEZOS_WITHOUT_OPAM.
ifndef TEZOS_WITHOUT_OPAM
current_opam_version := $(shell opam --version)
endif

include scripts/version.sh

DOCKER_IMAGE_NAME := tezos
DOCKER_IMAGE_VERSION := latest
DOCKER_BUILD_IMAGE_NAME := $(DOCKER_IMAGE_NAME)_build
DOCKER_BUILD_IMAGE_VERSION := latest
DOCKER_BARE_IMAGE_NAME := $(DOCKER_IMAGE_NAME)-bare
DOCKER_BARE_IMAGE_VERSION := latest
DOCKER_DEBUG_IMAGE_NAME := $(DOCKER_IMAGE_NAME)-debug
DOCKER_DEBUG_IMAGE_VERSION := latest
DOCKER_DEPS_IMAGE_NAME := registry.gitlab.com/tezos/opam-repository
DOCKER_DEPS_IMAGE_VERSION := runtime-build-dependencies--${opam_repository_tag}
DOCKER_DEPS_MINIMAL_IMAGE_VERSION := runtime-dependencies--${opam_repository_tag}
COVERAGE_REPORT := _coverage_report
COBERTURA_REPORT := _coverage_report/cobertura.xml
CODE_QUALITY_REPORT := _reports/gl-code-quality-report.json
PROFILE?=dev
VALID_PROFILES=dev release static

TEZOS_BIN=tezos-node tezos-validator tezos-client tezos-admin-client tezos-signer tezos-codec tezos-protocol-compiler tezos-snoop tezos-proxy-server \
    $(foreach p, $(active_protocol_versions), tezos-baker-$(p)) \
    $(foreach p, $(active_protocol_versions), tezos-accuser-$(p)) \
    $(foreach p, $(active_protocol_versions), \
		  $(shell if [ -f $(call directory_of_version,$p)/bin_endorser/dune ]; then \
		             echo tezos-endorser-$(p); fi)) \

UNRELEASED_TEZOS_BIN=$(foreach p, $(tx_rollup_protocol_versions), tezos-tx-rollup-node-$p) \
   $(foreach p, $(tx_rollup_protocol_versions), tezos-tx-rollup-client-$p) \
   $(foreach p, $(sc_rollup_protocol_versions), tezos-sc-rollup-node-$p) \
   $(foreach p, $(sc_rollup_protocol_versions), tezos-sc-rollup-client-$p)

# See first mention of TEZOS_WITHOUT_OPAM.
ifndef TEZOS_WITHOUT_OPAM
ifeq ($(filter ${opam_version}.%,${current_opam_version}),)
$(error Unexpected opam version (found: ${current_opam_version}, expected: ${opam_version}.*))
endif
endif

ifeq ($(filter ${VALID_PROFILES},${PROFILE}),)
$(error Unexpected dune profile (got: ${PROFILE}, expecting one of: ${VALID_PROFILES}))
endif

# See first mention of TEZOS_WITHOUT_OPAM.
ifdef TEZOS_WITHOUT_OPAM
current_ocaml_version := $(shell ocamlc -version)
else
current_ocaml_version := $(shell opam exec -- ocamlc -version)
endif

.PHONY: all
all:
	@$(MAKE) build

.PHONY: release
release:
	@$(MAKE) build PROFILE=release

.PHONY: build-parameters
build-parameters:
	@dune build --profile=$(PROFILE) $(COVERAGE_OPTIONS) @copy-parameters

.PHONY: $(TEZOS_BIN)
$(TEZOS_BIN):
	dune build $(COVERAGE_OPTIONS) --profile=$(PROFILE) _build/install/default/bin/$@
	cp -f _build/install/default/bin/$@ ./

.PHONY: $(UNRELEASED_TEZOS_BIN)
$(UNRELEASED_TEZOS_BIN):
	@dune build $(COVERAGE_OPTIONS) --profile=$(PROFILE) _build/install/default/bin/$@
	@cp -f _build/install/default/bin/$@ ./

.PHONY: build
build:
ifneq (${current_ocaml_version},${ocaml_version})
	$(error Unexpected ocaml version (found: ${current_ocaml_version}, expected: ${ocaml_version}))
endif
	@dune build --profile=$(PROFILE) $(COVERAGE_OPTIONS) \
		$(foreach b, $(TEZOS_BIN), _build/install/default/bin/${b}) \
		@copy-parameters
	@cp -f $(foreach b, $(TEZOS_BIN), _build/install/default/bin/${b}) ./

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
	@bisect-ppx-report html --ignore-missing-files -o ${COVERAGE_REPORT} --coverage-path ${COVERAGE_OUTPUT}
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
	@$(MAKE) build PROFILE=dev DUNE_INSTRUMENT_WITH=tezos-time-measurement

.PHONY: build-sandbox
build-sandbox:
	@dune build --profile=$(PROFILE) $(COVERAGE_OPTIONS) src/bin_sandbox/main.exe
	@cp -f _build/default/src/bin_sandbox/main.exe tezos-sandbox

.PHONY: test-protocol-compile
test-protocol-compile:
	@dune build --profile=$(PROFILE) $(COVERAGE_OPTIONS) @runtest_compile_protocol
	@dune build --profile=$(PROFILE) $(COVERAGE_OPTIONS) @runtest_out_of_opam

PROTO_LIBS := $(shell find src/ -path src/proto_\* -name test -type d 2>/dev/null | LC_COLLATE=C sort)

.PHONY: test-proto-unit
test-proto-unit:
	DUNE_PROFILE=$(PROFILE) \
		COVERAGE_OPTIONS="$(COVERAGE_OPTIONS)" \
		scripts/test_wrapper.sh test-proto-unit \
		$(addprefix @, $(addsuffix /runtest,$(PROTO_LIBS)))


# We do not run vendor tests because they are a no-op from dune
NONPROTO_LIBS := $(shell find src/ -path src/proto_\* -prune -o -name test -type d -exec test -f \{\}/dune \; -print | LC_COLLATE=C sort)

.PHONY: test-nonproto-unit
test-nonproto-unit:
	DUNE_PROFILE=$(PROFILE) \
		COVERAGE_OPTIONS="$(COVERAGE_OPTIONS)" \
		scripts/test_wrapper.sh test-nonproto-unit \
		$(addprefix @, $(addsuffix /runtest,$(NONPROTO_LIBS)))

.PHONY: test-unit
test-unit: test-nonproto-unit test-proto-unit

.PHONY: test-unit-alpha
test-unit-alpha:
	@dune build --profile=$(PROFILE) @src/proto_alpha/lib_protocol/runtest

.PHONY: test-python
test-python: all
	@$(MAKE) -C tests_python all

.PHONY: test-python-alpha
test-python-alpha: all
	@$(MAKE) -C tests_python alpha

.PHONY: test-python-tenderbake
test-python-tenderbake: all
	@$(MAKE) -C tests_python tenderbake

.PHONY: test-flextesa
test-flextesa:
	@$(MAKE) -f sandbox.Makefile

# TODO: https://gitlab.com/tezos/tezos/-/issues/3018
# Disable verbose once the log file bug in Alcotest is fixed.
.PHONY: test-js
test-js:
	@ALCOTEST_VERBOSE=yes dune build @runtest_js
	@dune exec ./src/tooling/run_js_inline_tests.exe

.PHONY: build-tezt
build-tezt:
	@dune build tezt

.PHONY: test-tezt test-tezt-i test-tezt-c test-tezt-v
test-tezt:
	@dune exec --profile=$(PROFILE) $(COVERAGE_OPTIONS) tezt/tests/main.exe
test-tezt-i:
	@dune exec --profile=$(PROFILE) $(COVERAGE_OPTIONS) tezt/tests/main.exe -- --info
test-tezt-c:
	@dune exec --profile=$(PROFILE) $(COVERAGE_OPTIONS) tezt/tests/main.exe -- --commands
test-tezt-v:
	@dune exec --profile=$(PROFILE) $(COVERAGE_OPTIONS) tezt/tests/main.exe -- --verbose

.PHONY: test-tezt-coverage
test-tezt-coverage:
	@dune exec --profile=$(PROFILE) $(COVERAGE_OPTIONS) tezt/tests/main.exe -- --keep-going --test-timeout 1800

.PHONY: test-code
test-code: test-protocol-compile test-unit test-flextesa test-python test-tezt

# This is `make test-code` except for flextesa (which doesn't
# play well with coverage). We allow failure (prefix "-") because we still want
# the coverage report even if an individual test happens to fail.
.PHONY: test-coverage
test-coverage:
	-@$(MAKE) test-protocol-compile
	-@$(MAKE) test-unit
	-@$(MAKE) test-python
	-@$(MAKE) test-tezt

.PHONY: test-coverage-tenderbake
test-coverage-tenderbake:
	-@$(MAKE) test-unit-alpha
	-@$(MAKE) test-python-tenderbake

# Ensure that all unit tests are restricted to their opam package
# (change 'tezos-test-helpers' to one the most elementary packages of
# the repo if you add "internal" dependencies to tezos-test-helpers)
.PHONY: lint-tests-pkg
lint-tests-pkg:
	@(dune build -p tezos-test-helpers @runtest @runtest_js) || \
	{ echo "You have probably defined some tests in dune files without specifying to which 'package' they belong."; exit 1; }



NONPROTO_LIBS_DIR := $(addsuffix /,${NONPROTO_LIBS})
EXCLUDE_NONPROTO_LIBS_DIR := $(addprefix --exclude-file ,${NONPROTO_LIBS_DIR})
PROTO_LIBS_DIR = $(addsuffix /,${PROTO_LIBS})
EXCLUDE_PROTO_LIBS_DIR := $(addprefix --exclude-file ,${PROTO_LIBS_DIR})

.PHONY: lint-ometrics
lint-ometrics:
	@echo "Running ometrics analysis in your changes."
	@ometrics check ${EXCLUDE_NONPROTO_LIBS_DIR} ${EXCLUDE_PROTO_LIBS_DIR} \
        --exclude-file "src/proto_alpha/lib_protocol/alpha_context.mli" \
        --exclude-file "src/proto_alpha/lib_protocol/alpha_context.ml" \
        --exclude-file "tezt/tests/" \
        --exclude-entry-re "pp\|pp_.+" \
        --exclude-entry-re "encoding\|encoding_.+\|.+_encoding" \
        --exclude-entry-re "compare\|compare_.+\|.+_compare"

.PHONY: lint-ometrics-gitlab
lint-ometrics-gitlab:
	@echo "Running ometrics analysis in your changes."
	@mkdir -p _reports
	@ometrics check-clone ${OMETRICS_GIT} --branch ${OMETRICS_BRANCH} \
        ${EXCLUDE_NONPROTO_LIBS_DIR} ${EXCLUDE_PROTO_LIBS_DIR} \
        --exclude-file "src/proto_alpha/lib_protocol/alpha_context.mli" \
        --exclude-file "src/proto_alpha/lib_protocol/alpha_context.ml" \
        --exclude-file "tezt/tests/" \
        --exclude-entry-re "pp\|pp_.+" \
        --exclude-entry-re "encoding\|encoding_.+\|.+_encoding" \
        --exclude-entry-re "compare\|compare_.+\|.+_compare" \
        --gitlab --output ${CODE_QUALITY_REPORT}
	@echo "Report should be available in file://$(shell pwd)/${CODE_QUALITY_REPORT}"

.PHONY: test
test: test-code

.PHONY: check-linting check-python-linting

check-linting:
	@scripts/lint.sh --check-scripts
	@scripts/lint.sh --check-ocamlformat
	@dune build --profile=$(PROFILE) @fmt

check-python-linting:
	@$(MAKE) -C tests_python lint
	@$(MAKE) -C docs lint

check-ocaml-linting:
	@./scripts/semgrep/lint-all-ocaml-sources.sh

.PHONY: fmt fmt-ocaml fmt-python
fmt: fmt-ocaml fmt-python

fmt-ocaml:
	@dune build --profile=$(PROFILE) @fmt --auto-promote

fmt-python:
	@$(MAKE) -C tests_python fmt

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
build-tps: lift-protocol-limits-patch build build-tezt
	@dune build ./src/bin_tps_evaluation
	@cp -f ./_build/install/default/bin/tezos-tps-evaluation .
	@cp -f ./src/bin_tps_evaluation/tezos-tps-evaluation-benchmark-tps .
	@cp -f ./src/bin_tps_evaluation/tezos-tps-evaluation-estimate-average-block .
	@cp -f ./src/bin_tps_evaluation/tezos-tps-evaluation-gas-tps .

.PHONY: build-unreleased
build-unreleased: build $(UNRELEASED_TEZOS_BIN)

.PHONY: docker-image-build
docker-image-build:
	@docker build \
		-t $(DOCKER_BUILD_IMAGE_NAME):$(DOCKER_BUILD_IMAGE_VERSION) \
		-f build.Dockerfile \
		--build-arg BASE_IMAGE=$(DOCKER_DEPS_IMAGE_NAME) \
		--build-arg BASE_IMAGE_VERSION=$(DOCKER_DEPS_IMAGE_VERSION) \
		.

.PHONY: docker-image-debug
docker-image-debug:
	docker build \
		-t $(DOCKER_DEBUG_IMAGE_NAME):$(DOCKER_DEBUG_IMAGE_VERSION) \
		--build-arg BASE_IMAGE=$(DOCKER_DEPS_IMAGE_NAME) \
		--build-arg BASE_IMAGE_VERSION=$(DOCKER_DEPS_MINIMAL_IMAGE_VERSION) \
		--build-arg BUILD_IMAGE=$(DOCKER_BUILD_IMAGE_NAME) \
		--build-arg BUILD_IMAGE_VERSION=$(DOCKER_BUILD_IMAGE_VERSION) \
		--target=debug \
		.

.PHONY: docker-image-bare
docker-image-bare:
	@docker build \
		-t $(DOCKER_BARE_IMAGE_NAME):$(DOCKER_BARE_IMAGE_VERSION) \
		--build-arg=BASE_IMAGE=$(DOCKER_DEPS_IMAGE_NAME) \
		--build-arg=BASE_IMAGE_VERSION=$(DOCKER_DEPS_MINIMAL_IMAGE_VERSION) \
		--build-arg=BASE_IMAGE_VERSION_NON_MIN=$(DOCKER_DEPS_IMAGE_VERSION) \
		--build-arg BUILD_IMAGE=$(DOCKER_BUILD_IMAGE_NAME) \
		--build-arg BUILD_IMAGE_VERSION=$(DOCKER_BUILD_IMAGE_VERSION) \
		--target=bare \
		.

.PHONY: docker-image-minimal
docker-image-minimal:
	@docker build \
		-t $(DOCKER_IMAGE_NAME):$(DOCKER_IMAGE_VERSION) \
		--build-arg=BASE_IMAGE=$(DOCKER_DEPS_IMAGE_NAME) \
		--build-arg=BASE_IMAGE_VERSION=$(DOCKER_DEPS_MINIMAL_IMAGE_VERSION) \
		--build-arg=BASE_IMAGE_VERSION_NON_MIN=$(DOCKER_DEPS_IMAGE_VERSION) \
		--build-arg BUILD_IMAGE=$(DOCKER_BUILD_IMAGE_NAME) \
		--build-arg BUILD_IMAGE_VERSION=$(DOCKER_BUILD_IMAGE_VERSION) \
		.

.PHONY: docker-image
docker-image: docker-image-build docker-image-debug docker-image-bare docker-image-minimal

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

.PHONY: clean
clean: coverage-clean
	@-dune clean
	@-rm -f ${TEZOS_BIN} ${UNRELEASED_TEZOS_BIN} tezos-sandbox
	@-${MAKE} -C docs clean
	@-${MAKE} -C tests_python clean
	@-rm -f docs/api/tezos-{baker,endorser,accuser}-alpha.html docs/api/tezos-{admin-,}client.html docs/api/tezos-signer.html
