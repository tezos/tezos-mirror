PACKAGES:=$(patsubst %.opam,%,$(notdir $(shell find src vendors -name \*.opam -print)))

active_protocol_versions := $(shell cat active_protocol_versions)

define directory_of_version
src/proto_$(shell echo $1 | tr -- - _)
endef

current_opam_version := $(shell opam --version)
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
MERLIN_INSTALLED := $(shell opam list merlin --installed --silent 2> /dev/null; echo $$?)

TEZOS_BIN=tezos-node tezos-validator tezos-client tezos-admin-client tezos-signer tezos-codec tezos-protocol-compiler tezos-snoop tezos-proxy-server \
    $(foreach p, $(active_protocol_versions), tezos-baker-$(p)) \
    $(foreach p, $(active_protocol_versions), tezos-accuser-$(p)) \
    $(foreach p, $(active_protocol_versions), \
		  $(shell if [ -f $(call directory_of_version,$p)/bin_endorser/dune ]; then \
		             echo tezos-endorser-$(p); fi))

ifeq ($(filter ${opam_version}.%,${current_opam_version}),)
$(error Unexpected opam version (found: ${current_opam_version}, expected: ${opam_version}.*))
endif

current_ocaml_version := $(shell opam exec -- ocamlc -version)

.PHONY: all
all:
	@$(MAKE) build PROFILE=dev

.PHONY: release
release:
	@$(MAKE) build PROFILE=release

.PHONY: build-parameters
build-parameters:
	@dune build $(COVERAGE_OPTIONS) --profile=$(PROFILE) @copy-parameters

build: generate_dune
ifneq (${current_ocaml_version},${ocaml_version})
	$(error Unexpected ocaml version (found: ${current_ocaml_version}, expected: ${ocaml_version}))
endif
	dune build $(COVERAGE_OPTIONS) --profile=$(PROFILE) \
		$(foreach b, $(TEZOS_BIN), _build/install/default/bin/${b}) \
		@copy-parameters
	cp -f $(foreach b, $(TEZOS_BIN), _build/install/default/bin/${b}) ./
ifeq ($(MERLIN_INSTALLED),0) # only build tooling support if merlin is installed
	@dune build @check
endif

# List protocols, i.e. directories proto_* in src with a TEZOS_PROTOCOL file.
TEZOS_PROTOCOL_FILES=$(wildcard src/proto_*/lib_protocol/TEZOS_PROTOCOL)
PROTOCOLS=$(patsubst %/lib_protocol/TEZOS_PROTOCOL,%,${TEZOS_PROTOCOL_FILES})

DUNE_INCS=$(patsubst %,%/lib_protocol/dune.inc, ${PROTOCOLS})

.PHONY: generate_dune
generate_dune: ${DUNE_INCS}

${DUNE_INCS}:: src/proto_%/lib_protocol/dune.inc: \
  src/proto_%/lib_protocol/TEZOS_PROTOCOL
	dune build @$(dir $@)/runtest_dune_template --auto-promote
	touch $@

.PHONY: all.pkg
all.pkg: generate_dune
	@dune build \
	    $(patsubst %.opam,%.install, $(shell find src vendors -name \*.opam -print))

$(addsuffix .pkg,${PACKAGES}): %.pkg:
	@dune build \
	    $(patsubst %.opam,%.install, $(shell find src vendors -name $*.opam -print))

$(addsuffix .test,${PACKAGES}): %.test:
	@dune build \
	    @$(patsubst %/$*.opam,%,$(shell find src vendors -name $*.opam))/runtest

.PHONY: coverage-report
coverage-report:
# /!\ FIXME: https://gitlab.com/tezos/tezos/-/issues/1945
# corrupted files will be better handled in the next bisect_ppx version
# and this check will be removed.
	@find ${COVERAGE_OUTPUT} -size 0 -type f -delete
	@bisect-ppx-report html -o ${COVERAGE_REPORT} --coverage-path ${COVERAGE_OUTPUT}
	@echo "Report should be available in ${COVERAGE_REPORT}/index.html"

.PHONY: coverage-report-summary
coverage-report-summary:
	@bisect-ppx-report summary --coverage-path ${COVERAGE_OUTPUT}

CORRUPTED_FILES_FOUND := $(shell find ${COVERAGE_OUTPUT} -size 0 -type f | wc -l)

.PHONY: coverage-report-cobertura
coverage-report-cobertura:
# /!\ FIXME: https://gitlab.com/tezos/tezos/-/issues/1945
# corrupted files will be better handled in the next bisect_ppx version
# and this check will be removed.
ifneq ($(CORRUPTED_FILES_FOUND), 0)
	@echo "Corrupted .coverage files found, removing them:"
	@find ${COVERAGE_OUTPUT} -size 0 -type f -print -delete
endif
	@bisect-ppx-report cobertura --ignore-missing-file --coverage-path ${COVERAGE_OUTPUT} ${COBERTURA_REPORT}
	@echo "Cobertura report should be available in ${COBERTURA_REPORT}"

.PHONY: enable-time-measurement
enable-time-measurement:
	@$(MAKE) build PROFILE=dev DUNE_INSTRUMENT_WITH=tezos-time-measurement

.PHONY: build-sandbox
build-sandbox:
	@dune build src/bin_sandbox/main.exe
	@cp -f _build/default/src/bin_sandbox/main.exe tezos-sandbox

.PHONY: build-test
build-test: build-sandbox
	@dune build @buildtest

.PHONY: test-protocol-compile
test-protocol-compile:
	@dune build $(COVERAGE_OPTIONS) @runtest_compile_protocol
	@dune build $(COVERAGE_OPTIONS) @runtest_out_of_opam

PROTO_LIBS := $(shell find src/ -path src/proto_\* -name test -type d -exec test -f \{\}/dune \; -print 2>/dev/null | LC_COLLATE=C sort)
PROTO_LIBS_NAMES := $(patsubst %/test,%,$(PROTO_LIBS))
PROTO_TARGETS := $(addsuffix .test_proto,${PROTO_LIBS_NAMES})

$(PROTO_TARGETS): %.test_proto:
	scripts/test_wrapper.sh $* $(subst /,_,$(patsubst src/proto_%,%,$*)) $(COVERAGE_OPTIONS)

.PHONY: test-proto-unit
test-proto-unit: $(PROTO_TARGETS)

# We do not run vendor tests because they are a no-op from dune
NONPROTO_LIBS := $(shell find src/ -path src/proto_\* -prune -o -name test -type d -exec test -f \{\}/dune \; -print | LC_COLLATE=C sort)
NONPROTO_LIBS_NAMES := $(patsubst %/test,%,$(NONPROTO_LIBS))
NONPROTO_TARGETS := $(addsuffix .test_nonproto,${NONPROTO_LIBS_NAMES})

$(NONPROTO_TARGETS): %.test_nonproto:
	scripts/test_wrapper.sh $* $(subst /,_,$(patsubst src/lib_%,%,$(patsubst src/bin_%,%,$*))) $(COVERAGE_OPTIONS)

.PHONY: test-nonproto-unit
test-nonproto-unit: $(NONPROTO_TARGETS)

.PHONY: test-unit
test-unit: test-nonproto-unit test-proto-unit

.PHONY: test-unit-alpha
test-unit-alpha:
	@dune build @src/proto_alpha/lib_protocol/runtest

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

.PHONY: test-js
test-js:
	@dune build @runtest_js
	@dune exec ./src/tooling/run_js_inline_tests.exe

.PHONY: test-tezt test-tezt-i test-tezt-c test-tezt-v
test-tezt:
	@dune exec $(COVERAGE_OPTIONS) tezt/tests/main.exe
test-tezt-i:
	@dune exec $(COVERAGE_OPTIONS) tezt/tests/main.exe -- --info
test-tezt-c:
	@dune exec $(COVERAGE_OPTIONS) tezt/tests/main.exe -- --commands
test-tezt-v:
	@dune exec $(COVERAGE_OPTIONS) tezt/tests/main.exe -- --verbose

.PHONY: test-tezt-coverage
test-tezt-coverage:
	@dune exec $(COVERAGE_OPTIONS) tezt/tests/main.exe -- --keep-going --test-timeout 1800

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

.PHONY: lint-opam-dune
lint-opam-dune:
	@dune build @runtest_dune_template


# Ensure that all unit tests are restricted to their opam package
# (change 'tezos-test-helpers' to one the most elementary packages of
# the repo if you add "internal" dependencies to tezos-test-helpers)
.PHONY: lint-tests-pkg
lint-tests-pkg:
	@(dune build -p tezos-test-helpers @runtest @runtest_js) || \
	{ echo "You have probably defined some tests in dune files without specifying to which 'package' they belong."; exit 1; }


.PHONY: test
test: lint-opam-dune test-code

.PHONY: check-linting check-python-linting

check-linting:
	@src/tooling/lint.sh --check-scripts
	@src/tooling/lint.sh --check-ocamlformat
	@dune build @fmt

check-python-linting:
	@$(MAKE) -C tests_python lint
	@$(MAKE) -C docs lint

.PHONY: fmt fmt-ocaml fmt-python
fmt: fmt-ocaml fmt-python

fmt-ocaml:
	@dune build @fmt --auto-promote

fmt-python:
	@$(MAKE) -C tests_python fmt

.PHONY: build-deps
build-deps:
	@./scripts/install_build_deps.sh

.PHONY: build-dev-deps
build-dev-deps:
	@./scripts/install_build_deps.sh --dev

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
	@dune build @install
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
	@-rm -f ${TEZOS_BIN} tezos-sandbox
	@-${MAKE} -C docs clean
	@-${MAKE} -C tests_python clean
	@-rm -f docs/api/tezos-{baker,endorser,accuser}-alpha.html docs/api/tezos-{admin-,}client.html docs/api/tezos-signer.html
