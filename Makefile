PACKAGES:=$(patsubst %.opam,%,$(notdir $(shell find src vendors -name \*.opam -print)))

active_protocol_versions := $(shell cat active_protocol_versions)
active_protocol_directories := $(shell tr -- - _ < active_protocol_versions)

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
DOCKER_DEPS_IMAGE_VERSION := ${opam_repository_tag}
DOCKER_DEPS_MINIMAL_IMAGE_VERSION := minimal--${opam_repository_tag}
COVERAGE_REPORT := _coverage_report
MERLIN_INSTALLED := $(shell opam list merlin --installed --silent 2> /dev/null; echo $$?)

ifeq ($(filter ${opam_version}.%,${current_opam_version}),)
	$(error Unexpected opam version (found: ${current_opam_version}, expected: ${opam_version}.*))
endif

current_ocaml_version := $(shell opam exec -- ocamlc -version)

.PHONY: all
all:
	@$(MAKE) build PROFILE=dev

build: generate_dune
ifneq (${current_ocaml_version},${ocaml_version})
	$(error Unexpected ocaml version (found: ${current_ocaml_version}, expected: ${ocaml_version}))
endif
	@dune build $(COVERAGE_OPTIONS) --profile=$(PROFILE) \
		src/bin_node/main.exe \
		src/bin_validation/main_validator.exe \
		src/bin_client/main_client.exe \
		src/bin_client/main_admin.exe \
		src/bin_signer/main_signer.exe \
		src/bin_codec/codec.exe \
		src/lib_protocol_compiler/main_native.exe \
		src/bin_snoop/main_snoop.exe \
		src/bin_proxy_server/main_proxy_server.exe \
		$(foreach p, $(active_protocol_directories), src/proto_$(p)/bin_baker/main_baker_$(p).exe) \
		$(foreach p, $(active_protocol_directories), src/proto_$(p)/bin_endorser/main_endorser_$(p).exe) \
		$(foreach p, $(active_protocol_directories), src/proto_$(p)/bin_accuser/main_accuser_$(p).exe) \
		$(foreach p, $(active_protocol_directories), src/proto_$(p)/lib_parameters/mainnet-parameters.json) \
		$(foreach p, $(active_protocol_directories), src/proto_$(p)/lib_parameters/sandbox-parameters.json) \
		$(foreach p, $(active_protocol_directories), src/proto_$(p)/lib_parameters/test-parameters.json)
	@cp -f _build/default/src/bin_node/main.exe tezos-node
	@cp -f _build/default/src/bin_validation/main_validator.exe tezos-validator
	@cp -f _build/default/src/bin_client/main_client.exe tezos-client
	@cp -f _build/default/src/bin_client/main_admin.exe tezos-admin-client
	@cp -f _build/default/src/bin_signer/main_signer.exe tezos-signer
	@cp -f _build/default/src/bin_codec/codec.exe tezos-codec
	@cp -f _build/default/src/lib_protocol_compiler/main_native.exe tezos-protocol-compiler
	@cp -f _build/default/src/bin_snoop/main_snoop.exe tezos-snoop
	@cp -f _build/default/src/bin_proxy_server/main_proxy_server.exe tezos-proxy-server
	@for p in $(active_protocol_directories) ; do \
	   cp -f _build/default/src/proto_$$p/bin_baker/main_baker_$$p.exe tezos-baker-`echo $$p | tr -- _ -` ; \
	   cp -f _build/default/src/proto_$$p/bin_endorser/main_endorser_$$p.exe tezos-endorser-`echo $$p | tr -- _ -` ; \
	   cp -f _build/default/src/proto_$$p/bin_accuser/main_accuser_$$p.exe tezos-accuser-`echo $$p | tr -- _ -` ; \
	   mkdir -p src/proto_$$p/parameters ; \
	   cp -f _build/default/src/proto_$$p/lib_parameters/sandbox-parameters.json src/proto_$$p/parameters/sandbox-parameters.json ; \
	   cp -f _build/default/src/proto_$$p/lib_parameters/test-parameters.json src/proto_$$p/parameters/test-parameters.json ; \
	   cp -f _build/default/src/proto_$$p/lib_parameters/mainnet-parameters.json src/proto_$$p/parameters/mainnet-parameters.json ; \
	 done
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
	# remove spurious empty output files which prevent bisect from generating
	# the report
	@find ${COVERAGE_OUTPUT} -size 0 -type f -delete
	@bisect-ppx-report html -o ${COVERAGE_REPORT} --coverage-path ${COVERAGE_OUTPUT}
	@echo "Report should be available in ${COVERAGE_REPORT}/index.html"

.PHONY: coverage-report-summary
coverage-report-summary:
	@bisect-ppx-report summary --coverage-path ${COVERAGE_OUTPUT}

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

PROTO_LIBS := $(shell find src -name test -type d 2>/dev/null | grep src/proto_ | LC_COLLATE=C sort)
PROTO_LIBS_NAMES := $(patsubst %/test,%,$(PROTO_LIBS))
PROTO_TARGETS := $(addsuffix .test_proto,${PROTO_LIBS_NAMES})

$(PROTO_TARGETS): %.test_proto:
	scripts/test_wrapper.sh $* $(subst /,_,$(patsubst src/proto_%,%,$*))

.PHONY: test-proto-unit
test-proto-unit: $(PROTO_TARGETS)

# We do not run vendor tests because they are a no-op from dune
NONPROTO_LIBS := $(shell find src/ -path src/proto_\* -prune -o -name test -type d -print | LC_COLLATE=C sort)
NONPROTO_LIBS_NAMES := $(patsubst %/test,%,$(NONPROTO_LIBS))
NONPROTO_TARGETS := $(addsuffix .test_nonproto,${NONPROTO_LIBS_NAMES})

$(NONPROTO_TARGETS): %.test_nonproto:
	scripts/test_wrapper.sh $* $(subst /,_,$(patsubst src/lib_%,%,$(patsubst src/bin_%,%,$*)))

.PHONY: test-nonproto-unit
test-nonproto-unit: $(NONPROTO_TARGETS)

.PHONY: test-unit
test-unit: test-nonproto-unit test-proto-unit

.PHONY: test-python
test-python: all
	@$(MAKE) -C tests_python all

.PHONY: test-flextesa
test-flextesa:
	@$(MAKE) -f sandbox.Makefile

.PHONY: test-tezt test-tezt-i test-tezt-c test-tezt-v
test-tezt:
	@dune exec $(COVERAGE_OPTIONS) tezt/tests/main.exe
test-tezt-i:
	@dune exec $(COVERAGE_OPTIONS) tezt/tests/main.exe -- --info
test-tezt-c:
	@dune exec $(COVERAGE_OPTIONS) tezt/tests/main.exe -- --commands
test-tezt-v:
	@dune exec $(COVERAGE_OPTIONS) tezt/tests/main.exe -- --verbose

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

.PHONY: lint-opam-dune
lint-opam-dune:
	@dune build @runtest_dune_template
	@./scripts/check_opam_test.sh

.PHONY: test
test: lint-opam-dune test-code

.PHONY: check-linting check-python-linting

check-linting:
	@src/tooling/lint.sh --check-scripts
	@src/tooling/lint.sh --check-ocamlformat
	@dune build @fmt

check-python-linting:
	@$(MAKE) -C tests_python lint

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
	@-rm -f \
		tezos-node \
		tezos-validator \
		tezos-client \
		tezos-signer \
		tezos-admin-client \
		tezos-codec \
		tezos-protocol-compiler \
		tezos-snoop \
		tezos-proxy-server \
		tezos-sandbox \
	  $(foreach p, $(active_protocol_versions), tezos-baker-$(p) tezos-endorser-$(p) tezos-accuser-$(p)) \
	  $(foreach p, $(active_protocol_directories), src/proto_$(p)/parameters/sandbox-parameters.json src/proto_$(p)/parameters/test-parameters.json)
	@-${MAKE} -C docs clean
	@-${MAKE} -C tests_python clean
	@-rm -f docs/api/tezos-{baker,endorser,accuser}-alpha.html docs/api/tezos-{admin-,}client.html docs/api/tezos-signer.html
