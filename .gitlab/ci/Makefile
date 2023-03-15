# Ensure we are in Gitlab runner context
ifndef CI_PROJECT_DIR
	$(error This Makefile must be called from a Gitlab runner)
endif

last_proto_name := $(shell find ${CI_PROJECT_DIR}/src -name "proto_[0-9][0-9][0-9]_*" | awk -F'/' '{print $$NF}' | sort -r | head -1)
last_proto_version := $(shell echo ${last_proto_name} | cut -d'_' -f2 | sed 's/^0*//')
new_proto_version_int:=$(shell echo $$(($(last_proto_version)+1)))
new_proto_version := $(shell if [ "${new_proto_version_int}" -lt "100" ]; then echo 0${new_proto_version_int}; else echo ${new_proto_version_int}; fi)

.PHONY: test-protocol-compiles
test-protocol-compiles:
	cd ${CI_PROJECT_DIR} && \
		dune build @runtest_compile_protocol

.PHONY: test-tezt
test-tezt:
	cd ${CI_PROJECT_DIR} && \
		_build/default/tezt/tests/main.exe ${TESTS} --color --log-buffer-size 5000 --log-file tezt.log --global-timeout 1800 --on-unknown-regression-files fail --junit ${JUNIT} --from-record tezt/records --job ${CI_NODE_INDEX}/${CI_NODE_TOTAL} --record tezt-results-${CI_NODE_INDEX}.json -j 4 --retry 1 && \
		./scripts/ci/merge_coverage.sh

.PHONY: test-tezt-static-binaries
test-tezt-static-binaries:
ifndef TESTS
	$(error TESTS is undefined)
endif
	cd ${CI_PROJECT_DIR} && \
		dune exec tezt/tests/main.exe -- ${TESTS} --color --log-buffer-size 5000 --log-file tezt.log --global-timeout 1800 --on-unknown-regression-files fail --junit ${JUNIT} --from-record tezt/records --job ${CI_NODE_INDEX}/${CI_NODE_TOTAL} --record tezt-results-${CI_NODE_INDEX}.json -j 4 --retry 1

.PHONY: test-tezt-build-long
test-tezt-build-long:
	cd ${CI_PROJECT_DIR} && \
		dune build @tezt/long_tests/check

.PHONY: test-tezt-self-test
test-tezt-self-test:
	cd ${CI_PROJECT_DIR} && \
		dune runtest tezt/self_tests/

.PHONY: test-tezt-fetch-records
test-tezt-fetch-records:
	cd ${CI_PROJECT_DIR} && \
		dune exec tezt/records/update.exe -- --log-file tezt-fetch-records.log --test-arg from=last-merged-pipeline --info

.PHONY: integration-test-compiler-rejections
integration-test-compiler-rejections:
	cd ${CI_PROJECT_DIR} && \
		dune build @runtest_rejections

.PHONY: integration-test-snapshot-alpha-and-link
integration-test-snapshot-alpha-and-link: gitlab-take-ownership
	cd ${CI_PROJECT_DIR} && \
		$(MAKE) octez-protocol-compiler && \
		./scripts/snapshot_alpha_and_link.sh "${new_proto_version}" next && \
		$(MAKE) && \
		dune build src/proto_"${new_proto_version}"_*/ && \
		./scripts/user_activated_upgrade.sh src/proto_"${new_proto_version}"_*/ 1 && \
		dune build src/proto_"${new_proto_version}"_*/

.PHONY: integration-test-liquidity-baking-scripts
integration-test-liquidity-baking-scripts:
	cd ${CI_PROJECT_DIR} && \
		./scripts/ci/test_liquidity_baking_scripts.sh && \
		./scripts/ci/merge_coverage.sh

.PHONY: lint-misc-check
lint-misc-check: gitlab-take-ownership
	cd ${CI_PROJECT_DIR} && \
		./scripts/ci/lint_misc_check.sh

.PHONY: check-precommit
check-precommit:
	cd ${CI_PROJECT_DIR} && \
		poetry run pylint scripts/pre_commit/pre_commit.py && \
		poetry run pycodestyle scripts/pre_commit/pre_commit.py && \
		poetry run mypy scripts/pre_commit/pre_commit.py

.PHONY: check-scripts-b58-prefix
check-scripts-b58-prefix:
	cd ${CI_PROJECT_DIR} && \
		poetry run pylint scripts/b58_prefix/b58_prefix.py --disable=missing-docstring --disable=invalid-name && \
		poetry run pytest scripts/b58_prefix/test_b58_prefix.py -v

.PHONY: build-static-binaries
build-static-binaries: gitlab-take-ownership
	cd ${CI_PROJECT_DIR} && \
		./scripts/ci/build_static_binaries.sh

.PHONY: build-full-unreleased
build-full-unreleased:
	cd ${CI_PROJECT_DIR} && \
		./scripts/ci/build_full_unreleased.sh

.PHONY: sanity-ci
sanity-ci: gitlab-take-ownership
	${MAKE} -C ${CI_PROJECT_DIR}/manifest check
	${CI_PROJECT_DIR}/scripts/lint.sh --check-gitlab-ci-yml

ifndef TEZOS_WITHOUT_OPAM
.PHONY: opam-prepare
opam-prepare:
	cd ${CI_PROJECT_DIR} && \
		git init _opam-repo-for-release && \
		./scripts/opam-prepare-repo.sh dev ./ ./_opam-repo-for-release && \
		git -C _opam-repo-for-release add packages && \
		git -C _opam-repo-for-release commit -m "tezos packages"
endif

ifndef TEZOS_WITHOUT_OPAM
.PHONY: opam-configure
opam-configure:
	cd ${CI_PROJECT_DIR} && \
		opam remote add dev-repo ./_opam-repo-for-release  && \
		opam install --yes ${package}.dev && \
		opam reinstall --yes --with-test ${package}.dev
endif

.PHONY: doc-build-all
doc-build-all:
	cd ${CI_PROJECT_DIR} && \
		make all && \
		make -C docs -j all

.PHONY: doc-publish
doc-publish:
	cd ${CI_PROJECT_DIR} && \
		./scripts/ci/doc_publish.sh

.PHONY: doc-linkcheck
doc-linkcheck:
	cd ${CI_PROJECT_DIR} && \
		make all && \
		make -C docs redirectcheck && \
		make -C docs linkcheck

.PHONY: opam-release
opam-release:
	cd ${CI_PROJECT_DIR} && \
		./scripts/ci/opam-release.sh

.PHONY: gitlab-release
gitlab-release:
	cd ${CI_PROJECT_DIR} && \
		./scripts/ci/gitlab-release.sh

.PHONY: gitlab-take-ownership
gitlab-take-ownership:
	# Note: Only done in Gitlab CI context
ifdef CI_PROJECT_DIR
	# FIXME: https://gitlab.com/tezos/tezos/-/issues/2865
	sudo chown -R $(shell id -u):$(shell id -g) ${CI_PROJECT_DIR}
endif
