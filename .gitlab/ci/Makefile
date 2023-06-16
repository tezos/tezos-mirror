# Ensure we are in Gitlab runner context
ifndef CI_PROJECT_DIR
	$(error This Makefile must be called from a Gitlab runner)
endif

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
		./scripts/remove-old-protocols.sh .trash && \
		make all && \
		./scripts/restore-old-protocols.sh .trash && \
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
