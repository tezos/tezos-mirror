# Ensure we are in Gitlab runner context
ifndef CI_PROJECT_DIR
	$(error This Makefile must be called from a Gitlab runner)
endif

ifndef TEZOS_WITHOUT_OPAM
.PHONY: opam-prepare
opam-prepare:
	cd ${CI_PROJECT_DIR} && \
		git init _opam-repo-for-release && \
		./scripts/opam-prepare-repo.sh dev ./ ./_opam-repo-for-release && \
		git -C _opam-repo-for-release add packages && \
		git -C _opam-repo-for-release commit -m "tezos packages"
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
