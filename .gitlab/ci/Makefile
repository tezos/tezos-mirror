# Ensure we are in Gitlab runner context
ifndef CI_PROJECT_DIR
	$(error This Makefile must be called from a Gitlab runner)
endif

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
