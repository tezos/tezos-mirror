.PHONY: all
all: # Regenerate GitLab CI configuration.
	(cd .. && . ./scripts/version.sh && dune exec ci/bin/main.exe)

.PHONY: verbose
verbose: # Regenerate GitLab CI configuration with verbose output.
	(cd .. && . ./scripts/version.sh && dune exec ci/bin/main.exe -- --verbose)

.PHONY: inline-source-info
inline-source-info: # Regenerate GitLab CI configuration, inlining source information in generated files.
	(cd .. && . ./scripts/version.sh && dune exec ci/bin/main.exe -- --inline-source-info)

.PHONY: docker-%
docker-%:
	@( cd .. \
	    && . ./scripts/version.sh \
	    && docker run -it -w$$(pwd) -v$$(pwd):$$(pwd) --entrypoint opam \
	         registry.gitlab.com/tezos/opam-repository:runtime-build-dependencies--$${opam_repository_tag} exec -- \
	         make -C ci $* )

.PHONY: docker-all
docker-all: # Build the target 'all' using the 'runtime-build-dependencies' Docker image.

.PHONY: docker-verbose
docker-verbose: # Build the target 'verbose' using the 'runtime-build-dependencies' Docker image.

.PHONY: docker-inline-source-info
docker-inline-source-info: # Build the target 'inline-source-info' using the 'runtime-build-dependencies' Docker image.

.PHONY: check
check: # Used in the CI to verify that [.gitlab-ci.yml] is up to date.
	@git diff --exit-code HEAD -- ../.gitlab-ci.yml || (echo "Cannot check generated [.gitlab-ci.yml] file, some changes are uncommitted"; exit 1)
	@$(MAKE) all
	@git diff --exit-code HEAD -- ../.gitlab-ci.yml > /dev/null || ( \
	  echo "Repository not clean after 'make -C ci'."; \
	  echo "You should not edit generated GitLab CI .yml files directly."; \
	  echo "Edit ci/bin/main.ml instead."; \
	  echo "Then run 'make -C ci' and commit the difference."; \
	  exit 1 \
	)

help: # Display this help.
	@echo "GitLab CI configuration generator. Available targets:"
	@echo
	@grep '^[^[:space:]]\+: #' makefile | sed 's/^\([^[:space:]]\+\): # \(.*\)/ - \1: \2/'
