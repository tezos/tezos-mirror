.PHONY: all
all:  # Regenerate GitLab CI configuration.
	cd .. && ${MAKE} -C manifest && . ./scripts/version.sh && dune exec ci/bin/main.exe

.PHONY: verbose
verbose: # Regenerate GitLab CI configuration with verbose output.
	cd .. && ${MAKE} -C manifest && . ./scripts/version.sh && dune exec ci/bin/main.exe -- --verbose

.PHONY: inline-source-info
inline-source-info: # Regenerate GitLab CI configuration, inlining source information in generated files.
	cd .. && ${MAKE} -C manifest && . ./scripts/version.sh && dune exec ci/bin/main.exe -- --inline-source-info

.PHONY: remove-extra-files
remove-extra-files: # Regenerate GitLab CI configuration and remove any non-generated files.
	cd .. && ${MAKE} -C manifest && . ./scripts/version.sh && dune exec ci/bin/main.exe -- --remove-extra-files

.PHONY: docker-do-%
docker-do-%:
	@cd .. \
	    && . ./scripts/version.sh \
	    && docker run -it -w$$(pwd) -v$$(pwd):$$(pwd) --entrypoint opam \
	         $${ci_image_name}/build:amd64--$$(images/image_tag.sh images/ci) exec -- \
			 make --always-make -C ci $* # --always-make to not mix build artifacts from host and guest system.

.PHONY: docker-all
docker-all: docker-do-all # Build the target 'all' using the 'build' Docker image.

.PHONY: docker-verbose
docker-verbose: docker-do-verbose # Build the target 'verbose' using the 'build' Docker image.

.PHONY: docker-inline-source-info
docker-inline-source-info: docker-do-inline-source-info # Build the target 'inline-source-info' using the 'build' Docker image.

.PHONY: docker-remove-extra-files
docker-remove-extra-files: docker-do-remove-extra-files # Build the target 'remove-extra-files' using the 'build' Docker image.

.PHONY: check
check: all # Used in the CI to verify that [.gitlab-ci.yml] is up to date.
	@git status | grep "nothing to commit" > /dev/null || ( \
	  echo "Repository not clean after 'make -C ci'."; \
	  echo "You should not edit generated GitLab CI .yml files directly."; \
	  echo "Edit the appropriate file(s) in ci/bin/ instead."; \
	  echo "Then run 'make -C ci' and commit the difference."; \
	  exit 1 \
	)

help: # Display this help.
	@echo "GitLab CI configuration generator. Available targets:"
	@echo
	@grep '^[^[:space:]]\+:.*#' makefile | sed 's/^\([^[:space:]]\+\):.*# \(.*\)/ - \1: \2/'
