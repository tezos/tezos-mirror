.PHONY: all
all: manifest # Regenerate GitLab CI configuration.
	cd .. && . ./scripts/version.sh && dune exec ci/bin/main.exe

.PHONY: verbose
verbose: manifest # Regenerate GitLab CI configuration with verbose output.
	cd .. && . ./scripts/version.sh && dune exec ci/bin/main.exe -- --verbose

.PHONY: inline-source-info
inline-source-info: manifest # Regenerate GitLab CI configuration, inlining source information in generated files.
	cd .. && . ./scripts/version.sh && dune exec ci/bin/main.exe -- --inline-source-info

.PHONY: remove-extra-files
remove-extra-files: manifest # Regenerate GitLab CI configuration and remove any non-generated files.
	cd .. && . ./scripts/version.sh && dune exec ci/bin/main.exe -- --remove-extra-files

.PHONY: list-pipelines
list-pipelines: manifest # List and describe the set of registered pipelines
	cd .. && . ./scripts/version.sh && dune exec ci/bin/main.exe -- --list-pipelines

.PHONY: overview-pipelines
overview-pipelines: manifest # List and describe the set of registered pipelines
	cd .. && . ./scripts/version.sh && dune exec ci/bin/main.exe -- --overview-pipelines

%.describe-pipeline: manifest # List and describe the set of registered pipelines
	cd .. && . ./scripts/version.sh && dune exec ci/bin/main.exe -- --describe-pipeline $*

.PHONY: manifest
manifest:
	cd .. && ${MAKE} --silent -C manifest

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

.PHONY: docker-list-pipelines
docker-list-pipelines: docker-do-list-pipelines # Build the target 'list-pipelines' using the 'build' Docker image.

.PHONY: check
check: all # Used in the CI to verify that [.gitlab-ci.yml] is up to date.
	@if [ $$(git status --porcelain | wc -l) -ne 0 ]; then ( \
	  echo "Repository not clean after 'make -C ci'."; \
	  echo "You should not edit generated GitLab CI .yml files directly."; \
	  echo "Edit the appropriate file(s) in ci/bin/ instead."; \
	  echo "Then run 'make -C ci' and commit the difference."; \
	  echo "Some new .yml files may have been created. If so, commit them as well."; \
	  exit 1 \
	); fi

help: # Display this help.
	@echo "GitLab CI configuration generator. Available targets:"
	@echo
	@grep '^[^[:space:]]\+:.*#' makefile | sed 's/:.*# /:/' | column -t -s':'
