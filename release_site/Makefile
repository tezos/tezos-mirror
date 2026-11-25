.PHONY: build
build:
	dune build src/

.PHONY: test
test: build
	dune exec tezt/main.exe

.PHONY: test-version-manager
test-version-manager: build
	dune exec tezt/main.exe -- --file test_version_manager.ml

.PHONY: test-release-site
test-release-site: build
	dune exec tezt/main.exe -- --file test_release_site.ml
