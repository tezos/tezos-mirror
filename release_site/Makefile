.PHONY: build
build:
	dune build src/

.PHONY: test
test: build
	dune exec tezt/main.exe

.PHONY: test-version-manager
test-version-manager: build
	dune exec tezt/main.exe -- --file test_version_manager.ml

.PHONY: test-generate-release-page
test-generate-release-page: build
	dune exec tezt/main.exe -- --file test_generate_release_page.ml
