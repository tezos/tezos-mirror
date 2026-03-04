.PHONY: build
build:
	dune build .

.PHONY: test
test: build
	dune exec tezt/main.exe

.PHONY: test-version-manager
test-version-manager: build
	dune exec tezt/main.exe -- --file test_version_manager.ml
