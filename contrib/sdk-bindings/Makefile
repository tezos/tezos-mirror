# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

MAKE_RUST = $(MAKE) -C rust/
MAKE_PYTHON = $(MAKE) -C rust/ -f python.mk
MAKE_KOTLIN = $(MAKE) -C kotlin/demo/
MAKE_SWIFT = $(MAKE) -C swift/

.PHONY: check
check:
	@$(MAKE_RUST) check

.PHONY: bindings
bindings:
	@$(MAKE_RUST) generate-bindings

.PHONY: clean
clean:
	@$(MAKE_RUST) clean
	@$(MAKE_PYTHON) clean
	@$(MAKE_KOTLIN) clean
	@$(MAKE_SWIFT) clean

.PHONY: test
test: test-rust test-bindings

.PHONY: test-rust
test-rust:
	@$(MAKE_RUST) test

.PHONY: test-bindings
test-bindings: bindings
	@$(MAKE_PYTHON) test
	@$(MAKE_KOTLIN) test
	@$(MAKE_SWIFT) test

.PHONY: publish
publish:
	@$(MAKE_PYTHON) publish
