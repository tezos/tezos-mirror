# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

.PHONY: check
check:
	@make -C rust/ check

.PHONY: bindings
bindings:
	@make -C rust/ generate-bindings

.PHONY: clean
clean:
	@make -C rust/ clean
	@make -C python/ clean
	@make -C kotlin/demo clean
	@make -C swift clean

.PHONY: test
test: test-rust test-bindings

.PHONY: test-rust
test-rust:
	@make -C rust/ test

.PHONY: test-bindings
test-bindings: bindings
	@make -C python/ test
	@make -C kotlin/demo test
	@make -C swift test
