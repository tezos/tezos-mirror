# SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

.PHONY: all
all: build test check

.PHONY: build
build:
	@cargo build --release

.PHONY: test
test:
	@# This executable compiles to RISC-V. It should be run in the RISC-V sandbox.

.PHONY: check
check:
	@cargo check --workspace --locked
	@cargo fmt --check
	@cargo clippy -- --deny warnings

.PHONY: clean
clean:
	@cargo clean
