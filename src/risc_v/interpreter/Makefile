# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT

.PHONY: all
all: build test check

.PHONY: build
build:
	@cargo build --release

.PHONY: build-deps
build-deps:

.PHONY: build-dev-deps
build-dev-deps: build-deps
	@rustup component add rustfmt clippy

.PHONY: test
test:
	@cargo test

.PHONY: check
check:
	@cargo fmt --check
	@cargo clippy --all-targets -- --deny warnings

.PHONY: clean
clean:
	@cargo clean
