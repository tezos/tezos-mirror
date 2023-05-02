# SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
#
# SPDX-License-Identifier: MIT

.PHONY: all
all: build check test

.PHONY: build
build:
	@cargo build --release

.PHONY: check
check:
	@cargo fmt --check
	@cargo clippy --all-targets -- --deny warnings

.PHONY: build-deps
build-deps:

.PHONY: build-dev-deps
build-dev-deps:
	@rustup component add rustfmt clippy

.PHONY: test
test:
	@cargo test
