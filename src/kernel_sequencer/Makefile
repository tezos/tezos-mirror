# SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
# SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

.PHONY: all
all: build check test

.PHONY: build
build:
	@cargo build --release --target wasm32-unknown-unknown --example sequenced_kernel

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

.PHONY: clean
clean:
	@cargo clean
