# SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
#
# SPDX-License-Identifier: MIT

.PHONY: all
all: build test

build:
	@cargo fmt --check
	@cargo clippy --all-targets -- --deny warnings
	@cargo build --release

build-deps:
	@rustup component add rustfmt clippy

test:
	@cargo test
