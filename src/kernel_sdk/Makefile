# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
# SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

CC=$(which clang)

all: build test doc

build:
	@cargo fmt --check
	@cargo clippy --all-targets --all-features -- --deny warnings
	@cargo check --no-default-features
	@cargo build --target wasm32-unknown-unknown --release

test:
	@cargo test --all-features

doc:
	@cargo doc

build-deps:
	@rustup target add wasm32-unknown-unknown
	@rustup component add rustfmt clippy
