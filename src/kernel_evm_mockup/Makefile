# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
# SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

all:
	@cargo fmt --check
	@cargo clippy -- --deny warnings
	@cargo build --target wasm32-unknown-unknown --release

build-deps:
	@rustup target add wasm32-unknown-unknown
	@rustup component add rustfmt clippy
