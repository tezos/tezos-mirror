# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT


all:
	@rustup target add wasm32-unknown-unknown
	@cargo fmt --check
	@cargo clippy -- --deny warnings
	@cargo build --target wasm32-unknown-unknown --release
