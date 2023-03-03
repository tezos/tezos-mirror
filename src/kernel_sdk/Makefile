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
	@cargo check --target wasm32-unknown-unknown
	@cargo build -p tezos_smart_rollup_installer --release

installer.wasm:
	@cargo build -p installer_kernel \
		--target wasm32-unknown-unknown \
		--release \
		--no-default-features \
		--features entrypoint
	@cp target/wasm32-unknown-unknown/release/installer_kernel.wasm \
		installer.wasm
	@wasm-strip installer.wasm

test:
	@cargo test --all-features

doc:
	@cargo doc  --no-deps --all-features

build-deps:
	@rustup target add wasm32-unknown-unknown
	@rustup component add rustfmt clippy

clean:
	@cargo clean
	@rm installer.wasm
