# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
# SPDX-FileCopyrightText: 2023-2025 TriliTech <contact@trili.tech>
# SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

.PHONY: build-deps
build-deps:
	rustup target add wasm32-unknown-unknown
	rustup target add wasm32-unknown-unknown
	../../src/riscv/scripts/install-rust-std-hermit.sh

.PHONY: all
all: check test

.PHONY: check-fmt
check-fmt:
	cargo fmt --check

.PHONY: check-native
check-native:
	cargo check --all-targets
	cargo clippy --all-targets

.PHONY: check-wasm
check-wasm:
	cargo check --target wasm32-unknown-unknown --no-default-features --features bls
	cargo clippy --target wasm32-unknown-unknown --no-default-features --features bls

.PHONY: check-riscv-hermit
check-riscv-hermit:
	cargo check --target riscv64gc-unknown-hermit --no-default-features
	cargo clippy --target riscv64gc-unknown-hermit --no-default-features

# TODO: SDK-74 -- check against riscv64gc-unknown-linux-musl, which requires a more
#       recent rust version
.PHONY: check
check: check-fmt check-native check-wasm check-riscv-hermit

.PHONY: test
test:
	cargo test
   
.PHONY: clean
clean:
	cargo clean
