# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
# SPDX-FileCopyrightText: 2023-2025 TriliTech <contact@trili.tech>
# SPDX-FileCopyrightText: 2023,2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

RISCV_RUSTC_VERSION = 1.88.0

.PHONY: all
all: check test

.PHONY: build-deps
build-deps:
	@rustup target add wasm32-unknown-unknown
	@rustup toolchain install ${RISCV_RUSTC_VERSION} --target riscv64gc-unknown-linux-musl

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

.PHONY: check-riscv
check-riscv:
	cargo +${RISCV_RUSTC_VERSION} check --target riscv64gc-unknown-linux-musl --no-default-features
	cargo +${RISCV_RUSTC_VERSION} clippy --target riscv64gc-unknown-linux-musl --no-default-features

.PHONY: check
check: check-fmt check-native check-wasm check-riscv

.PHONY: test
test:
	cargo test

.PHONY: clean
clean:
	cargo clean
