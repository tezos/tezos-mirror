# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
# SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

# The SDK contains some executables, that may be used by tezt.
# Since the tezt tests run on a different container to the one used
# for `make build-kernels`, we allow to compile executables using the
# musl toolchain, to make them fully statically-linked - allowing them
# to run in the tezt container, by setting the `NATIVE_TARGET`
# environment variable.

CC=clang

all: build test doc

NATIVE_TARGET ?=
ifneq ($(NATIVE_TARGET),)
NATIVE_OPT := --target "$(NATIVE_TARGET)"
endif

SDK_VERSION := "0.1.0"

check:
	@cargo fmt --check
	@cargo clippy --all-targets --all-features -- --deny warnings
	@cargo check --no-default-features
	@cargo check --target wasm32-unknown-unknown

build: check installer.wasm
	@cargo build -p tezos-smart-rollup-installer \
		--release \
                $(NATIVE_OPT)

installer.wasm:
	@cargo build -p installer-kernel \
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
	@rustup target add wasm32-unknown-unknown $(NATIVE_TARGET)
	@rustup component add rustfmt clippy

clean:
	@cargo clean
	@rm installer.wasm

publish-deps: build-deps
	@cargo install cargo-publish-workspace-v2 --version "0.2.1"

publish: installer.wasm
	@cargo publish-workspace -p 'tezos-smart-rollup-' \
		--exclude installer-kernel \
		--target-version $(SDK_VERSION)
