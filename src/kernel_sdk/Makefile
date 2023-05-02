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

.PHONY: all
all: build test doc

NATIVE_TARGET ?=
ifneq ($(NATIVE_TARGET),)
NATIVE_OPT := --target "$(NATIVE_TARGET)"
endif

SDK_VERSION := "0.1.0"

.PHONY: check
check: installer-template publish-check
	@cargo fmt --check
	@cargo clippy --all-targets --all-features -- --deny warnings
	@cargo check --no-default-features
	@cargo check --target wasm32-unknown-unknown

# Ensure that individual crates have setup features on
# their own dependencies correctly - when building as
# a workspace, features enabled by one crate are enabled
# on all.
.PHONY: publish-check
publish-check: installer-template
	@cargo check -p tezos-smart-rollup-core
	@cargo check -p tezos-smart-rollup-host
	@cargo check -p tezos-smart-rollup-debug
	@cargo check -p tezos-smart-rollup-panic-hook
	@cargo check -p tezos-smart-rollup-entrypoint
	@cargo check -p tezos-smart-rollup-encoding
	@cargo check -p tezos-smart-rollup-storage
	@cargo check -p tezos-smart-rollup-mock
	@cargo check -p tezos-smart-rollup-installer
	@cargo check -p tezos-smart-rollup

.PHONY: build
build: installer-template
	@cargo build -p tezos-smart-rollup-installer \
	        --release \
	        $(NATIVE_OPT)

.PHONY: sdk-readme
sdk-readme:
	@cp README.md sdk/README.md

.PHONY: installer-template
installer-template: sdk-readme
	@cargo build -p installer-kernel \
	       --target wasm32-unknown-unknown \
	       --release \
	       --no-default-features \
	       --features entrypoint
	@cp target/wasm32-unknown-unknown/release/installer_kernel.wasm \
	    installer-client/installer.wasm
	@wasm-strip installer-client/installer.wasm

.PHONY: test
test:
	@cargo test --all-features

.PHONY: doc
doc: sdk-readme
	@cargo doc  --no-deps --all-features

.PHONY: build-deps
build-deps:
	@rustup target add wasm32-unknown-unknown $(NATIVE_TARGET)

.PHONY: build-dev-deps
build-dev-deps: build-deps
	@rustup component add rustfmt clippy

.PHONY: clean
clean:
	@cargo clean
	@rm -rf installer-client/installer.wasm
	@rm -rf sdk/README.md

.PHONY: publish-deps
publish-deps: build-deps
	@cargo install cargo-publish-workspace-v2 --version "0.2.1"

# --allow-dirty required to include the built installer.wasm
# and README copied to sdk/, which are not checked in
.PHONY: publish
publish: installer-template sdk-readme
	@cargo publish-workspace -p 'tezos-smart-rollup-' \
	       --exclude installer-kernel \
	       --target-version $(SDK_VERSION) \
	       --aligned-versions-only \
	       -- --allow-dirty
