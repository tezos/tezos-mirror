# SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

RUST_VERSION_SUPERVISED := 1.84.1

.PHONY: all
all: build test check

.PHONY: build
build:
	@cargo build --release

.PHONY: build-linux-musl
build-linux-musl:
	@cargo +$(RUST_VERSION_SUPERVISED) build --target=riscv64gc-unknown-linux-musl --release

.PHONY: build-deps
build-deps:
	@exec ../scripts/install-rust-std-hermit.sh
	@rustup toolchain install $(RUST_VERSION_SUPERVISED)
	@rustup target add --toolchain $(RUST_VERSION_SUPERVISED) riscv64gc-unknown-linux-musl

.PHONY: test
test:
	@# This executable compiles to RISC-V. It should be run in the RISC-V sandbox.

.PHONY: check
check:
	@exec ../scripts/format.sh --check
	@cargo check --workspace --locked
	@cargo clippy -- --deny warnings
	@cargo doc --all-features --document-private-items --no-deps

.PHONY: clean
clean:
	@cargo clean
