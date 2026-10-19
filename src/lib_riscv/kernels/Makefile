# SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT

KERNELS=riscv-echo

CARGO_TARGET_DIR ?= target
export CARGO_TARGET_DIR
BUILD_DIR=$(CARGO_TARGET_DIR)/riscv64gc-unknown-linux-musl/release

all: check build

build: ${KERNELS}

# Shared with the other Dune-managed Rust components.
CARGO_BUILD_SCRIPT ?= ../../../scripts/cargo-build.sh

cargo-build:
	@sh "$(CARGO_BUILD_SCRIPT)" "$(DUNE_PROFILE)" "$(OCTEZ_RUST_INCREMENTAL)" --release

riscv-echo: cargo-build
	@cp "$(BUILD_DIR)/$@" "$@"
	@shasum -a 256 $@ > $@.checksum

build-dev-deps:
	@rustup component add rustfmt clippy

check:
	@exec ../../../scripts/check-format-rust.sh
	@cargo check --workspace
	@cargo clippy --workspace -- --deny warnings

clean:
	@cargo clean
	@rm -f $(KERNELS) $(addsuffix .checksum,$(KERNELS))

audit:
	@cargo audit

# Mark all targets as phony to make sure they're always executed
.PHONY: all $(MAKECMDGOALS)
