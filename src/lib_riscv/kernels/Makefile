# SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT

KERNELS=riscv-echo

BUILD_DIR=target/riscv64gc-unknown-linux-musl/release

all: check build

build: ${KERNELS}

cargo-build:
	@cargo build --release

riscv-echo: cargo-build
	@mv $(BUILD_DIR)/$@ $@
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
