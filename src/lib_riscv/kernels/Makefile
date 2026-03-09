# SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT

all: check build

build:
	@cargo build --release

build-dev-deps:
	@rustup component add rustfmt clippy

check:
	@exec ../../../scripts/check-format-rust.sh
	@cargo check --workspace
	@cargo clippy --workspace -- --deny warnings

clean:
	@cargo clean

audit:
	@cargo audit

# Mark all targets as phony to make sure they're always executed
.PHONY: all $(MAKECMDGOALS)
