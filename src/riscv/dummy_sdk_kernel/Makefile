# SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

.PHONY: all
all: build check

.PHONY: build
build:
	@cargo build --release

.PHONY: build-deps
build-deps:
	@exec ../scripts/install-rust-std-hermit.sh

.PHONY: check
check:
	@exec ../scripts/format.sh --check
	@cargo check --workspace --locked
	@cargo clippy -- --deny warnings
	@cargo doc --all-features --document-private-items --no-deps

.PHONY: clean
clean:
	@cargo clean
