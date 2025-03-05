# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

.PHONY: check
check:
	@cargo fmt --check
	@cargo check --locked --all-targets
	@cargo clippy --all-targets

.PHONY: build
build:
	@cargo build --release

.PHONY: clean
clean:
	@cargo clean

.PHONY: test
test:
	@cargo test
