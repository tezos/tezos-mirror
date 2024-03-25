# SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

.PHONY: build-deps
build-deps:
	@rustup component add rustfmt clippy

check:
	@cargo check --all-targets --locked
	@cargo clippy --all-targets
	@cargo fmt --check

.PHONY: build
build:
	@cargo build --release
