# SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

.PHONY: all
all: build test check

.PHONY: build
build:
	@cargo build --release --target=riscv64gc-unknown-hermit

.PHONY: build-dev-deps
build-dev-deps: build-deps
	@exec ./install-rust-std-hermit.sh
	@rustup component add rustfmt clippy

.PHONY: test
test:
	@cargo test --no-default-features

.PHONY: run
run:
	@cargo run --no-default-features

.PHONY: check
check:
	@cargo fmt --check
	# TODO: Clippy! (currently doesn't work)

.PHONY: clean
clean:
	@cargo clean
