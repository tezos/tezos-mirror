# SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

.PHONY: all
all: build test check

.PHONY: build
build:
	@cargo build --release --target=riscv64gc-unknown-none-elf
	@cargo build --release --target=riscv64gc-unknown-hermit --no-default-features --features crypto

.PHONY: build-deps
build-deps:
	@rustup target add riscv64gc-unknown-none-elf

.PHONY: build-dev-deps
build-dev-deps: build-deps
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
