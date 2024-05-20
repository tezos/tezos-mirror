# SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

NATIVE_TARGET ?= $(shell rustc -vV | grep 'host:' | awk '{print $$2}')
ifneq ($(NATIVE_TARGET),)
NATIVE_OPT := --target "$(NATIVE_TARGET)"
endif

.PHONY: all
all: build test check inbox-bench

.PHONY: build-deps
build-deps:
	@exec ../scripts/install-rust-std-hermit.sh
	@rustup component add rustfmt clippy
ifneq ($(NATIVE_TARGET),)
	@rustup target add $(NATIVE_TARGET)
endif

.PHONY: build
build: build-kernel inbox-bench

.PHONY: build-kernel
build-kernel:
	@cargo build -p jstz --release

.PHONY: test
test:
	@cargo test --no-default-features $(NATIVE_OPT)

.PHONY: run
run:
	@cargo run --no-default-features

export INBOX_FILE = $(shell mktemp)

.PHONY: check
check:
	@cargo check --all-features --workspace --locked
	@cargo fmt --check
	@cargo clippy --all-features -- --deny warnings

.PHONY: inbox-bench
inbox-bench:
	@cargo build $(NATIVE_OPT) --bin inbox-bench --release
	@cp target/$(NATIVE_TARGET)/release/inbox-bench .

.PHONY: clean
clean:
	@cargo clean
