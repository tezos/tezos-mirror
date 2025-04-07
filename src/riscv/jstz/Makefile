# SPDX-FileCopyrightText: 2023-2025 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

NATIVE_TARGET ?= $(shell rustc -vV | grep 'host:' | awk '{print $$2}')
ifneq ($(NATIVE_TARGET),)
NATIVE_OPT := --target "$(NATIVE_TARGET)"
endif

RUST_VERSION_SUPERVISED := 1.84.1

INBOX_FILE ?= $(shell mktemp)

.PHONY: all
all: build test check inbox-bench

.PHONY: print-native-target
print-native-target:
	$(info $(NATIVE_TARGET))

.PHONY: build-deps
build-deps:
	@exec ../scripts/install-rust-std-hermit.sh
	@rustup toolchain install $(RUST_VERSION_SUPERVISED)
	@rustup component add rustfmt clippy
	@rustup target add --toolchain $(RUST_VERSION_SUPERVISED) riscv64gc-unknown-linux-musl
ifneq ($(NATIVE_TARGET),)
	@rustup target add $(NATIVE_TARGET)
endif

.PHONY: build
build: build-kernel inbox-bench

.PHONY: build-kernel
build-kernel:
	@cargo build -p jstz --release

.PHONY: build-kernel-static
build-kernel-static:
	@INBOX_FILE=$(INBOX_FILE) cargo build -p jstz --release --features static-inbox

.PHONY: build-kernel-native
build-kernel-native:
	@INBOX_FILE=$(INBOX_FILE) cargo build -p jstz --release --features static-inbox $(NATIVE_OPT)

.PHONY: build-kernel-linux-musl
build-kernel-linux-musl:
	@cargo +$(RUST_VERSION_SUPERVISED) build --target=riscv64gc-unknown-linux-musl --package jstz --release

.PHONY: test
test:
	@cargo test --no-default-features $(NATIVE_OPT)
	@../scripts/jstz-bench.sh -t 1
	@../scripts/jstz-bench.sh -t 1 -s
	@../scripts/jstz-bench.sh -t 1 -sn

.PHONY: run
run:
	@cargo run --no-default-features

.PHONY: check
check:
	@exec ../scripts/format.sh --check
	@INBOX_FILE=$(INBOX_FILE) cargo check --all-features --workspace --locked
	@INBOX_FILE=$(INBOX_FILE) cargo clippy --all-features -- --deny warnings
	@INBOX_FILE=$(INBOX_FILE) cargo doc --all-features --document-private-items --no-deps

.PHONY: inbox-bench
inbox-bench:
	@cargo build $(NATIVE_OPT) --bin inbox-bench --release
	@cp target/$(NATIVE_TARGET)/release/inbox-bench .

.PHONY: clean
clean:
	@cargo clean
