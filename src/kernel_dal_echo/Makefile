# SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
#
# SPDX-License-Identifier: MIT

NATIVE_TARGET ?=
ifneq ($(NATIVE_TARGET),)
NATIVE_OPT := --target "$(NATIVE_TARGET)"
endif

.PHONY: all
all: build test check

.PHONY: dal_echo_kernel
dal_echo_kernel:
	@cargo build --target wasm32-unknown-unknown --release -p dal_echo_kernel

.PHONY: build
build: dal_echo_kernel

.PHONY: build-deps
build-deps:
	@rustup target add wasm32-unknown-unknown $(NATIVE_TARGET)

.PHONY: build-dev-deps
build-dev-deps: build-deps
	@rustup component add rustfmt clippy

.PHONY: test
test:
	@cargo test --features testing

.PHONY: check
check:
	@cargo clippy --features testing --all-targets -- --deny warnings

.PHONY: clean
clean:
	@cargo clean
