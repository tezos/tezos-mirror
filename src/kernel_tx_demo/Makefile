# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
# SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

NATIVE_TARGET ?=
ifneq ($(NATIVE_TARGET),)
NATIVE_OPT := --target "$(NATIVE_TARGET)"
endif

.PHONY: all
all: build test check

.PHONY: build
build:
	@cargo build --target wasm32-unknown-unknown --release -p tx_kernel
	@cargo build $(NATIVE_OPT) --release -p tx-demo-collector

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
	@cargo fmt --check
	@cargo clippy --features testing --all-targets -- --deny warnings

.PHONY: clean
clean:
	@cargo clean
