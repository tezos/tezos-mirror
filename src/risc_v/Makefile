# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT

.PHONY: all
all: build test check

# In some situations we might want to override the compilation target.
NATIVE_TARGET ?=

ifneq ($(NATIVE_TARGET),)
NATIVE_OPT := --target="$(NATIVE_TARGET)"
endif

.PHONY: build
build:
	@cargo build --release --workspace $(NATIVE_OPT)
	@make -C dummy_kernel build
	@make -C jstz build

.PHONY: build-sandbox
build-sandbox:
	@cargo build --release --package risc-v-sandbox $(NATIVE_OPT)

.PHONY: build-deps
build-deps:
	@exec ./install-rust-std-hermit.sh
	@rustup component add rustfmt clippy

.PHONY: test
test:
	@cargo test --workspace
	@make -C dummy_kernel test
	@make -C jstz build

.PHONY: check
check:
	@cargo check --workspace
	@cargo clippy --workspace -- --deny warnings
	@make -C dummy_kernel check
	@make -C jstz check

.PHONY: clean
clean:
	@cargo clean
	@make -C dummy_kernel clean
	@make -C jstz clean
