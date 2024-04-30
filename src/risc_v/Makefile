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
build: risc-v-dummy.elf risc-v-sandbox
	@cargo build --release --workspace $(NATIVE_OPT)
	@make -C jstz build
	@make -C tests build

# Use this target when building in docker images with no Ocaml.
# The PVM requires both Ocaml and Rust to build, so it won't build with the
# Rust only toolchain docker images.
.PHONY: build-exclude-ocaml-api
build-exclude-ocaml-api: risc-v-dummy.elf risc-v-sandbox
	@cargo build --release --exclude octez-risc-v-api --workspace $(NATIVE_OPT)
	@make -C jstz build
	@make -C tests build

.PHONY: risc-v-sandbox
risc-v-sandbox::
	@cargo build --release --package risc-v-sandbox $(NATIVE_OPT)
	@ln -f target/$(NATIVE_TARGET)/release/risc-v-sandbox $@

risc-v-dummy.elf::
	@make -C dummy_kernel build
	@ln -f dummy_kernel/target/riscv64gc-unknown-hermit/release/risc-v-dummy $@

.PHONY: build-deps
build-deps:
	@exec ./install-rust-std-hermit.sh
	@rustup component add rustfmt clippy
ifneq ($(NATIVE_TARGET),)
	@rustup target add $(NATIVE_TARGET)
endif

	# Iterate through all the toolchains. 'rustup show' will install the
	# toolchain in addition to showing toolchain information.
	@find . -iname 'rust-toolchain*' -execdir rustup show active-toolchain \; 2>/dev/null

.PHONY: test
test: build
	@cargo test --workspace
	@make -C dummy_kernel test
	@make -C jstz test

.PHONY: test-exclude-ocaml-api
test-no-pvm: build-exclude-ocaml-api
	@cargo test --exclude octez-risc-v-api --workspace
	@make -C dummy_kernel test
	@make -C jstz test

.PHONY: check
check:
	@cargo update --workspace --locked
	@cargo check --workspace --all-targets
	@cargo clippy --workspace --all-targets -- --deny warnings
	@taplo format --check $(shell git ls-files '**/Cargo.toml')
	@make -C dummy_kernel check
	@make -C jstz check
	@make -C tests check

	# Check format of all crates
	@exec ../../scripts/check-format-rust.sh

.PHONY: check-exclude-ocaml-api
check-exclude-ocaml-api:
	@cargo update --workspace --locked
	@cargo check --exclude octez-risc-v-api --workspace --all-targets
	@cargo clippy --exclude octez-risc-v-api --workspace --all-targets -- --deny warnings
	@taplo format --check $(shell git ls-files '**/Cargo.toml')
	@make -C dummy_kernel check
	@make -C jstz check
	@make -C tests check

	# Check format of all crates
	@exec ../../scripts/check-format-rust.sh

.PHONY: audit
audit:
	@# We don't add this to the check target because it requires installation
	@# of 'cargo-audit' via 'cargo install' which we can't control very well
	@# on the machines of other developers. E.g. if we need to upgrade its
	@# version, it may cause issues for developers.
	@# For that reason we expose this as a separate target and run it only in
	@# CI.
	@cargo audit

.PHONY: clean
clean:
	@cargo clean
	@make -C dummy_kernel clean
	@make -C jstz clean
	@make -C tests clean
	@rm -f risc-v-sandbox risc-v-dummy.elf
