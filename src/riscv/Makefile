# SPDX-FileCopyrightText: 2023-2025 Nomadic Labs <contact@nomadic-labs.com>
# SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

.PHONY: all
all: build check

# In some situations we might want to override the compilation target.
NATIVE_TARGET ?=

ifneq ($(NATIVE_TARGET),)
NATIVE_OPT := --target="$(NATIVE_TARGET)"
endif

# We don't want to check all features in CI, so we provide this varible to configure it.
CHECK_FLAGS ?= --all-features

# CI needs the ability to toggle some features
EXTRA_FLAGS ?=

# Mechanism to allow enabling additional features - for example JIT
SANDBOX_ENABLE_FEATURES ?=

# The version of nightly that we use for anything that requires it
NIGHTLY_VERSION = nightly-2025-01-30

# Prefix for downloading assets from the riscv-pvm repo
RISCV_PVM_REPO = https://raw.githubusercontent.com/tezos/riscv-pvm/refs/heads/main

ASSETS := \
  assets/riscv-dummy.elf \
  assets/jstz \
  assets/jstz_proof_initial \
  assets/etherlink \
  assets/etherlink_proof_initial

.PHONY: build
build:
	@cargo build --release --workspace $(NATIVE_OPT) $(EXTRA_FLAGS)

# Update checked-in binaries and regression files
.PHONY: update-assets $(ASSETS)
update-assets: $(ASSETS)

# Update to the latest version of `riscv-pvm`. Assets will be updated accordingly.
.PHONY: update-riscv-pvm
update-riscv-pvm: update-assets
	@cargo update octez-riscv

# Update the compiled dummy kernel used in CI tests
assets/riscv-dummy.elf assets/riscv-dummy.elf.checksum::
	@curl -L -o $@ $(RISCV_PVM_REPO)/assets/riscv-dummy.elf
	@curl -L -o $@.checksum $(RISCV_PVM_REPO)/assets/riscv-dummy.elf.checksum

# Update the compiled jstz kernel used in CI tests
assets/jstz assets/jstz.checksum::
	@curl -L -o $@ $(RISCV_PVM_REPO)/assets/jstz
	@curl -L -o $@.checksum $(RISCV_PVM_REPO)/assets/jstz.checksum

# Update the compiled Etherlink kernel used in CI tests
assets/etherlink assets/etherlink.checksum::
	@curl -L -o $@ $(RISCV_PVM_REPO)/assets/etherlink
	@curl -L -o $@.checksum $(RISCV_PVM_REPO)/assets/etherlink.checksum

# Update the serialised proof for the initial tick of jstz used in the CI tests
assets/jstz_proof_initial:
	@curl -L -o $@ $(RISCV_PVM_REPO)/src/riscv/lib/tests/expected/jstz/proof_initial

# Update the serialised proof for the initial tick of Etherlink used in the CI tests
assets/etherlink_proof_initial:
	@curl -L -o $@ $(RISCV_PVM_REPO)/src/riscv/lib/tests/expected/etherlink/proof_initial

.PHONY: build-deps
build-deps:
	# Ensure the toolchain is installed.
	# The second command triggers installation for Rustup 1.28+.
	@rustup show active-toolchain || rustup toolchain install
	@rustup component add rustfmt clippy

ifneq ($(NATIVE_TARGET),)
	@rustup target add $(NATIVE_TARGET)
endif

	# Install Nightly for formatting with its Rustfmt, and running Miri
	@rustup toolchain install $(NIGHTLY_VERSION) -c rustfmt miri rust-src

	# Iterate through all the toolchains. 'rustup show' (before Rustup 1.28) and
	# 'rustup toolchain install' (Rustup 1.28+) will install the toolchain.
	@find . -iname 'rust-toolchain*' -execdir sh -c "rustup show active-toolchain || rustup toolchain install" \; 2>/dev/null

	# Coverage deps
	@./scripts/isa-suite-coverage.sh -d

.PHONY: check
check:
	@exec scripts/format.sh --check
	@taplo format --check $(shell ls **/Cargo.toml)

	@cargo update --workspace --locked
	@cargo check --workspace --all-targets $(CHECK_FLAGS) $(EXTRA_FLAGS)
	@cargo clippy --workspace --all-targets $(CHECK_FLAGS) $(EXTRA_FLAGS) -- --deny warnings
	@cargo doc --document-private-items --no-deps $(CHECK_FLAGS) $(EXTRA_FLAGS)

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
