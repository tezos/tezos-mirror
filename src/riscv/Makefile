# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
# SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

.PHONY: all
all: build test check

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

.PHONY: build
build:
	@cargo build --release --workspace $(NATIVE_OPT) $(EXTRA_FLAGS)

# Update checked-in binaries
.PHONY: update-assets
update-assets: assets/riscv-dummy.elf assets/jstz

# Update the compiled dummy kernel used in CI tests
assets/riscv-dummy.elf assets/riscv-dummy.elf.checksum::
	@cargo install --config .cargo/config.toml --locked --git https://github.com/tezos/riscv-pvm.git --branch ole/github --target riscv64gc-unknown-linux-musl --root assets riscv-dummy
	@mv assets/bin/riscv-dummy $@
	@sha256sum $@ > $@.checksum

assets/jstz assets/jstz.checksum::
	@cargo install --config .cargo/config.toml --locked --git https://github.com/tezos/riscv-pvm.git --branch ole/github --target riscv64gc-unknown-linux-musl --root assets jstz
	@mv assets/bin/jstz $@
	@sha256sum $@ > $@.checksum

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
