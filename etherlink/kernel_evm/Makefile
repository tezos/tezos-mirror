# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
# SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
# SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

.PHONY: all
all: build test check

.PHONY: build
build:
ifdef EVM_KERNEL_FEATURES
	$(eval FEATURES := --features ${EVM_KERNEL_FEATURES})
endif
ifdef EXCLUDE_MEMBER
	$(eval EXCLUDE := --workspace --exclude ${EXCLUDE_MEMBER})
else
# By default evm-evaluation is excluded as it's an isolated component
# of the EVM workspace.
	$(eval EXCLUDE := --workspace --exclude evm-evaluation)
endif
	@cargo build --target wasm32-unknown-unknown --release ${EXCLUDE} ${FEATURES}

.PHONY: build-evm-execution
build-evm-execution:
	@cargo build --target wasm32-unknown-unknown --release --package evm-execution

.PHONY: build-evm-evaluation
build-evm-evaluation:
	@cargo build --release --package evm-evaluation

.PHONY: build-deps
build-deps:
	@rustup target add wasm32-unknown-unknown

.PHONY: build-dev-deps
build-dev-deps: build-deps
	@rustup component add rustfmt clippy

.PHONY: test
test:
	@cargo test --features testing ${TESTNAME}

.PHONY: check
check:
	@cargo clippy --all-targets --features testing -- --deny warnings

.PHONY: check-all
check-all: check
	@cargo fmt --check

.PHONY: clean
clean:
	@cargo clean
