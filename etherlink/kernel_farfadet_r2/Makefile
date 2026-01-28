# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
# SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
# SPDX-FileCopyrightText: 2023-2025 Functori <contact@functori.com>
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
ifndef EVM_KERNEL_SKIP_BYTECODE
	@make -C revm/contracts/predeployed bytecode
endif
	@cargo build --target wasm32-unknown-unknown --release ${EXCLUDE} ${FEATURES}

.PHONY: build-debug
build-debug:
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
	@cargo build --profile release-with-debug --target wasm32-unknown-unknown ${EXCLUDE} ${FEATURES}

.PHONY: build-evm-evaluation
build-evm-evaluation:
ifdef EVM_EVALUATION_FEATURES
	@cargo build --features ${EVM_EVALUATION_FEATURES} --release --package evm-evaluation
else
	@cargo build --release --package evm-evaluation
endif

.PHONY: build-revm-evaluation
build-revm-evaluation:
ifdef EVM_EVALUATION_FEATURES
	@cargo build --features ${EVM_EVALUATION_FEATURES} --release --package revm-evaluation
else
	@cargo build --release --package revm-evaluation
endif

.PHONY: build-revm
build-revm:
	@cargo build --target wasm32-unknown-unknown --package revm-etherlink

.PHONY: build-deps
build-deps:
	# 'rustup show' will install the toolchain in addition to showing
	# toolchain information.
	@rustup show active-toolchain 2> /dev/null
	@rustup target add wasm32-unknown-unknown

.PHONY: build-r2-deps
build-r2-deps: build-deps
	@rustup component add rustfmt clippy

.PHONY: test
test:
# Setting RUST_MIN_STACK is needed otherwise some tests will panic by reaching
# Rust's max stack size.
	@RUST_MIN_STACK=104857600 RUST_TEST_THREADS=1 cargo test --features testing ${TESTNAME}

.PHONY: check
check:
	@cargo update --workspace --locked
	@cargo clippy --all-targets --features testing -- --deny warnings

.PHONY: check-all
check-all: check
	@cargo fmt --check

.PHONY: clean
clean:
	@cargo clean
