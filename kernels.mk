# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
# SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
# SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

KERNELS=tx_kernel.wasm tx_kernel_dal.wasm dal_echo_kernel.wasm
SDK_DIR=src/kernel_sdk
DEMO_DIR=src/kernel_tx_demo
RISC_V_DIR=src/risc_v

.PHONY: all
all: build-dev-deps check test build
all: build-dev-deps check test build

.PHONY: kernel_sdk
kernel_sdk:
	@make -C src/kernel_sdk build
	@cp src/kernel_sdk/target/$(NATIVE_TARGET)/release/smart-rollup-installer .

.PHONY: tx_demo_collector
tx_demo_collector:
	@make -C src/kernel_tx_demo build
	@cp src/kernel_tx_demo/target/$(NATIVE_TARGET)/release/tx-demo-collector .

tx_kernel.wasm: tx_demo_collector
	# TODO: https://gitlab.com/tezos/tezos/-/issues/6407
	# There is an issue where `wasm-strip` fails for `tx_kernel.wasm`.
	# Use `wasm-strip` once this issue is solved.
	@cp src/kernel_tx_demo/target/wasm32-unknown-unknown/release/tx_kernel.wasm $@

tx_kernel_dal.wasm: tx_demo_collector
	# TODO: https://gitlab.com/tezos/tezos/-/issues/6407
	# There is an issue where `wasm-strip` fails for `tx_kernel.wasm`.
	# Use `wasm-strip` once this issue is solved.
	@cp src/kernel_tx_demo/target/wasm32-unknown-unknown/release/tx_kernel_dal.wasm $@

dal_echo_kernel.wasm:
	@make -C src/kernel_dal_echo build
	@cp src/kernel_dal_echo/target/wasm32-unknown-unknown/release/dal_echo_kernel.wasm $@
	@wasm-strip $@


.PHONY: build
build: ${KERNELS} kernel_sdk
	@make -C ${RISC_V_DIR} build

.PHONY: clang-supports-wasm
clang-supports-wasm:
	./scripts/kernels_check_clang.sh

.PHONY: build-dev-deps
build-dev-deps: clang-supports-wasm build-deps
	@make -C ${SDK_DIR} build-dev-deps
	@make -C ${DEMO_DIR} build-dev-deps

.PHONY: build-deps
build-deps:
	@make -C ${SDK_DIR} build-deps
	@make -C ${DEMO_DIR} build-deps
	@make -C ${RISC_V_DIR} build-deps

	# Iterate through all the toolchains. 'rustup show' will install the
	# toolchain in addition to showing toolchain information.
	@find src -iname 'rust-toolchain*' -execdir rustup show active-toolchain \; 2>/dev/null

.PHONY: test
test:
	@make -C ${SDK_DIR} test
	@make -C ${DEMO_DIR} test
	@make -C ${RISC_V_DIR} test

.PHONY: check
check: build-dev-deps
	@make -C ${SDK_DIR} check
	@make -C ${DEMO_DIR} check
	@make -C ${RISC_V_DIR} check

	# Check formatting of all crates.
	@exec scripts/check-format-rust.sh

.PHONY: publish-sdk-deps
publish-sdk-deps: build-deps
	@make -C ${SDK_DIR} publish-deps

.PHONY: publish-sdk
publish-sdk:
	@make -C ${SDK_DIR} publish

.PHONY: clean
clean:
	@rm -f ${KERNELS}
	@make -C ${SDK_DIR} clean
	@make -C ${DEMO_DIR} clean
	@make -C ${RISC_V_DIR} clean
