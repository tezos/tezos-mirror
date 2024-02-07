# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
# SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
# SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

KERNELS=tx_kernel.wasm tx_kernel_dal.wasm dal_echo_kernel.wasm risc-v-dummy risc-v-dummy.elf jstz
SDK_DIR=src/kernel_sdk
RISC_V_DIR=src/risc_v
RISC_V_DUMMY_DIR=src/risc_v/dummy_kernel
RISC_V_JSTZ_DIR=src/risc_v/jstz
RISC_V_TESTS_DIR=src/risc_v/tests
DEMO_DIR=src/kernel_tx_demo

.PHONY: all
all: build-dev-deps check test build
all: build-dev-deps check test build

.PHONY: kernel_sdk
kernel_sdk:
	@make -C src/kernel_sdk build
	@cp src/kernel_sdk/target/$(NATIVE_TARGET)/release/smart-rollup-installer .

.PHONY: risc-v-sandbox
risc-v-sandbox:
	@make -C $(RISC_V_DIR) build-sandbox
	@ln -f $(RISC_V_DIR)/target/$(NATIVE_TARGET)/release/risc-v-sandbox $@

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

.PHONY: risc-v-dummy.elf
risc-v-dummy.elf:
	@make -C ${RISC_V_DUMMY_DIR} build
	@ln -f ${RISC_V_DUMMY_DIR}/target/riscv64gc-unknown-hermit/release/risc-v-dummy $@

.PHONY: risc-v-dummy
risc-v-dummy:
	@make -C ${RISC_V_DUMMY_DIR} build

.PHONY: jstz
jstz:
	@make -C ${RISC_V_JSTZ_DIR} build

.PHONY: risc-v-tests
risc-v-tests:
	@make -C ${RISC_V_TESTS_DIR} build

.PHONY: build
build: ${KERNELS} kernel_sdk risc-v-sandbox risc-v-tests

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
	@make -C ${RISC_V_DIR} test
	@make -C ${DEMO_DIR} test

.PHONY: check
check: build-dev-deps
	@make -C ${SDK_DIR} check
	@make -C ${RISC_V_DIR} check
	@make -C ${DEMO_DIR} check
	@make -C ${RISC_V_TESTS_DIR} check

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
	@make -C ${RISC_V_DIR} clean
	@make -C ${DEMO_DIR} clean
	@rm -f risc-v-sandbox
	@make -C ${RISC_V_TESTS_DIR} clean
