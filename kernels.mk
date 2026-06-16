# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
# SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
# SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

RISCV_KERNELS_DIR=src/lib_riscv/kernels
RISCV_KERNELS=riscv-echo
RISCV_KERNELS_ARTIFACTS=$(RISCV_KERNELS) $(addsuffix .checksum, $(RISCV_KERNELS))

KERNELS=tx_kernel.wasm tx_kernel_dal.wasm dal_echo_kernel.wasm dal_echo_kernel_bandwidth.wasm
SDK_DIR=src/kernel_sdk
DEMO_DIR=src/kernel_tx_demo

.PHONY: all
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

dal_echo_kernel_bandwidth.wasm:
	@make -C src/kernel_dal_echo dal_echo_kernel_bandwidth
	@cp src/kernel_dal_echo/target/wasm32-unknown-unknown/release/dal_echo_kernel_bandwidth.wasm $@
	@wasm-strip $@

riscv-echo riscv-echo.checksum:
	@ make -C ${RISCV_KERNELS_DIR} build
	@mv ${RISCV_KERNELS_DIR}/$@ $@
	@mv ${RISCV_KERNELS_DIR}/$@.checksum $@.checksum

.PHONY: build
build: ${KERNELS} ${RISCV_KERNELS} kernel_sdk

.PHONY: clang-supports-wasm
clang-supports-wasm:
	./scripts/kernels_check_clang.sh

.PHONY: build-dev-deps
build-dev-deps: clang-supports-wasm build-deps
	@make -C ${SDK_DIR} build-dev-deps
	@make -C ${DEMO_DIR} build-dev-deps
	@make -C ${RISCV_KERNELS_DIR} build-dev-deps

.PHONY: build-deps
build-deps:
	@make -C ${SDK_DIR} build-deps
	@make -C ${DEMO_DIR} build-deps

	# Iterate through all the toolchains. 'rustup show' will install the
	# toolchain in addition to showing toolchain information.
	@find ${SDK_DIR} ${DEMO_DIR} -iname 'rust-toolchain*' -execdir rustup show active-toolchain \; 2>/dev/null

.PHONY: test
test:
	@make -C ${SDK_DIR} test
	@make -C ${DEMO_DIR} test

.PHONY: check
check: build-dev-deps
	@make -C ${SDK_DIR} check
	@make -C ${DEMO_DIR} check
	@make -C ${RISCV_KERNELS_DIR} check

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
	@rm -f ${RISCV_KERNELS_ARTIFACTS}
	@make -C ${SDK_DIR} clean
	@make -C ${DEMO_DIR} clean
	@make -C ${RISCV_KERNELS_DIR} clean
	@rm -f smart-rollup-installer tx-demo-collector
