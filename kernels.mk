# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
# SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
# SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

KERNELS=evm_kernel.wasm sequenced_kernel.wasm tx_kernel.wasm tx_kernel_dal.wasm dal_echo_kernel.wasm risc-v-dummy.elf
SDK_DIR=src/kernel_sdk
RISC_V_SANDBOX_DIR=src/risc_v/sandbox
RISC_V_INTERPRETER_DIR=src/risc_v/interpreter
RISC_V_DUMMY_DIR=src/risc_v/dummy_kernel
RISC_V_TESTS_DIR=src/risc_v/tests
EVM_DIR=src/kernel_evm
DEMO_DIR=src/kernel_tx_demo
SEQUENCER_DIR=src/kernel_sequencer
EVM_KERNEL_PREIMAGES=_evm_installer_preimages
EVM_UNSTRIPPED_KERNEL_PREIMAGES=_evm_unstripped_installer_preimages

.PHONY: all
all: build-dev-deps check test build
all: build-dev-deps check test build

.PHONY: kernel_sdk
kernel_sdk:
	@make -C src/kernel_sdk build
	@cp src/kernel_sdk/target/$(NATIVE_TARGET)/release/smart-rollup-installer .

.PHONY: risc-v-sandbox
risc-v-sandbox:
	@make -C $(RISC_V_SANDBOX_DIR) build
	@ln -f $(RISC_V_SANDBOX_DIR)/target/$(NATIVE_TARGET)/release/risc-v-sandbox $@

.PHONY: risc-v-interpreter
risc-v-interpreter:
	@make -C $(RISC_V_INTERPRETER_DIR) build

.PHONY: evm-execution
evm-execution:
	@make -C src/kernel_evm build-evm-execution

.PHONY: evm-evaluation-assessor
evm-evaluation-assessor:
	@make -C src/kernel_evm build-evm-evaluation
	@cp src/kernel_evm/target/release/evm-evaluation $@

evm_kernel_unstripped.wasm::
	@make -C src/kernel_evm build
	@cp src/kernel_evm/target/wasm32-unknown-unknown/release/evm_kernel.wasm $@

evm_kernel.wasm:: evm_kernel_unstripped.wasm
	@cp evm_kernel_unstripped.wasm $@
	@wasm-strip $@

evm_installer.wasm:: kernel_sdk evm_kernel.wasm
ifdef EVM_CONFIG
	$(eval CONFIG := --setup-file ${EVM_CONFIG})
endif
	@./smart-rollup-installer get-reveal-installer \
	--upgrade-to evm_kernel.wasm \
	--preimages-dir ${EVM_KERNEL_PREIMAGES} \
	--output $@ \
	${CONFIG}

evm_unstripped_installer.wasm:: kernel_sdk evm_kernel_unstripped.wasm
ifdef EVM_CONFIG
	$(eval CONFIG := --setup-file ${EVM_CONFIG})
endif
	@./smart-rollup-installer get-reveal-installer \
	--upgrade-to evm_kernel_unstripped.wasm \
	--preimages-dir ${EVM_UNSTRIPPED_KERNEL_PREIMAGES} \
	--output $@ \
	${CONFIG}

evm_benchmark_installer.wasm::
	@${MAKE} -f kernels.mk \
	EVM_CONFIG=src/kernel_evm/config/benchmarking.yaml \
	EVM_KERNEL_FEATURES=benchmark,debug \
	evm_unstripped_installer.wasm
	cp evm_unstripped_installer.wasm $@

evm_installer_dev.wasm::
	@${MAKE} -f kernels.mk EVM_CONFIG=src/kernel_evm/config/dev.yaml evm_installer.wasm

sequencer.wasm::
	@${MAKE} -f kernels.mk EVM_CONFIG=src/kernel_evm/config/sequencer.yaml evm_installer.wasm
	@cp evm_installer.wasm sequencer.wasm

sequenced_kernel.wasm:
	@make -C src/kernel_sequencer build
	@cp src/kernel_sequencer/target/wasm32-unknown-unknown/release/examples/sequenced_kernel.wasm $@
	@wasm-strip $@

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
	@ln -f ${RISC_V_DUMMY_DIR}/target/riscv64gc-unknown-none-elf/release/risc-v-dummy $@

.PHONY: risc-v-tests
risc-v-tests:
	@make -C ${RISC_V_TESTS_DIR} build

.PHONY: build
build: ${KERNELS} evm-evaluation-assessor evm-execution kernel_sdk risc-v-sandbox risc-v-interpreter risc-v-tests

.PHONY: build-dev-deps
build-dev-deps: build-deps
	@make -C ${SDK_DIR} build-dev-deps
	@make -C ${EVM_DIR} build-dev-deps
	@make -C ${SEQUENCER_DIR} build-dev-deps
	@make -C ${DEMO_DIR} build-dev-deps

.PHONY: build-deps
build-deps:
	@make -C ${SDK_DIR} build-deps
	@make -C ${RISC_V_SANDBOX_DIR} build-deps
	@make -C ${EVM_DIR} build-deps
	@make -C ${SEQUENCER_DIR} build-deps
	@make -C ${DEMO_DIR} build-deps

	# Iterate through all the toolchains. 'rustup show' will install the
	# toolchain in addition to showing toolchain information.
	@find src -iname 'rust-toolchain*' -execdir rustup show active-toolchain \; 2>/dev/null

.PHONY: test
test:
	@make -C ${SDK_DIR} test
	@make -C ${RISC_V_SANDBOX_DIR} test
	@make -C ${RISC_V_INTERPRETER_DIR} test
	@make -C ${RISC_V_DUMMY_DIR} test
	@make -C ${EVM_DIR} test
	@make -C ${SEQUENCER_DIR} test
	@make -C ${DEMO_DIR} test

.PHONY: check
check: build-dev-deps
	@make -C ${SDK_DIR} check
	@make -C ${RISC_V_SANDBOX_DIR} check
	@make -C ${RISC_V_INTERPRETER_DIR} check
	@make -C ${RISC_V_DUMMY_DIR} check
	@make -C ${EVM_DIR} check
	@make -C ${SEQUENCER_DIR} check
	@make -C ${DEMO_DIR} check
	@make -C ${RISC_V_TESTS_DIR} check

	# Check formatting of all crates.
	@find src -iname Cargo.lock -execdir cargo fmt --check \;

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
	@make -C ${RISC_V_SANDBOX_DIR} clean
	@make -C ${RISC_V_INTERPRETER_DIR} clean
	@make -C ${RISC_V_DUMMY_DIR} clean
	@make -C ${EVM_DIR} clean
	@make -C ${SEQUENCER_DIR} clean
	@make -C ${DEMO_DIR} clean
	@rm -rf ${EVM_KERNEL_PREIMAGES}
	@rm -f risc-v-sandbox
	@make -C ${RISC_V_TESTS_DIR} clean
