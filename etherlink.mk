# SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
# SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

KERNELS=evm_kernel.wasm
EVM_DIR=etherlink/kernel_evm
EVM_KERNEL_PREIMAGES=_evm_installer_preimages
EVM_UNSTRIPPED_KERNEL_PREIMAGES=_evm_unstripped_installer_preimages

NATIVE_TARGET ?=
ifneq ($(NATIVE_TARGET),)
NATIVE_OPT := --target "$(NATIVE_TARGET)"
endif

.PHONY: all
all: build-dev-deps check test build

.PHONY: evm-execution
evm-execution:
	@$(MAKE) -C ${EVM_DIR} build-evm-execution

.PHONY: evm-evaluation-assessor
evm-evaluation-assessor:
	@$(MAKE) -C ${EVM_DIR} build-evm-evaluation
	@cp ${EVM_DIR}/target/release/evm-evaluation $@

evm_kernel_unstripped.wasm::
	@$(MAKE) -C ${EVM_DIR} build
	@cp etherlink/kernel_evm/target/wasm32-unknown-unknown/release/evm_kernel.wasm $@

evm_kernel.wasm:: evm_kernel_unstripped.wasm
	@cp evm_kernel_unstripped.wasm $@
	@wasm-strip $@

.PHONY: kernel_sdk
kernel_sdk:
	@$(MAKE) -f kernels.mk kernel_sdk

evm_installer.wasm:: kernel_sdk evm_kernel.wasm
ifdef EVM_CONFIG
	$(eval CONFIG := --setup-file ${EVM_CONFIG})
endif
ifeq (${DISPLAY_ROOT_HASH}, true)
	$(eval DISPLAY_CONFIG := --display-root-hash)
endif
	@./smart-rollup-installer get-reveal-installer \
	--upgrade-to evm_kernel.wasm \
	--preimages-dir ${EVM_KERNEL_PREIMAGES} \
	--output $@ \
	${CONFIG} \
	${DISPLAY_CONFIG}

evm_unstripped_installer.wasm:: kernel_sdk evm_kernel_unstripped.wasm
ifdef EVM_CONFIG
	$(eval CONFIG := --setup-file ${EVM_CONFIG})
endif
ifeq (${DISPLAY_ROOT_HASH}, true)
	$(eval DISPLAY_CONFIG := --display-root-hash)
endif
	@./smart-rollup-installer get-reveal-installer \
	--upgrade-to evm_kernel_unstripped.wasm \
	--preimages-dir ${EVM_UNSTRIPPED_KERNEL_PREIMAGES} \
	--output $@ \
	${CONFIG} \
	${DISPLAY_CONFIG}

evm_benchmark_kernel.wasm::
ifdef BENCHMARK_FEATURES
	$(eval EVM_KERNEL_FEATURES := ${BENCHMARK_FEATURES})
else
	$(eval EVM_KERNEL_FEATURES := benchmark-full)
endif
	@${MAKE} -f etherlink.mk \
	EVM_KERNEL_FEATURES=${EVM_KERNEL_FEATURES} \
	evm_kernel_unstripped.wasm
	cp evm_kernel_unstripped.wasm $@

evm_installer_dev.wasm::
	@${MAKE} -f etherlink.mk EVM_CONFIG=etherlink/config/dev.yaml evm_installer.wasm

.PHONY: build
build: ${KERNELS} evm-evaluation-assessor evm-execution kernel_sdk

.PHONY: clang-supports-wasm
clang-supports-wasm:
	./scripts/kernels_check_clang.sh

.PHONY: build-dev-deps
build-dev-deps: clang-supports-wasm build-deps
	@$(MAKE) -C ${EVM_DIR} build-dev-deps

.PHONY: build-deps
build-deps:
	@$(MAKE) -C ${EVM_DIR} build-deps

.PHONY: test
test:
	@$(MAKE) -C ${EVM_DIR} test

.PHONY: check
check: build-dev-deps
	@$(MAKE) -C ${EVM_DIR} check

	# Check format of all crates
	@exec scripts/check-format-rust.sh

.PHONY: clean
clean:
	@$(MAKE) -f kernels.mk clean
	@rm -f evm_kernel_unstripped.wasm evm_kernel.wasm evm_installer.wasm evm_unstripped_installer.wasm evm_installer.wasm evm_installer_dev.wasm evm_benchmark_kernel.wasm sequencer.wasm
	@$(MAKE) -C ${EVM_DIR} clean
	@rm -rf ${EVM_KERNEL_PREIMAGES} ${EVM_UNSTRIPPED_KERNEL_PREIMAGES}

sequencer.wasm::
	@${MAKE} -f etherlink.mk EVM_CONFIG=etherlink/config/sequencer.yaml evm_installer.wasm
	@cp evm_installer.wasm sequencer.wasm

octez-dsn-node:
	@cd etherlink/bin_dsn_node; cargo build --release $(NATIVE_OPT)
	@cp etherlink/bin_dsn_node/target/$(NATIVE_TARGET)/release/dsn-node octez-dsn-node
