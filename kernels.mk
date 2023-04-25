KERNELS = kernel_sdk evm_kernel.wasm kernel_sequencer

kernel_sdk:
	@make -C src/kernel_sdk
	@cp src/kernel_sdk/target/$(NATIVE_TARGET)/release/smart-rollup-installer .

evm_kernel.wasm::
	@make -C src/kernel_evm
	@cp src/kernel_evm/target/wasm32-unknown-unknown/release/evm_kernel.wasm $@
	@wasm-strip $@

kernel_sequencer:
	@make -C src/kernel_sequencer

build-deps:
	@make -C src/kernel_sdk build-deps
	@make -C src/kernel_evm build-deps
	@make -C src/kernel_sequencer build-deps

test-kernels:
	@make -C src/kernel_sdk test
	@make -C src/kernel_evm_mockup tests
	@make -C src/kernel_sequencer tests

.PHONY: build-kernels
build-kernels: ${KERNELS}

.PHONY: publish-sdk-deps
publish-sdk-deps: build-deps
	@make -C src/kernel_sdk publish-deps

.PHONY: publish-sdk
publish-sdk:
	@make -C src/kernel_sdk publish
