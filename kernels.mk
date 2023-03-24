KERNELS = kernel_sdk evm_kernel.wasm

kernel_sdk:
	@make -C src/kernel_sdk
	@cp src/kernel_sdk/target/$(NATIVE_TARGET)/release/smart-rollup-installer .

evm_kernel.wasm::
	@make -C src/kernel_evm
	@cp src/kernel_evm/target/wasm32-unknown-unknown/release/evm_kernel.wasm $@
	@wasm-strip $@

build-deps:
	@make -C src/kernel_sdk build-deps
	@make -C src/kernel_evm build-deps

test-kernels:
	@make -C src/kernel_sdk test
	@make -C src/kernel_evm_mockup tests

.PHONY: build-kernels
build-kernels: ${KERNELS}
