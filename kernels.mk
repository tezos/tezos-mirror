KERNELS = kernel_sdk evm_mockup_kernel.wasm

kernel_sdk:
	@make -C src/kernel_sdk

evm_mockup_kernel.wasm::
	@make -C src/kernel_evm_mockup
	@cp src/kernel_evm_mockup/target/wasm32-unknown-unknown/release/evm_mockup_kernel.wasm $@
	@wasm-strip $@

build-deps:
	@make -C src/kernel_sdk build-deps
	@make -C src/kernel_evm_mockup build-deps

.PHONY: build-kernels
build-kernels: ${KERNELS}
