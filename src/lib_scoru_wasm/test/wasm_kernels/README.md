# About
This folder contains example test kernels, used for running `SCORU WASM` integration tests.

The test kernels have been built from [trili/kernel](https://gitlab.com/trili/kernel.git).

# Available kernels
It is possible to build the test kernels manually, and verify that they are bit-for-bit identical.

## Prerequisites
You will need `docker`, `git` and `wasm-strip` installed, alongside either `bash` or `zsh`.
- `wasm-strip` is part of the [WebAssembly Binary Toolkit](https://github.com/WebAssembly/wabt).

Next, clone the *trili/kernel* repository:
``` shell
git clone https://gitlab.com/trili/kernel.git wasm_kernel
cd wasm_kernel
```
and then follow the instructions below for the required kernel.

## [unreachable.wasm](./unreachable.wasm)
The unreachable kernel performs a simple computation (addition) on each call to its `kernel_next` entrypoint but raises `Unreachable` before yielding. It makes no use of any *PVM host-capabilities*.

It is designed to be small enough to be able to originate directly within a boot sector, but also large enough to be
used with the *gather-floppies* mechanism, and aims to test the `Stuck` state of the PVM.

To build the `unreachable.wasm` kernel, run the following from the checked-out `trili/kernel` repo:
``` shell
git checkout 5440400a7340ff60af1166c875fce2f5ca0afda0

# Load the required rust toolchain dockerfile
source scripts/cargo-docker.sh

cargo build -p test_kernel --target wasm32-unknown-unknown --release \
    --no-default-features --features abort,wee_alloc

# computation_kernel.wasm is a 1.6M wasm binary.
cp target/wasm32-unknown-unknown/release/test_kernel.wasm unreachable.wasm

# Strips binary down to 9.7K
wasm-strip unreachable.wasm
```

## [test-write-debug.wasm](./test-write-debug.wasm)
This kernel is designed to call the `write_debug` host function, which is a no-op in the PVM.

It may be originated directly within a boot sector.

To build the `test-write-debug.wasm` kernel, run the following from the checked-out `trili/kernel` repo:
```shell
git checkout 0c98b17c4599d6f656312b16f17798406d491d77

./scripts/build-unit-kernel.sh "test-write-debug"
```

## [test-store-has.wasm](./test-store-has.wasm)
This kernel is designed to test the `store_has` host function behaviour, on different keys in *durable storage*.

It may be originated directly within a boot sector.

To build the `test-store-has.wasm` kernel, run the following from the checked-out `trili/kernel` repo:
```shell
git checkout 4788b8a882efbc9c19621ab43d617b2bdd5b1baf

./scripts/build-unit-kernel.sh "test-store-has"
```

## [test-store-list-size.wasm](./test-store-list-size.wasm)
This kernel is designed to test the `store_list_size` host function behaviour, on different keys in *durable storage*.

It may be originated directly within a boot sector.

To build the `test-store-list-size.wasm` kernel, run the following from the checked-out `trili/kernel` repo:
```shell
git checkout 0c98b17c4599d6f656312b16f17798406d491d77

./scripts/build-unit-kernel.sh "test-store-list-size"
```

## [test-store-delete.wasm](./test-store-delete.wasm)
This kernel is designed to test the `store_delete` host function behaviour, on different keys in *durable storage*.

It may be originated directly within a boot sector.

To build the `test-store-delete.wasm` kernel, run the following from the checked-out `trili/kernel` repo:
```shell
git checkout 0c98b17c4599d6f656312b16f17798406d491d77

./scripts/build-unit-kernel.sh "test-store-delete"
```
