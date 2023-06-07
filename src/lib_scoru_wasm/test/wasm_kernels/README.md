# About
This folder contains example test kernels, used for running `SCORU WASM` integration tests.

The test kernels have been built from [tezos/kernel](https://gitlab.com/trili/kernel.git), then manually edited to take into account a change in naming introduced by [this MR](https://gitlab.com/tezos/tezos/-/merge_requests/6914).

```terminal
wasm2wast ${OLD_KERNEL} -o tmp.wast
sed -i -e 's/kernel_next/kernel_run/g' -e 's/rollup_safe_core/smart_rollup_core/g' tmp.wast
wast2wasm tmp.wast -o {OLD_KERNEL}
```

# Available kernels
It is possible to build the test kernels manually, and verify that they are bit-for-bit identical.

## Prerequisites
You will need `docker`, `git` and `wasm-strip` installed, alongside either `bash` or `zsh`.
- `wasm-strip` is part of the [WebAssembly Binary Toolkit](https://github.com/WebAssembly/wabt).

Next, clone the *tezos/kernel* repository:
``` shell
git clone https://gitlab.com/tezos/kernel.git wasm_kernel
cd wasm_kernel
```
and then follow the instructions below for the required kernel.

## [unreachable.wasm](./unreachable.wasm)
The unreachable kernel performs a simple computation (addition) on each call to its `kernel_run` entrypoint but raises `Unreachable` before yielding. It makes no use of any *PVM host-capabilities*.

It is designed to be small enough to be able to originate directly within a boot sector, but also large enough to be
used with the *gather-floppies* mechanism, and aims to test the `Stuck` state of the PVM.

To build the `unreachable.wasm` kernel, run the following from the checked-out `tezos/kernel` repo:
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

## [computation.wasm](./computation.wasm)

The computation kernel is a simple program that does in-memory addition and
optionally dumps inputs it gets to the output buffer.

It can be reproduced in the kernel repository like so:

```
git checkout tx-kernel-vRAM
./scripts/build-unit-kernel.sh "test-kernel,wee_alloc"
mv test-kernel,wee_alloc.wasm computation.wasm
```

## [tx-kernel-no-verif.wasm](./tx-kernel-no-verif.wasm)

This kernel is TORU-style kernel for transferring tickets between accounts. The `no-verif` variant disables
bls signature-verification, which otherwise take too long in the slow-PVM to run in CI.

It can be reproduced in the kernel repository like so:

```
git checkout 69f69144764dcd59dcc1fd144bf6e8f707f0431e

cargo make wasm-tx-kernel-no-sig-verif
cp target/wasm32-unknown-unknown/release/kernel_core.wasm tx-kernel-no-sig-verif.wasm
wasm-strip tx-kernel-no-sig-verif.wasm
```
