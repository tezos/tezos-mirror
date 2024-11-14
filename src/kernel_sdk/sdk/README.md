SDK for Tezos Smart Rollups.

To learn more about how Smart Rollups work in Tezos, see the
[Smart Rollup Documentation](https://tezos.gitlab.io/alpha/smart_rollups.html).

The purpose of this SDK is to make writing Smart Rollup kernels in Rust simpler.

To learn about the changes in the current version of the SDK, see the
[CHANGELOG](https://gitlab.com/tezos/tezos/-/blob/master/src/kernel_sdk/CHANGES.md).

## Smart Rollup Kernels

A kernel is a 32bit *WebAssembly* program that runs on a Smart Rollup. It decides how the Rollup
handles input messages, updates the Rollup state, and when to output messages targetting Layer 1.

While any programming-language with WebAssembly-compilation support *could* be used for writing
a Rollup kernel, Rust is an excellent fit due to first-class WASM support, deterministic runtime,
and safe memory management.

## Setting-up Rust

[rustup](https://rustup.rs/) is the standard way to get Rust. Once `rustup` is installed, enable
WASM as a compilation target with:

```shell
rustup target add wasm32-unknown-unknown
```

Rust also has a `wasm64-unknown-unknown` compilation target. This target is **not** compatible
with Tezos Smart Rollups, which only provide a 32bit address space.

## Installing Clang

In order to build the Rust SDK, `clang >= 11` is required in addition to Rust. This can be installed
through your favourite package manager.

On MacOS, LLVM and the WebAssembly Binary Toolkit (WABT) can be installed
through homebrew:

```shell
brew install llvm
brew install wabt
LLVM_PATH=$(brew --prefix llvm)
export AR="${LLVM_PATH}/bin/llvm-ar"
export CC="${LLVM_PATH}/bin/clang"
```

On Linux, often the default `CC` is gcc, which is not supported for kernel builds. Ensure clang is being used:

```shell
export CC=clang
```

## Features

| Feature         | Enables                             | About                                             |
|-----------------|-------------------------------------|---------------------------------------------------|
| `std`           | `alloc`                             | Disable for `#![no_std]` integration              |
| `alloc`         |                                     | Enables methods/types requiring `alloc` crate     |
| `panic-hook`    |                                     | Print panics to debug log and abort               |
| `dlmalloc`      |                                     | Enables `dlmalloc` as default allocator           |
| `crypto`        | `tezos_crypto_rs`                   | Integration with `tezos_crypto_rs` types          |
| `bls`           | `tezos_crypto_rs/bls`               | Dac Certificate signature verification            |
| `data-encoding` | `tezos_data_encoding`               | Integration with `tezos_data_encoding` traits     |
| `testing`       | `crypto`, `tezos_smart_rollup_mock` | Enables `MockHost` for writing tests              |
| `extra`         | `alloc`, `std`, `utils`             | Additional tooling not targetted at kernels       |
| `utils`         |                                     | Interaction with other tooling outside of the SDK |

## Usage

The following `Cargo.toml` file can be used to set up development with the Kernel SDK:

```toml
[package]
name = "kernel"
version = "0.1.0"
edition = "2021"
rust-version = "1.71.1"

[lib]
crate-type = ["cdylib", "rlib"]

[dependencies]
tezos-smart-rollup = "0.2.2"
tezos_data_encoding = "0.5"
tezos_crypto_rs = { version = "0.5", default-features = false }
nom = "7.1"

[dev-dependencies]
tezos-smart-rollup = { version = "0.2.0", features = ["testing"] }
```

Note that the `cdylib` crate type is required to enable compilation to wasm.

The following `lib.rs` file could then be used to get started with a 'hello kernel'.
This kernel will run once per inbox level.

```rust
use tezos_smart_rollup::prelude::*;

#[entrypoint::main]
fn hello_kernel(host: &mut impl Runtime) {
  debug_msg!(host, "Hello, kernel!\n");
}
```

With those two files saved to `Cargo.toml` & `src/lib.rs`, you can compile the kernel:

```shell
cargo build --release --target wasm32-unknown-unknown
cp target/wasm32-unknown-unknown/release/kernel.wasm .
```

Often, large `.wasm` files are produced. The size of these can be significantly reduced using [wasm-strip](https://github.com/WebAssembly/wabt), which will remove items such as debugging symbols & metadata from the binary, not required for execution on Smart Rollups:

```shell
wasm-strip kernel.wasm
```

You can test this kernel by using the [`octez-smart-rollup-wasm-debugger`](https://tezos.gitlab.io/alpha/smart_rollups.html#testing-your-kernel).

```shell
# Create an empty inputs.json file - the 'hello world' kernel does not read inputs.
echo '[[], []]' > inputs.json

# Run the kernel:
octez-smart-rollup-wasm-debugger --kernel kernel.wasm --inputs inputs.json
```

Once in the debugger, you can run the following commands to test the kernel:

```shell
> load inputs
Loaded 0 inputs at level 0
> step kernel_run
Hello, kernel!
Evaluation took 11000000000 ticks so far
Status: Waiting for input
Internal_status: Collect
> load inputs
Loaded 0 inputs at level 1
> step kernel_run
Hello, kernel!
Evaluation took 11000000000 ticks so far
Status: Waiting for input
Internal_status: Collect
```

As you can see, on each level, the kernel prints `Hello, kernel!` to the debug log.

## Unit-testing

To learn about writing unit tests against a kernel, see [`MockHost`].

[`MockHost`]: crate::testing::prelude
