### M.I.R - Michelson In Rust

This repo hosts the Rust implementation of the typechecker and interpreter for
Michelson smart contract language.

#### Building

You need `cargo` to build this project. You can use the following
command to build the project.

`cargo build`

To build using `wasm` target, just extend the previous command:

`cargo build --target wasm32-unknown-unknown`

Note that `clang`, `llvm`, and `wabt` are required for this target. See [src/kernel_sdk/sdk/README.md](../../src/kernel_sdk/sdk/README.md) for installation instructions.

#### Testing

You can run the included tests by the following command.

`cargo test`

Some tests print gas consumption information (in addition to testing it), but `cargo test` omits output from successful tests by default. To see it, run

`cargo test -- --show-output`
