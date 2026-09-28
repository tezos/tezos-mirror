### M.I.R - Michelson In Rust

This repo hosts the Rust implementation of the typechecker and interpreter for
Michelson smart contract language.

#### Building

You need `cargo` to build this project. You can use the following
command to build the project.

`cargo build`

To build using `wasm` target, just extend the previous command:

`cargo build --target wasm32-unknown-unknown`

Note that `clang`, `llvm`, and `wabt` are required for this target. See [src/kernel_sdk/sdk/README.md](../../../src/kernel_sdk/sdk/README.md) for installation instructions.

#### Testing

You can run the included tests by the following command.

`cargo test`

Some tests print gas consumption information (in addition to testing it), but `cargo test` omits output from successful tests by default. To see it, run

`cargo test -- --show-output`

#### Running examples

The repository includes some simple examples in the `examples/` directory. To
run them, you can use

`cargo run --example example_name`

Add the `--release` flag to build with optimization.

For example:

`cargo run --example lazy_parse --release`

Note examples are automatically built (but not run) by `cargo test`.

#### Cargo Features

| Feature | Default | Description |
|---------|---------|-------------|
| `text-parser` | yes | Enables the Michelson text parser and lexer (`logos`, `lalrpop-util`). Disable this to exclude the parser/lexer from the build (e.g. for WASM kernel deployments). |
| `bls` | yes | Enables BLS12-381 cryptographic operations via `blst`. |
| `allow_lazy_storage_transfer` | yes | Permits transfer of lazy storage (big maps / sapling states) in Michelson. |
| `tickets` | yes | Enables the `ticket` type and TICKET/READ_TICKET/SPLIT_TICKET/JOIN_TICKETS instructions. |

To build without the text parser (as the Tezos X kernel does):

```
cargo build --no-default-features --features allow_lazy_storage_transfer
```

When `text-parser` is disabled, `Parser::new()` and the `Parser::arena` field
remain available (needed by the interpreter's arena allocator), but
`Parser::parse` and `Parser::parse_top_level` are not compiled in. The `tzt_runner`
and `typecheck_script` binaries require `text-parser` and will not build without it.
