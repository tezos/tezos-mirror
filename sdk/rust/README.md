# TezEdge

---

Utility crates, forked from [tezedge](github.com/tezedge/tezedge), used as part of the `Kernel SDK` for tezos smart rollups.

Namely:
- [tezos_crypto_rs](./crypto/README.md) 
- [tezos_encoding](./tezos-encoding/README.md) 
- [tezos_encoding_derive](./tezos-encoding-derive/README.md)

## Setup

The following prerequisites are required:

- rust 1.60, with the `wasm32-unknown-unknown` target.
- clang - tested with `v11`.

> If running on MacOS - you will need to install llvm with brew, and ensure the brew-install is available in your path, rather than the default installation.

You should then be able to run:

```shell
cargo build
cargo test
cargo build --target wasm32-unknown-unknown --no-default-features
```
