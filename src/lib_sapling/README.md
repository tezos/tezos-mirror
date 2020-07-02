This OCaml library implements the Sapling protocol for privacy-preserving
transactions as described in its
[specification](https://github.com/zcash/zips/blob/master/protocol/sapling.pdf),
version 2020.1.2.

A large part of the functionalities are implemented by the
[librustzcash library](https://github.com/zcash/librustzcash) from the ZCash
project.
This library provides bindings to `librustzcash` and implements the
needed data structures to use the library.
Additionally it provides some facilities to forge transactions.

# Rustzcash

The Rust library exports a C compatible interface in
`librustzcash/src/rustzcash.rs` and
`librustzcash/include/librustzcash.h` that is used by the ZCash C++
node and client.
The files `rustzcash.ml{,i}` simply bind this C interface.
The binding can't be used alone to test the library as a number of
data structures are left to be implemented to the user of library.

Additionally the ZCash parameters are necessary to create and verify
proofs.
We assume that the library and the parameters are installed as part of
the usual `make build-deps`.

# Core

The file `core.ml` contains a more high level presentation of the
Sapling protocol with respect to the low level binding.
Core is organized in several modules that are exposed through a
limited signature for validators and a more complete signature for
clients.

# Storage

All the data structures are implemented in `storage.ml`, including the
incremental Merkle tree, the nullifier set, the root bounded list and
the ciphertexts list.

# Example

The file `test/example.ml` contains a simplified implementation of a
client and validator using the library.
