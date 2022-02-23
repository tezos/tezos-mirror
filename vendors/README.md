# Vendored libraries

## Cryptography

### ocaml-bls12-381
- Provides BLS12-381 fields, curves and pairing. The UNIX version relies on the Rust
  implementation [bls12_381](https://github.com/zkcrypto/bls12_381).
- Cloned from: [ocaml-bls12-381](https://gitlab.com/dannywillems/ocaml-bls12-381).
- As there is
  no efficient way to integrate Rust codebase into OPAM yet, it has been decided
  to vendor the library until a solution is found.

#### Differences with the initial library

- `benchmark` directory has been removed

- For the Rust dependencies, Tezos is moving to a virtual OPAM package called
  [tezos-rust-libs](https://gitlab.com/tezos/tezos-rust-libs). As of version
  0.3.13 of ocaml-bls12-381, the dune stanzas for the UNIX version suppose the
  Rust static library and C headers are available in
  `OPAM_SWITCH_PREFIX/lib/rustc-bls12-381` and
  `OPAM_SWITCH_PREFIX/include/rustc-bls12-381`. The integration of the virtual
  OPAM package in Tezos does require a change in the dune configuration and
  replaces `rustc-bls12-381` by `tezos-rust-libs`


## Snoop

Some dependencies of `lib_benchmark` are placed in vendors instead of e.g.
directly
being put in lib_benchmark, because they could be useful to do more than just
benchmarks but are not mature enough to be upstreamed. See <https://gitlab.com/tezos/tezos/-/issues/1523>
for more details.

### pyml-plot
- Contains pyml-based OCaml-Python bindings to matplotlib.
- Cloned from: [pyml-plot](https://gitlab.com/igarnier/pyml-plot)
- Dependency to lib-benchmark.

#### Differences with the initial library
None

### benchmark-utils
- Contains no tests; just stubs to system calls so unclear what we would be testing.
- C stubs useful for benchmarking, pinning cpus, ...
- Cloned from: nowhere
- Dependency to lib-benchmark. Contains manually written OCaml-C bindings.

#### Differences with the initial library
None

## Misc

### flextesa-lib
- This library has no tests here; they are in the `tezos/flextesa` and not reproduced here.
- Flextesa was initially developed to test 3rd party tools (Kiln, Dapps, etc.).
  It was then integrated into Tezos to improve the testing infrastructure and
  later, its development was re-externalized to the `tezos/flextesa` repository
  to be more flexible. It is the base for the `tezos-sandbox` executable in
  `src/bin_sandbox` and all the test scenarios defined in that `dune` file.
- Cloned from: [flextesa](https://gitlab.com/tezos/flextesa), just the `src/lib`
  sub-directory.
- It is vendored because it
  depends on some Tezos libraries (e.g. `tezos-crypto`), and it obviously makes
  some assumptions on how Tezos works hence using `opam` would be too cumbersome
  w.r.t. to changes in the protocol or the shell.

#### Differences with the initial library

Changes to the vendored version are periodically proposed in the main repository
as merge-requests.

### ocaml-ledger-wallet
- Library used by `tezos-client` to interact with the Ledger family of hardware
  wallets.
- Cloned from: [ledgerwallet](https://opam.ocaml.org/packages/ledgerwallet/)
  and [ledgerwallet-tezos](https://opam.ocaml.org/packages/ledgerwallet-tezos/)
- Reason for vendoring: unknown

#### Differences with the initial library
Unknown

#### Differences with the initial library

The ocaml bindings are slowly evolving to serve Tezos.

The C code has not evolved much.
