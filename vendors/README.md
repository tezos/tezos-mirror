# Vendored libraries

## ledgerwallet-tezos

- Library used by the Octez client and signer to interact with the Tezos apps
  of Ledger hardware wallets (see `src/lib_signer_backends/unix/ledger.available.ml`).
- Copied from: [ocaml-ledger-wallet](https://github.com/vbmithr/ocaml-ledger-wallet)
  0.4.1 (`src/ledgerwallet_tezos.ml{,i}` and `LICENSE.md`), ISC license.
- Reason for vendoring: the `ledgerwallet-tezos` opam package was archived from
  opam-repository (`x-maintenance-intent: [ "(none)" ]`), which prevents us from
  bumping opam-repository. It still depends on the `ledgerwallet` opam package,
  which is still in opam-repository.

### Differences with the initial library

- The source files are unchanged.
- `src/dune` only defines the `ledgerwallet-tezos` library, and marks it as
  `optional`, so that Octez builds without Ledger support when `ledgerwallet`
  is not installed.
- `ledgerwallet-tezos.opam` has no test dependencies (tests are not vendored).
