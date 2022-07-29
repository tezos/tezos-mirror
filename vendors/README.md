# Vendored libraries

## ocaml-hidapi
  - used in ocaml-ledger-wallet
  - Reason for vendoring: pass custom link flags when building
    the library to produce static executables

### Differences with the initial library

    + (rule
    +   (targets static_library_flags.sexp)
    +   (action (with-stdout-to static_library_flags.sexp (system "[ 'static' = '%{profile}' ] && echo '(-lusb-1.0 -ludev)' || echo '()'"))))

## ocaml-ledger-wallet
- Library used by `tezos-client` to interact with the Ledger family of hardware
  wallets.
- Cloned from: [ledgerwallet](https://opam.ocaml.org/packages/ledgerwallet/)
  and [ledgerwallet-tezos](https://opam.ocaml.org/packages/ledgerwallet-tezos/)
- Reason for vendoring:
  - It depends on ocaml-hidapi that we currently vendor.
    A library installed by opam cannot depend on a vendored library.
  - Unknown

### Differences with the initial library
Unknown

