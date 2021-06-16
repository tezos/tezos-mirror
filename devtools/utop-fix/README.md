# [Deprecated] utop-fix

## Deprecation note

Superseded by [tztop](../../src/tooling/tztop/README.md)

## Why a patch

This allows the tezos-protocol-compiler to look on the default OCaml environment, which may be undesirable in production, but is required for utop.

## How to use

```sh
# install utop
opam repository add default
opam install utop

# apply the patch
patch -p1 < devtools/utop-fix

# now you can just start utop
dune utop
```
