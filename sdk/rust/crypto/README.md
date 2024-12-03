Tezos crypto
===========

Component contains cryptographic algorithms for hashing, signing & signature verification, with a slant towards
those used by [tezos](https://tezos.com/) - and in particular the `Kernel SDK` for [smart rollups](https://tezos.gitlab.io/alpha/smart_rollups.html).

## Hash module

`tezos_crypto::hash` contains definitions for common hashes in tezos - such as contract & address hashes. These
support `b58check` encoding/decoding with the same prefixes used in the rest of tezos - such as `tz1` for `ed25519` addresses.

These support encoding/decoding to binary with the `tezos_encoding` crate.
