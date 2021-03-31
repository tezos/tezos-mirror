# Tezos Environment Changelog

This document lists changes to the [Tezos Environment](https://tezos.gitlab.io/shell/the_big_picture.html#the-economic-protocol-environment-and-compiler)

Changes are grouped by environment version and listed in reverse chronological order.

## V3

Architecture word-size: refuse to execute on 32-bit machines

## V2

Everywhere: update version dependencies for data-encoding, resto and lwt-canceler
https://gitlab.com/tezos/tezos/-/merge_requests/2570

Proxy mode: uses Irmin to implement the cache
https://gitlab.com/tezos/tezos/-/merge_requests/2562

lib_crypto: add hacl checks
https://gitlab.com/tezos/tezos/-/merge_requests/2580

More wrapping unwrapping of errors in proto
https://gitlab.com/tezos/tezos/-/merge_requests/2566

Proto V2: improve the Context.Tree.kind API to not load values
https://gitlab.com/tezos/tezos/-/merge_requests/2583

Add safe arrays to the version 2 of the environment
https://gitlab.com/tezos/tezos/-/merge_requests/2467

Add [numbits] in the version 2 of the protocol environment
https://gitlab.com/tezos/tezos/-/merge_requests/2464

proto-env no structs/v2/option
https://gitlab.com/tezos/tezos/-/merge_requests/2546

Add trees to the env v2
https://gitlab.com/tezos/tezos/-/merge_requests/2457

Storage: abstract the type of Tezos_storage_memory.Context.t
https://gitlab.com/tezos/tezos/-/merge_requests/2550

Env: use tezos-storage.context instead of implementing a new memory context
https://gitlab.com/tezos/tezos/-/merge_requests/2466

introduce Lwtreslib.Option and Lwtreslib.Result
https://gitlab.com/tezos/tezos/-/merge_requests/2054

Improve the context API
https://gitlab.com/tezos/tezos/-/merge_requests/2440

traces are abstract in the env v2
https://gitlab.com/tezos/tezos/-/merge_requests/2484

environment V2
https://gitlab.com/tezos/tezos/-/merge_requests/2444

Storage: rename context functions
https://gitlab.com/tezos/tezos/-/merge_requests/2461

Add new client mode: proxy
https://gitlab.com/tezos/tezos/-/merge_requests/1943

Sapling: rename sapling in tezos-sapling
https://gitlab.com/tezos/tezos/-/merge_requests/2417

Tests: quickcheck that memory_context is an array theory
https://gitlab.com/tezos/tezos/-/merge_requests/2282

## V1

See [008 changelog](https://tezos.gitlab.io/protocols/008_edo.html#environment-v1).
