# lib_smart_rollup -- Protocol-agnostic shared types for smart rollups

Library name: `octez_smart_rollup` (package `octez-l2-libs`)

## Purpose

Protocol-independent data types shared across the rollup node, protocol plugins, and tools. This is a leaf library with no dependency on node logic.

## Key types

- `Kind.t` -- PVM kinds: `Example_arith | Wasm_2_0_0 | Riscv`
- `Commitment.t` -- predecessor, inbox_level, number_of_ticks, compressed_state
- `Game.t` -- refutation game state: dissection chunks, conflicts, players (Alice/Bob)
- `Inbox.t` -- inbox with skip-list history proofs and Merkle witnesses
- `L1_operation.t` -- operations the node injects: `Publish`, `Cement`, `Refute`, `Timeout`, `Add_messages`, `Recover_bond`, `Execute_outbox_message`, `Publish_dal_commitment`
- `Operation_kind.t` -- operation kind tags with priority ordering
- `Sc_rollup_block.t` -- L2 block structure (header, content, ticks)
- `Rollup_node_services` -- RPC service path/query definitions (large file, co-edited with `rpc_directory.ml` in `lib_smart_rollup_node`)

## Adding a new shared type

Every type must provide `Data_encoding.t` and `pp` (pretty-printer). Use the `Versioned_data` pattern for protocol-compatible encodings -- see `commitment.ml` for the template. Hash types use the Tezos hash infrastructure (B58 check, fixed-size).

## Adding a new RPC service

Define the service path and query parameters in `rollup_node_services.ml` here, then register the handler in `src/lib_smart_rollup_node/rpc_directory.ml`.

## Build

```
dune build src/lib_smart_rollup/
```

Dune file is auto-generated from `manifest/main.ml` -- do not edit directly.
