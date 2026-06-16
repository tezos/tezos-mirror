# lib_injector -- Functor-based L1 operation injection framework

Library name: `octez_injector` (package `octez-injector`)

## Purpose

Manages the full lifecycle of L1 operations: queue, simulate, inject, track, confirm. Protocol-agnostic via functors. Used by the rollup node (instantiated in `src/lib_smart_rollup_node/injector.ml`) and standalone injector server.

## Architecture

### Functor: `Injector_functor.Make(Parameters)`

Parameters provide:
- `state` -- application-specific state
- `Tag` -- operation classification (e.g., `Publish`, `Cement`, `Add_messages`)
- `Operation` -- operation type with encoding
- `PROTOCOL_CLIENT` -- protocol-specific simulation, signing, inclusion checks

### Worker model

One async worker per `(signers, tags)` pair. Workers are dropbox-based (single pending request, new merges over old). Signers and tags must be **disjoint** across workers -- overlap triggers error at init.

## Operation lifecycle

```
Pending (Op_heap)  ->  Simulate  ->  Inject on L1  ->  Track  ->  Included  ->  Confirmed
     ^                                                    |
     +--------------- retry on reorg/failure -------------+
```

1. **Pending**: Bounded min-heap (50K), disk-persistent (filtered by `persist_operation`), filtered by tags on load
2. **Simulate**: Recursive dichotomy on quota exceed (halves batch until it fits)
3. **Inject**: Sign + submit to L1 node; rotate signers for load balancing
4. **Track**: Match injected `operation_hash` against block contents via LRU cache (32 blocks)
5. **Confirm**: Operations in blocks at `confirmed_level - retention_period` are forgotten

## Tag-based routing

- `inject(?tags)` -- trigger only workers matching these tags
- Each operation type maps to exactly one tag via `Parameters.operation_tag`
- Tags are disjoint per worker; overlap triggers error

## Injection strategies

Two strategies, set per-worker at init:
- **`Each_block`** -- inject after every new L1 block
- **`Delay_block of float`** -- wait `f * next_block_time` before injecting (accumulates more operations per batch)

## Retry logic

Unsuccessful operations get a pluggable retry decision via `Parameters.retry_unsuccessful_operation`:

| Status | Default action |
|--------|---------------|
| `Other_branch` (reorg) | Retry |
| `Backtracked` / `Skipped` | Retry |
| `Failed` permanent/outdated | Forget |
| `Failed` branch/temporary | Retry |
| `Never_included` (TTL exceeded) | Forget |

- `allowed_attempts` (default 10): max retries before discard
- `injection_ttl` (default 120 blocks): max wait for inclusion

## Disk persistence

`disk_persistence.ml` provides:
- `Make_table` -- one file per key in `data_dir/name/`, atomic writes (write to temp, rename)
- `Make_heap` -- bounded persistent min-heap for operation queue
- Only operations where `Parameters.persist_operation = true` are saved (e.g., batched messages). Commitments and refutation moves are NOT persisted -- they are re-derived from chain state on restart.

## Reorganization handling

On new L1 head, detect reorgs via `Layer_1.get_tezos_reorg_for_new_head()`. Reverted operations re-queued with `Other_branch` status. Reorg analysis skipped if >120 blocks deep.

## IMPORTANT: Injector anti-pattern

**Do NOT rely on injector feedback to determine operation outcomes.** The injector tells you an operation was injected/included, but this can be stale after reorgs. Instead, observe L1 blocks directly during normal daemon processing. See also `lib_smart_rollup_node/CLAUDE.md`.

## Key files

| File | Role |
|------|------|
| `injector_sigs.ml` | All module types: `PARAMETERS`, `PROTOCOL_CLIENT`, `S`. **Read this first.** |
| `injector_functor.ml/mli` | Core functor (~1800 lines): worker lifecycle, injection, tracking |
| `injector_operation.ml/mli` | Wraps operations with ID, order, error count |
| `disk_persistence.ml/mli` | Persistent hash table and heap |
| `injector_protocol.ml/mli` | Protocol client registration per `Protocol_hash.t` |
| `injector_events.ml` | Event declarations (inject, include, revert, discard) |
| `injector_metrics.ml/mli` | Prometheus gauges per tag |
| `injector_common.ml/mli` | Signer, fee parameter types |

## Protocol plugins

Each protocol implements `PROTOCOL_CLIENT` in `src/proto_*/lib_sc_rollup_node/sc_rollup_injector.ml`.

## Build

```
dune build src/lib_injector/
```
