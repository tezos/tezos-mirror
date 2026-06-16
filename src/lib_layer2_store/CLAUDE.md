# lib_layer2_store -- Storage abstraction for layer 2 nodes

Library name: `octez_layer2_store` (package `octez-l2-libs`)

## Purpose

Defines context signatures, phantom-typed access modes, and dispatches context operations to PVM-specific backends (Irmin, RISC-V). Also provides indexed store abstractions and snapshot utilities.

## Phantom-typed access modes

`access_mode.ml` defines the core permission types used throughout the rollup node:

```ocaml
type rw = [`Read | `Write]
type ro = [`Read]
type _ t = Read_only : ro t | Read_write : rw t
```

These propagate through `Context.t`, `Node_context.t`, `Reference.t`, and store types. Rules:
- `[`Read]` = read-only
- `[`Read | `Write]` = read-write
- `[< `Read | `Write > `Read]` = at least Read, possibly Write (use in function signatures)
- Use `readonly` to downcast. **It is not possible to upcast.**

**Caveat**: subtyping constraints do not work well with GADTs. If a GADT or a
function over GADTs is necessary, use (or define) the type without the
constraints in the implementation.

## Context dispatch pattern

`context.ml` uses existential types to dispatch all operations (checkout, commit, gc, PVMState) to the correct PVM backend. The packed `pvm_context_impl` first-class module determines the backend. Equality witnesses ensure type-safe downcasting when mixing state from different backends.

To add a new context backend: implement `Context_sigs.S` (in `context_sigs.ml`) and register via `Context.Wrapper.Make`.

## PVM state types

- `pvmstate` (mutable) -- used during evaluation, modified in place
- `imm_pvmstate` (immutable) -- used for caching dissection states in refutation
- `PVMState.copy`, `imm_copy`, `mut_copy` -- safe conversions between representations

## Key files

| File | Role |
|------|------|
| `access_mode.ml` | Phantom type definitions (`rw`, `ro`, `_ t`) |
| `context_sigs.ml` | `Context_sigs.S` module type: the interface all context backends implement |
| `context.ml/mli` | Existential dispatcher: `index`, `t`, `pvmstate`, `Wrapper` functor |
| `irmin_store.ml/mli` | Irmin-pack `BACKEND` implementation |
| `indexed_store.ml/mli` | Index-based key-value store functors |
| `store_sigs.ml` | Store interface hierarchy: `BACKEND`, `SINGLETON_STORE`, `INDEXABLE_STORE`, `INDEXED_FILE` |
| `store_utils.ml/mli` | Generic store functors over any `BACKEND` |
| `snapshot_utils.ml/mli` | Snapshot creation/extraction with gzip, headers |
| `desync_snapshots.ml/mli` | Incremental snapshots via desync tool |

## Build

```
dune build src/lib_layer2_store/
```

## Tests

```
dune exec src/lib_layer2_store/test/test_indexed_store.exe
```
