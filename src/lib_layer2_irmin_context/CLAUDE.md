# lib_layer2_irmin_context -- Irmin-based context for WASM/Arith PVMs

Library name: `octez_layer2_irmin_context`

## Purpose

Implements `Context_sigs.S` (defined in `lib_layer2_store`) using Irmin-pack. Used by WASM and Arith PVMs. Supports proofs for refutation games.

## Key types

- `state` = `tree` (Irmin tree, immutable)
- `mut_state` = `tree ref` (mutable wrapper for in-place PVM evaluation)
- `hash` = Irmin commit hash

## Key behaviors

- **`on_read_dirty = `Checkout`** -- cheap Irmin tree checkout (see `lib_layer2_store/CLAUDE.md` for the pattern)
- **GC**: `split` creates suffix chunks; `gc hash` removes data older than `hash`
- **Proofs**: `Proof` functor produces/verifies Irmin proofs for refutation games
- **PVMState**: stored under path `["pvm_state"]` in the tree

## Files

- `irmin_context.ml/mli` -- main implementation
- `irmin_context_events.ml` -- GC event logging

## Build

```
dune build src/lib_layer2_irmin_context/
```
