# lib_layer2_riscv_context -- RISC-V PVM context via Rust bindings

Library name: `octez_layer2_riscv_context`

## Purpose

Single-file library implementing `Context_sigs.S` (defined in `lib_layer2_store`) for the RISC-V PVM. Thin wrapper around `Octez_riscv_pvm.Storage`.

## Key behavior

- No cheap snapshot: checkout is expensive for RISC-V (unlike Irmin)
- State types delegate to `Storage.State.t` / `Storage.Mutable_state.t`
- Hash conversions between `Storage.Id.t` and `Smart_rollup_context_hash.t`
- Requires RISC-V Rust dependencies to build

## Files

- `riscv_context.ml` -- entire implementation (~107 lines)

## Build

```
dune build src/lib_layer2_riscv_context/
```
