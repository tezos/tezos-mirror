# bin_smart_rollup_node -- Entry point binary for the smart rollup node

Executable: `octez-smart-rollup-node` (package `octez-smart-rollup-node`)

## Purpose

CLI entry point. Bridges the Tezos client framework with `Rollup_node_daemon`. Contains no business logic.

## File: `main_smart_rollup_node.ml`

Defines CLI commands via `Tezos_clic`:
- `run` -- start the daemon (replaces deprecated `legacy_run`)
- `config init` -- generate configuration file
- `protocols` -- list supported protocols
- `export/import snapshot` -- snapshot management
- `replay block(s)` -- replay L1 blocks for debugging
- `dump/patch durable storage` -- WASM PVM durable storage tools
- `dump metrics` / `openapi` -- observability

## Protocol plugin linking

All protocol plugins (Proxford through Alpha) are linked here. Plugins self-register via side effects on load -- the `-linkall` flag in dune ensures they are all loaded.

## IMPORTANT: Dune is auto-generated

The `dune` file is generated from `manifest/main.ml` -- **do not edit directly**. When adding a new protocol, add its plugin to the manifest.

## Build

```
dune build src/bin_smart_rollup_node/
```
