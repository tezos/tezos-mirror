Core API for the virtual `smart_rollup_core` WASM module, exposed by
[Tezos Smart Rollups](https://tezos.gitlab.io/alpha/smart_rollups.html).

# About

In order for a kernel to do useful work, it must be able to read input from users, respond with output, and update
its internal state. To do so, rollups make a set of C-style *host functions* available to kernels, which enable:

- reading from the **inbox**.
- writing to the **outbox**.
- manipulating **durable storage**.

This crate provides the definitions of these host functions, including linking to the `smart_rollup_core` WASM module.
In addition, various constants (such as error codes) used when interacting with these host functions, are defined here.

# Safety

This crate exposes the host functions through the `SmartRollupCore` trait, which is unsafe to use - as it makes no effort
to provide a *safe rust* API. A safe API is defined in the `tezos-smart-rollup-host` crate, as a series of traits:
- `WasmHost`
- `HostDebug`
- `HostReveal`
- `StorageV1`

It's therefore recommended to use these traits directly from the `tezos-smart-rollup-host` crate, or
alternatively directly through the `tezos-smart-rollup` top-level SDK crate.
