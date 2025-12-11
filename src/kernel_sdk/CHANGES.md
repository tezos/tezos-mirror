# Changelog

## Version next

### SDK

- Add experimental support for compiling kernels to a RISC-V image behind the `proto-alpha` flag.
- Add an experimental rollup host for RISC-V with an in-memory store behind the `experimental-host-in-memory-store` flag.
- Add an `OutboxQueue` that can be used when more than 100 outbox messages are produced at a given level.
- Add `From OutboxMessageTransaction`, `From OutboxMessageTransactionBatch` for `OutboxMessage` to simplify construction.
- Fix the incomplete inbox on the first level of using `MockHost::default()`.
- Add support for new michelson `Ticket` constructor.
- Add michelson `nat`.
- Removes deprecated `proto-nairobi` feature flag.
- Stabilise `OutboxMessage::WhitelistUpdate`.
- Add `tezos-smart-rollup-utils`, for tooling that interacts with non-kernel related standards in the Smart Rollup ecosystem.
- Add `tezos_smart_rollup_utils::inbox` module, constructing inboxes for testing, and manipulating `inbox.json` files.
- Add `tezos_smart_rollup::entrypoint` module for kernel entrypoint configuration & macros.
- Add `entrypoint::main` procedural macro to mark the kernel entrypoint function.
- Add `extra` feature flag, for functionalility not-expected to be used by kernels directly.
- Remove the `proto-alpha` flag restriction on DAL host functions.
- Refactor `OutboxMessage` to `OutboxMessageFull` enabling different atomic batch backends that implement `AtomicBatch` trait.
- Add `AtomicBatch2` ... `AtomicBatch5` structs implementing `AtomicBatch` and constructed from tuples
  of `OutboxMessageTransaction` with potentially different parameter types.
- Add `--keep-going` option to native cli, to control whether the kernel should exit once the inbox has been drained.
- Implement the generic `reveal` host function in the `MockHost`, this allows in particular to use the DAL host functions in the mockup.
- Bump `tezos_crypto_rs`/`tezos_data_encoding` to `0.6.0` release.
- Allow overriding of the `debug_log` sink when using `MockHost`.
- Rename `testing` feature flag to `mock-core-trait` on `core` crate.
- Remove redundant `testing` feature flag from `debug` crate.
- Remove redundant `testing` feature flag from `entrypoint` crate.
- Remove redundant `testing` feature flag from `host` crate.
- Disable `testing` as default features of the main `sdk` crate.
- Change `Runtime::store_value_size` to return `PathNotFound` when the input is the path of a directory.
- Add support for `MichelsonTimestamp` encoding and decoding.
- Return a dummy value for `Runtime::reboot_left` on RISC-V, as RISC-V kernels do not need to reboot.
- Add `Hash` implementation for `OwnedPath`.
- Remove `tezos-smart-rollup-encoding::contract::Contract`. One may continue to use `tezos-smart-rollup::types::Contract`, which is exported
  from `tezos-protocol::contract::Contract`. This is a drop-in replacement that is fully backwards compatible.
- Add `tezos-smart-rollup-keyspace` crate which defines the `KeySpace` high-level durable storage API

### Installer client/kernel

- Add support for using the `set` instruction with large (> 512 byte) values.
- Add `merge-setup-files` subcommand to merge multiple configuration files in one.

## Version 0.2.2

### SDK

- Add michelson `or` and `option`.
- Add a feature flag `proto-alpha` to enable host functions introduced in
  unreleased protocols.
- Add `Runtime::reveal_dal_page` to let a kernel request pages from Tezos’ Data
  Availability Layer (DAL).
- Add a new case `WhitelistUpdate(OutboxMessageWhitelistUpdate)` to the
  `OutboxMessage<Expr: Michelson>` encoding, behind the `proto-alpha` feature flag.
- The implementation of `write_debug` in the `MockHost` no longer prefixes the debug message
  with `DEBUG:` and do not print a newline by default.
- Correctly initialise `MockHost::reboots_remaining` flag for testing outside of `run_level` api.
- Add `Runtime::reveal_dal_parameters` to let a kernel request DAL parameters from the L1.
- Add experimental, partial support for RISC-V kernels behind the `proto-alpha` flag.
- Add a `bls` feature flag, enabled by default, which allows disabling features which require
  the `bls` feature from `tezos_crypto_rs`.

### Installer client/kernel

- Remove unused dependency on `blst` crate, to simplify installation.

## Version 0.2.1

### SDK

- Add `Certificate::verify` to verify that a *DAC* certificate has been a signed by a number of
  committee members greater than a given threshold.
- Add `Certificate::reveal_to_store` to reveal up to ~10MB of DAC payload to storage in one go.
- Implement `core::fmt::Display` for `OwnedPath` and `RefPath` to get paths as strings for tests, errors, etc.
- Adjust `Path` trait to require `Display` on types that implement it.
- Add `KERNEL_BOOT_PATH` constant to `tezos-smart-rollup-host`.
- `tezos-smart-rollup-installer-config`: add `eval_config_instr` and `upgrade_reveal_flow` functions to
  simplify kernel upgrades.
- Update `Runtime::store_write` to write the whole slice; previously errored on slices longer than 2KB.
- Remove uses of `proto-nairobi` feature flag. Feature flag remains on crates, but does nothing.
- Implements `PublicKeySignatureVerifier` for `PublicKey`.

### Installer client/kernel

- Add option `--display-root-hash` to display the root hash of the kernel that will be installed.

## Version 0.2.0

### SDK

- Implements trait `Error` and `Display` for `PathError` in `tezos-smart-rollup-host`.
- Upgrade dependencies:
  - `tezos_crypto_rs`, `tezos_data_encoding` to `v0.5.0`.
  - `nom` to `7.1`.
- Rework `dac` module in `tezos-smart-rollup-encoding`:
  -  Keep `PreimageHash` in `dac` module.
  -  Move all other functions/structs move to `dac::pages` submodule. Deprecate importing from them `dac` directly.
- Introduce `dac::certificate` submodule for handling serialization and deserialization of DAC certificates.
- Add `inbox::ExternalMessageFrame` to `tezos-smart-rollup-encoding`, to define a shared framing protocol for
  Smart Rollup external messages.
- Add a feature flag `proto-nairobi` to enable host functions introduced in the `Nairobi`
  protocol.
- Implement host function `store_delete_value` introduced in the `Nairobi` protocol.
- Introduce `PublicKey` definition in `tezos-smart-rollup-encoding`
- Add `store_read_all` and `store_write_all` to read (write) a full value from (to) the
  storage, leveraging the need to chunk the reading (writing).

### Installer client/kernel

- Installer now reboots following successful upgrade, rather than proceeding directly to the next level.
- make `install_kernel` public to allow non-installer kernels to re-use upgrade mechanism.

## Version 0.1.0

### SDK

Initial release to [crates.io](https://crates.io/crates/tezos-smart-rollup), of the following:

- `tezos-smart-rollup-core`: low-level C-style definitions & linking of host functions.
- `tezos-smart-rollup-host`: safe-rust wrapper trait & definitions over host functions.
- `tezos-smart-rollup-debug`: output formatted messages to the rollup debug log.
- `tezos-smart-rollup-panic-hook`: capture kernel panics & write them to the debug log.
- `tezos-smart-rollup-encoding`: rollup-specific data types & encodings, such as definitions
  for parsing inbox messages, and writing outbox messages.
- `tezos-smart-rollup-storage`: transactional account system over durable storage, supporting
  nested commits/rollbacks of account state.
- `tezos-smart-rollup-entrypoint`: macro to correctly initialise `kernel_entry` entrypoint when
  targetting WASM, which is called by the rollup.
- `tezos-smart-rollup-mock`: mock implementation of host functions, to enable unit/integration
  testing of kernels without needing to compile to WASM.
- `tezos-smart-rollup-sdk`: top-level SDK crate to allow kernel development with fewer initial
  dependencies.

The initial release supports all host functions defined in the **Mumbai** protocol.

### Installer client

Initial release to [crates.io](https://crates.io/crates/tezos-smart-rollup-installer).

The installer client enables installation of kernels which are too big to fit in a `rollup origination`
operation. Instead, the client generates an installer which immediately upgrades to desired kernel. This
*installer kernel* may then be used to originate the rollup with.
