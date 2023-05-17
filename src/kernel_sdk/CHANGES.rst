:orphan:

Changelog
'''''''''

Version 0.1.0
=============

SDK
---

Initial release to crates.io, of the following:

- ``tezos-smart-rollup-core``: low-level C-style definitions & linking of host functions.
- ``tezos-smart-rollup-host``: safe-rust wrapper trait & definitions over host functions.
- ``tezos-smart-rollup-debug``: output formatted messages to the rollup debug log.
- ``tezos-smart-rollup-panic-hook``: capture kernel panics & write them to the debug log.
- ``tezos-smart-rollup-encoding``: rollup-specific data types & encodings, such as definitions
  for parsing inbox messages, and writing outbox messages.
- ``tezos-smart-rollup-storage``: transactional account system over durable storage, supporting
  nested commits/rollbacks of account state.
- ``tezos-smart-rollup-entrypoint``: macro to correctly initialise ``kernel_entry`` entrypoint when
  targetting WASM, which is called by the rollup.
- ``tezos-smart-rollup-mock``: mock implementation of host functions, to enable unit/integration
  testing of kernels without needing to compile to WASM.
- ``tezos-smart-rollup-sdk``: top-level SDK crate to allow kernel development with fewer initial
  dependencies.

The initial release supports all host functions defined in the **Mumbai** protocol.

Installer client
----------------

Initial release to https://crates.io.

The installer client enables installation of kernels which are too big to fit in a ``rollup origination``
operation. Instead, the client generates an installer which immediately upgrades to desired kernel. This
*installer kernel* may then be used to originate the rollup with.


Version next
=============

SDK
---

- Implements trait ``Error`` and ``Display`` for ``PathError`` in ``tezos-smart-rollup-host``.
- Upgrade dependencies:
  - ``tezos_crypto_rs``, ``tezos_data_encoding`` to ``v0.5.0``.
  - ``nom`` to ``7.1``.
- Rework ``dac`` module in ``tezos-smart-rollup-encoding``:
  -  Keep ``PreimageHash`` in ``dac`` module.
  -  Move all other functions/structs move to ``dac::pages`` submodule. Deprecate importing from them ``dac`` directly.
- Introduce ``dac::certificate`` submodule for handling serialization and deserialization of DAC certificates.
- Add ``inbox::ExternalMessageFrame`` to ``tezos-smart-rollup-encoding``, to define a shared framing protocol for
  Smart Rollup external messages.
- Add a feature flag ``proto-nairobi`` to enable host functions introduced in the ``Nairobi``
  protocol.
- Implement host function ``store_delete_value`` introduced in the ``Nairobi`` protocol.

Installer client
----------------
