Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to Oxford (see :ref:`naming_convention`).

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Octez.

.. contents::

Environment Version
-------------------

This protocol requires a different protocol environment version than Oxford.
It requires protocol environment V11, compared to V10 for Oxford.

Smart Rollups
-------------

- The ``smart_rollup_originate`` operation now also takes an optional
  whitelist of public key hashes. This whitelist cannot be used yet
  (the ``sc_rollup.private_enable`` flag has to be set to true). (MR :gl:`!9401`)

- The ``transferring`` parameter from smart rollup client command
  ``get proof for message <index> of outbox at level <level>`` is now optional. (MR :gl:`!9461`)

- Enable the latest version of the WASM PVM (``2.0.0-r3``). Existing smart
  rollups will see their PVM automatically upgrade, and newly originated smart
  rollups will use this version directly (MR :gl:`!9735`)

- Added the updated whitelist for private rollups in the receipt of
  the outbox message execution receipt. (MR :gl:`!10095`)

Zero Knowledge Rollups (ongoing)
--------------------------------

Data Availability Layer (ongoing)
---------------------------------

Adaptive Issuance (ongoing)
----------------------------

Gas improvements
----------------

Breaking Changes
----------------

A DAL attestation operation now contains a new ``slot`` field, while the
``attestor`` field is renamed to ``attester``. (MRs :gl:`!10183`, :gl:`!10294`)

RPC Changes
-----------

Operation receipts
------------------

Bug Fixes
---------

- Fix reporting of gas in traced execution of Michelson scripts. (MR :gl:`!6558`)

Minor Changes
-------------

- Arithmetic errors on Michelson ``mutez`` type have been exported so
  they can now be caught outside of the protocol. (MR :gl:`!9934`)

Internal
--------

- Register an error's encoding: ``WASM_proof_verification_failed``. It was
  previously not registered, making the error message a bit obscure. (MR :gl:`!9603`)

- Move some Michelson elaboration and erasure functions to the gas
  monad. (MR :gl:`!10071`)
