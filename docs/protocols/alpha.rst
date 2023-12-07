Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to Oxford (see :ref:`naming_convention`).

For changes brought by the rejected Oxford proposal with respect to Nairobi, see :doc:`../protocols/018_oxford`.

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Octez.

.. contents::

Environment Version
-------------------

This protocol requires a different protocol environment version than Oxford.
It requires protocol environment V12, compared to V11 for Oxford.

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

- Add private rollups: smart rollup with an updatable whitelist stakers. Only stakers on the whitelist can publish commitment and participate in a refutation game. (MRs :gl:`!9823`, :gl:`!10104`, :gl:`!9823`, :gl:`!9572`, :gl:`!9427`, :gl:`!9472`, :gl:`!9439`, :gl:`!9401`)

Zero Knowledge Rollups (ongoing)
--------------------------------

Data Availability Layer (ongoing)
---------------------------------

Adaptive Issuance (ongoing)
----------------------------

- Adaptive Issuance is locked behind a feature flag and cannot be activated for this proposal. The voting mechanism for Adaptive Issuance remains accessible, but is ignored and can never activate the feature. Moreover, the vote EMA will be reset before reactivating the feature flag. (MR :gl:`!10371`)

- The ``stake`` and ``unstake`` operations are currently deactivated, calls to these operations will fail. Staking and unstaking transfers are still used internally, and may appear in balance receipts. (MR :gl:`!10849`)

- The ``unstake`` client command uses the ``amount`` field instead of an extra parameter. (MRs :gl:`!10377`, :gl:`!10429`)

- The semantics of forbidden delegates has been adjusted: a delegate becomes forbidden if it has been slashed for more than 51% of its frozen stake over the last 2 cycles. (MRs :gl:`!10382`, :gl:`!10844`)

- Slashing penalties for double-signing are now applied at the end of the cycle where denunciations were included, rather than immediately. The same applies for rewards allocated from denunciations. (MR :gl:`!10389`)

- Double baking penalty is now 5% of the offending baker's stake -- instead of 10%. (MR :gl:`!10431`)

- The ``set deposits limit`` operation has been brought back. (MR :gl:`!10449`)

- Balance updates now include more information related to staking in general, including slashing and rewards. (MRs :gl:`!10485`, :gl:`!10486`, :gl:`!10487`, :gl:`!10488`, :gl:`!10496`, :gl:`!10526`, :gl:`!10766`, :gl:`!10853`)

- The new staking mechanism is used internally to freeze deposits automatically at cycle ends, and mimic Nairobi's behavior. (MR :gl:`!10562`)

- Unstaked frozen deposits, i.e recently unstaked funds, can be used by bakers to be staked again (unless the baker has been slashed). They are used in addition to liquid funds for staking, prioritizing the most recent unstake requests. (MR :gl:`!10781`)

Gas improvements
----------------

Breaking Changes
----------------

A DAL attestation operation now contains a new ``slot`` field, while the
``attestor`` field is removed. (MRs :gl:`!10183`, :gl:`!10294`, :gl:`!10317`)

RPC Changes
-----------

Operation receipts
------------------

Protocol parameters
-------------------

- The protocol constant ``max_slashing_period`` has been moved from parametric
  constants to fixed constants. (MR :gl:`!10451`)

Bug Fixes
---------

- Fix reporting of gas in traced execution of Michelson scripts. (MR :gl:`!6558`)

Minor Changes
-------------

- Arithmetic errors on Michelson ``mutez`` type have been exported so
  they can now be caught outside of the protocol. (MR :gl:`!9934`)

- Michelson error traces for elaboration of invalid data was made more
  consistent by adding errors in some cases (BLS12-381 values, Sapling
  transactions, and timelocks). (MR :gl:`!10227`)

Internal
--------

- Register an error's encoding: ``WASM_proof_verification_failed``. It was
  previously not registered, making the error message a bit obscure. (MR :gl:`!9603`)
