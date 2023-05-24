Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to Nairobi (see :ref:`naming_convention`).

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Octez.

.. contents::

Environment Version
-------------------

This protocol requires a different protocol environment version than Nairobi.
It requires protocol environment V10, compared to V9 for Nairobi.

Smart Rollups
-------------

- Add the support for bootstrapped smart rollups in storage initialization,
  similarly to bootstrapped accounts and smart contracts. (MR :gl:`!8552`)

Zero Knowledge Rollups (ongoing)
--------------------------------

Data Availability Layer (ongoing)
---------------------------------

Adaptive Inflation (ongoing)
----------------------------

- Introduce feature flag for Adaptive Inflation. (MR :gl:`!8566`)


Gas improvements
----------------

Breaking Changes
----------------

- Protocol parameter ``ratio_of_frozen_deposits_slashed_per_double_endorsement`` is
  converted from the ratio ``1/5`` into the percentage ``50%`` and renamed to
  ``percentage_of_frozen_deposits_slashed_per_double_endorsement``. (MR :gl:`!8753`)

- Protocol parameter ``double_baking_punishment`` is converted from a fixed
  value of ``640tz`` into the percentage ``11%`` and renamed to
  ``percentage_of_frozen_deposits_slashed_per_double_baking``. (MR :gl:`!8753`)


RPC Changes
-----------

- Split duplicated argument ``pkh`` in RPC ``smart_rollups/smart_rollup/<address>/staker1/<pkh>/staker2/<pkh>/timeout``
  and ``smart_rollups/smart_rollup/<address>/staker1/<pkh>/staker2/<pkh>/timeout_reached`` into ``/staker1/<staker1_pkh>/staker2/<staker2_pkh>``.
  This changes the RPC description but not its use. (MR :gl:`!8339`)

- Update context with new reward parameters. This changes the JSON from the RPC
  ``/chains/main/blocks/head/context/constants``. (MR :gl:`!8657`)

Operation receipts
------------------

Bug Fixes
---------

Minor Changes
-------------

Internal
--------

- Fail earlier when a smart rollup commitment is in conflict when cementing.
  (MR :gl:`!8128`)

- split smart rollup origination fct for readibility. (MR :gl:`!8276`)

- Remove the deprecated and unused ``tx_rollup_l2_address`` Michelson
  type. (MR :gl:`!8546`)

- Add an internal represention case for the ``UNIT`` Michelson instruction. (MR :gl:`!8579`)

- Encoding that supports ``endorsement`` kind in JSON are now suffixed with
  ``_with_legacy_attestation_name``. Non legacy encoding supports
  ``attestation`` kind. (MR :gl:`!8563`)

- Michelson: remove legacy behaviour related to contract type. (MR :gl:`!5800`)

- Michelson: cleanup legacy annotation management. (MR :gl:`!8208`)

- Michelson: refactor management of metadata in ty smart constructors. (MR :gl:`!8420`)

- Michelson: remove unused deprecated tx_rollup_l2_address type. (MR :gl:`!8546`)

- Rename ``source`` into ``sender``. (MR :gl:`!7373`)

- Improve efficiency of solving the baker PoW challenge. (MR :gl:`!8403`)

- Refactor declarations of ``make_empty_context`` and ``make_empty_tree`` for easier use.
  (MR :gl:`!8550`)

- Move notions of Smart rollup address and various smart rollup hashes types to
  the shell to make them common to all protocols though the environment. (MR
  :gl:`!8562`, MR :gl:`!8625`)

- Refactoring : stake splitted between a frozen part and a delegated part. (MR :gl:`!8051`)

- Refactoring : rewards computed as a relative portion of the total amount of tez
  rewarded per minute (about 85tez/min). (MR :gl:`!8657`)

- Introduce the notion of rollups “machine” which can compute the semantics of
  a given rollup, but cannot be used to generate or verify proof. (MR
  :gl:`!8815`)
