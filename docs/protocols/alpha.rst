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

Zero Knowledge Rollups (ongoing)
--------------------------------

Data Availability Layer (ongoing)
---------------------------------

Gas improvements
----------------

Breaking Changes
----------------

RPC Changes
-----------

- Split duplicated argument ``pkh`` in RPC ``smart_rollups/smart_rollup/<address>/staker1/<pkh>/staker2/<pkh>/timeout``
  and ``smart_rollups/smart_rollup/<address>/staker1/<pkh>/staker2/<pkh>/timeout_reached`` into ``/staker1/<staker1_pkh>/staker2/<staker2_pkh>``.
  This changes the RPC description but not its use. (MR :gl:`!8339`)

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
