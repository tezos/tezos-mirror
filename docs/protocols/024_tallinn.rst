.. TODO tezos/tezos#2170: search shifted protocol name/number & adapt

Protocol Tallinn
=================

This page lists the changes brought by protocol Tallinn with respect
to Seoul (see :ref:`naming_convention`).
For the list of changes brought by Seoul with respect to Rio, see :doc:`./023_seoul`.

For a higher-level overview of the most salient new features see the
`announcement blog <https://research-development.nomadic-labs.com/tallinn-announcement.html>`__.

An overview of breaking changes and deprecations introduced in
protocol Tallinn can be found :ref:`here<tallinn_breaking_changes>`. These
changes are also listed below in their respective topical section,
tagged with **Breaking change** or **Deprecation**.

The code is available in directory :src:`src/proto_024_PtTALLiN` of
the ``master`` branch of Octez and the full documentation in
:doc:`this page <../active/index>`.


Environment Version
-------------------

Protocol Tallinn uses the same environment version V15 as protocol
Seoul.


Smart Rollups
-------------

Data Availability Layer
-----------------------

Adaptive Issuance
-----------------

Michelson
---------

- Added a new instruction ``INDEX_ADDRESS``. It provides a unique
  identifier of type ``nat`` for values of type ``address``, stored in
  the context. (MRs :gl:`!18866`, :gl:`!18943`)

- Added a new instruction ``GET_ADDRESS_INDEX``. It returns the unique
  identifier of type ``nat option`` for values of type ``address``, if
  the address has already been indexed through ``INDEX_ADDRESS``; it
  returns ``None`` otherwise. (MR :gl:`!18866`)

- Improved error message when using non packable values with ``PACK``,
  ``FAILWITH`` and ``EMIT``, and in parameters of partially applied
  ``LAMBDA``. (MR :gl:`!19889`)

Gas improvements
----------------

.. _tallinn_RPC_changes:

RPC Changes
-----------

- **Breaking change** Updated RPC ``GET
  /chains/<chain_id>/blocks/<block_id>/helpers/validators`` to group
  delegates by level. The returned list contains one element for each
  queried level (by default, only the current level), and contains
  five fields: the ``level`` itself, the ``consensus_threshold``
  required for the current level, the ``consensus_committee`` of the
  current level, the ``all_bakers_attest_activated`` which indicates the
  activation status of the "All Bakers Attest" feature, and
  ``delegates`` which is the list of validators for
  that level. Each element of this last list contains the fields
  present in the previous version of this RPC: ``delegate``, "slots"
  which have been renamed to ``rounds``, ``consensus_key``, and
  ``companion_key`` (optional).  Also include new fields for
  delegates, ``attesting_power``, with their attesting power for the
  level, and ``attestation_slot``, their slot for the given level.
  (MRs :gl:`!18931`, :gl:`!18959`, :gl:`!18984`, :gl:`!19886`)

- **Breaking change** Updated RPC ``GET
  /chains/<chain_id>/blocks/<block_id>/context/issuance/expected_issuance``.
  Output field ``baking_reward_bonus_per_slot`` has been replaced with
  ``baking_reward_bonus_per_block``, and ``attesting_reward_per_slot``
  with ``attesting_reward_per_block``. Their respective values are
  consequently 7000 times as high as before (since there are 7000
  slots per block). (MR :gl:`!18959`)

- Added a new RPC ``GET
  /chains/<chain_id>/blocks/<block_id>/helpers/baking_power_distribution_for_current_cycle``. It
  returns the total baking power and baking power distribution for all
  the active delegates, that are used to determine consensus rights
  for the current cycle. (MRs :gl:`!18019`, :gl:`!19898`)

- Added a new RPC ``GET
  /chains/<chain_id>/blocks/<block_id>/helpers/tz4_baker_number_ratio?cycle=<cycle>``.
  It returns the portion of active delegates that sign with a BLS key.
  The ``cycle`` argument, if omitted, defaults to the current
  cycle. (MRs :gl:`!19093`, :gl:`!19710`)

- Added a new RPC ``GET
  /chains/<chain_id>/blocks/<block_id>/context/destination/<destination>/index``.
  It returns the index of the given destination (e.g. tz1, Smart
  Rollup addresses, etc.), or ``null`` if the destination has not been
  indexed by the opcode ``INDEX_ADDRESS`` yet. (MR :gl:`!18944`)

- Added a new RPC ``GET
  /chains/<chain_id>/blocks/<block_id>/helpers/all_bakers_attest_activation_level``.
  It returns the first level at which the All Bakers Attest feature activates. If this
  level is not yet set, it returns ``null`` instead. (MR :gl:`!19757`)


Blocks and block receipts
-------------------------

- **Breaking change** Removed obsolete field
  ``adaptive_issuance_vote`` from the block header, and fields
  ``adaptive_issuance_vote_ema`` and
  ``adaptive_issuance_activation_cycle`` from the block metadata. Note
  that the adaptive issuance activation cycle (which is 748 on
  mainnet) can still be queried via the RPC ``GET
  /chains/<chain>/blocks/<block>/context/adaptive_issuance_launch_cycle``. (MR
  :gl:`!19215`)

- Added fields related to consensus in block metadata:
  ``attestations`` and ``preattestations``. They can be ``null`` when the corresponding
  consensus operations are not required in the block. Otherwise, they contain three fields:
  the ``total_committee_power`` and ``threshold``, as described in
  :ref:`the consensus documentation<tb_validator_tallinn>`, and the
  ``recorded_power``, summing the power of all (pre)attestations
  of the block. (MR :gl:`!19835`)

- Added ``all_bakers_attest_activation_level`` field in block metadata,
  which returns the activation level of the "All Bakers Attest" feature if
  the feature is set to activate. The field remains ``null`` otherwise. (MR :gl:`!19835`)

Operation receipts
------------------

- Added a new field ``address_registry_diff`` to the operation
  metadata. It contains the addresses that have been newly indexed
  through the opcode ``INDEX_ADDRESS``. (MR :gl:`!18870`)

- **Breaking change** Updated attestation and preattestation receipts metadata.
  The ``consensus_power`` field is now divided in two parts: an integer field ``slots``,
  which corresponds to the number of slots attributed to the delegate, and represents
  its consensus power until "All Bakers Attest" activates, and an optional string field
  ``baking_power`` parsed as an int64, which is the baking power of the delegate, and represents its consensus power
  once "All Bakers Attest" activates. This last field is not in the receipt until
  "All Bakers Attest" activates. (MRs :gl:`!18915`, :gl:`!19835`)


Errors
------

Protocol parameters
-------------------

.. _6s_block_time_parameters_tallinn:

6s Block Time (MRs :gl:`!19045`, :gl:`!19473`)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Breaking change** Block time has been reduced from 8 seconds to 6
seconds on mainnet (on ghostnet, it remains unchanged at 4
seconds). That is, a block can be produced with a delay of 6 seconds
with respect to the previous block, if the latter is at round 0.

This reduced block time comes with the updates of multiple related
protocol parameters:

.. list-table:: Changes to protocol parameters
   :widths: 50 25 25
   :header-rows: 1

   * - Parameter (unit)
     - Old (seoul) value
     - New value
   * - ``minimal_block_delay`` (seconds)
     - ``8``
     - ``6``
   * - ``delay_increment_per_round`` (seconds)
     - ``4``
     - ``3``
   * - ``blocks_per_cycle`` (blocks)
     - ``10800``
     - ``14400``
   * - ``blocks_per_commitment`` (blocks)
     - ``84``
     - ``112``
   * - ``nonce_revelation_threshold`` (blocks)
     - ``300``
     - ``400``
   * - ``max_operations_time_to_live`` (blocks)
     - ``450``
     - ``600``
   * - ``hard_gas_limit_per_block`` (gas unit)
     - ``1386666``
     - ``1040000``

Smart rollup protocol parameters have also been updated accordingly,
in order to preserve the same durations as in the previous
protocol. For example, the challenge window is still two weeks.

.. list-table:: Changes to smart rollup protocol parameters
   :widths: 50 25 25
   :header-rows: 1

   * - Parameter (unit)
     - Old (seoul) value
     - New value
   * - ``smart_rollup_challenge_window_in_blocks``
     - ``151200``
     - ``201600``
   * - ``smart_rollup_commitment_period_in_blocks``
     - ``112``
     - ``150``
   * - ``smart_rollup_max_lookahead_in_blocks``
     - ``324000``
     - ``432000``
   * - ``smart_rollup_timeout_period_in_blocks``
     - ``75600``
     - ``100800``

Note that the ``smart_rollup_max_active_outbox_levels`` has not been
updated, because the current storage implementation of the executed
outbox message in the Layer 1 does not allow for a safe update. As a
consequence, the maximal allowed :ref:`period for withdrawal <withdrawal_period>` of assets from
smart rollups to Layer 1 has been reduced from ~14 days to ~10 days.


Feature flags
^^^^^^^^^^^^^

- Replaced the feature-controlling parameter
  ``all_bakers_attest_activation_level`` with
  ``all_bakers_attest_activation_threshold``. This causes
  all-bakers-attest to activate as soon as the ratio of bakers that
  use a tz4 consensus keys exceeds the specified threshold, set to 50%
  in protocol Tallinn. (MR :gl:`!19093`)


Bug Fixes
---------

- **Breaking change** Updated cache functions to include the context
  when needed. Previously backtracked gas costs for some cache calls
  are now properly accounted for, increasing by at most 2 units of gas
  per function call. (MR :gl:`!19134`)


Minor Changes
-------------

Internal
--------

- When activating this protocol directly from Genesis (so only in
  tests and on some test networks):

  + Baking rights of bootstrap accounts for the first few cycles are
    now computed with Adaptive Issuance enabled, meaning that
    delegated tez are already weighted less than staked tez. (MR
    :gl:`!16945`)

  + Fixed registration of bootstrap accounts with an initial consensus
    key. (MR :gl:`!19314`)

- Removed dead code related to Adaptive Issuance activation, EMA, and
  per-block vote. (MRs :gl:`!15789`, :gl:`!19215`)
