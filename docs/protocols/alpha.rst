Protocol Alpha
==============

This page lists the changes brought by protocol Alpha with respect
to Seoul (see :ref:`naming_convention`).
For the list of changes brought by Seoul with respect to Rio, see :doc:`./023_seoul`.

For a higher-level overview of the most salient new features see the
`announcement blog <https://research-development.nomadic-labs.com/blog.html>`__.

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Octez and the full documentation in :doc:`this page <../alpha/index>`.

Environment Version
-------------------



Smart Rollups
-------------


Data Availability Layer
-----------------------

Adaptive Issuance
-----------------

Michelson
---------

- A new instruction named ``INDEX_ADDRESS`` has been added, it
  provides a unique identifier of type ``nat`` for values
  of type ``address``, stored in the context. (MR :gl:`!18866`)

- A new instruction named ``GET_ADDRESS_INDEX`` has been added, and returns the
  unique identifier of type ``nat option`` for values of type ``address``, if
  the address has already been indexed through ``INDEX_ADDRESS``. Returns
  ``None`` otherwise. (MR :gl:`!18866`)

Gas improvements
----------------

Breaking Changes
----------------

- Updated ``GET
  /chains/<chain_id>/blocks/<block_id>/helpers/validators`` to group delegates by level.
  The returned list contains one element for each queried level (by default, only the current level),
  and contains four fields: the ``level`` itself, the ``consensus_threshold`` required for the current
  level, the ``consensus_committee`` of the current level, and ``delegates`` which is the list
  of validators for that level. Each element of this last
  list contains the fields present in the previous version of this RPC: ``delegate``, "slots"
  which have been renamed to ``rounds``, ``consensus_key``, and ``companion_key`` (optional).
  Also include new fields for delegates, ``attesting_power``, with their attesting power
  for the level, and ``attestation_slot``, their slot for the given level.
  (MR :gl:`!18931`, :gl:`!18959`, :gl:`!18984`)

- Updated ``GET /chains/<chain_id>/blocks/<block_id>/context/issuance/expected_issuance``,
  changing ``baking_reward_bonus_per_slot`` with ``baking_reward_bonus_per_block``, and
  ``attesting_reward_per_slot`` with ``attesting_reward_per_block``. (MR :gl:`!18959`)


RPC Changes
-----------

- Added ``GET /chains/<chain_id>/blocks/<block_id>/helpers/stake_info``,
  which returns the staking power distribution for all the active delegates
  at the current cycle. (MR :gl:`!18019`)
- Added ``GET
  /chains/<chain_id>/blocks/<block_id>/context/destination/<destination>/index``
  which returns the index of the given destination (e.g. tz1, Smart Rollup
  addresses, etc.) or ``null`` if the destination has not been indexed by
  the opcode ``INDEX_ADDRESS`` yet. (MR :gl:`!18944`)
- Added ``GET
  /chains/<chain_id>/blocks/<block_id>/helpers/tz4_staker_number_ratio?cycle=<cycle>``
  which returns the portion of active delegates that sign with a BLS key.
  The ``cycle`` argument, if omitted, defaults to the current
  cycle. (MR :gl:`!19093`)

Operation receipts
------------------

- Added ``address_registry_diff`` field in ``metadata`` for newly indexed
  addresses from the opcode ``INDEX_ADDRESS`` in the operation. (MR
  :gl:`!18870`)

Errors
------


Protocol parameters
-------------------

- Lowered the number of blocks per cycle (``blocks_per_cycle``) from
  10800 (~1 day) to 2400 (~4 hours) on mainnet (with 6-second minimal
  block time). (MRs :gl:`!17583`, :gl:`!19045`)

  - On ghostnet where minimal block time is 4s, lowered
    ``blocks_per_cycle`` from 10800 (~12 hours) to 3600 (~4
    hours). (MR :gl:`!17583`)

- In order for the duration of voting periods to stay about the same,
  updated the number of cycles per voting period
  (``cycles_per_voting_period``) on mainnet from 14 cycles (that is,
  ~14 days with old cycle duration) to 84 cycles (~14 days with new
  cycle duration). (MR :gl:`!17583`)

- In order to keep the same duration in days for the activation of
  delegate parameters, updated the number of cycles after which
  submitted delegate parameters take effect
  (``delegate_parameters_activation_delay``) on mainnet from 5 cycles
  (that is, ~5 days with old cycle duration) to 30 cycles (~5 days
  with new cycle duration). (MR :gl:`!17583`)

- Reduced blocks per commitment (``blocks_per_commitment``) to 18 to
  keep the number of nonces per cycle to 128. (MRs :gl:`!17583`,
  :gl:`!19045`)

- Reduced the nonce revelation period (``nonce_revelation_threshold``)
  from 300 blocks to 200 blocks. Reduced accordingly the VDF
  difficulty (``vdf_difficulty``) as well. (MRs :gl:`!17583`,
  :gl:`!19045`)

- Lowered the ``consensus_rights_delay`` protocol constant from 2
  cycles to 1 cycle. (MR :gl:`!18783`)

- Reduced the ``cache_stake_distribution_cycles`` and
  ``cache_sampler_state_cycles`` protocol constants from 5 cycles to 4
  cycles, in order to reduce memory consumption. Only
  ``consensus_rights_delay + slashing_delay + 2 = 1 + 1 + 2 = 4``
  cycles are needed. (MR :gl:`!18783`)


6s Block Time (MR :gl:`!19045`)
---------------------------------

Block time have been reduced from 8 seconds to 6 seconds. That is, a
block can be produced with a delay of 6 seconds with respect to the
previous block, if both blocks have round 0. This change comes with
updating many related protocol parameters in order to match the
reduced block time.

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
     - ``2400``
   * - ``blocks_per_commitment`` (blocks)
     - ``84``
     - ``18``
   * - ``nonce_revelation_threshold`` (blocks)
     - ``300``
     - ``200``
   * - ``max_operations_time_to_live`` (blocks)
     - ``450``
     - ``600``
   * - ``hard_gas_limit_per_block`` (gas unit)
     - ``1386666``
     - ``1040000``

Smart rollup protocol parameters have been updated with regard to the
reduction of block time ensuring the same duration as today. For
example, the challenge window is two weeks.

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

The ``smart_rollup_max_active_outbox_levels`` has not been updated,
and the max allowed period of withdrawal has been reduced in
consequence to ~10 days because the current storage implementation of
the executed outbox message in the Layer 1 does not allow to update it
safely.


Bug Fixes
---------

- Updated cache functions to include the context when
  needed. Previously backtracked gas costs for some cache calls are
  now properly accounted for, increasing by at most 2 units of gas per
  function call. Notably, **the ``set delegate`` operation now has a
  slightly higher gas cost.** (MR :gl:`!19134`)

Minor Changes
-------------

Internal
--------

- When activating this protocol directly from Genesis (so only in
  tests and on some test networks), baking rights of bootstrap
  accounts for the first few cycles are now computed with Adaptive
  Issuance enabled, meaning that delegated tez are already weighted
  less than staked tez. (MR :gl:`!16945`)

- Remove obsolete internal field related to Adaptive Issuance
  activation. (MR :gl:`!15789`)

- Added internal field related to All Bakers Attest All Blocks
  activation. (MR :gl:`!19093`)
