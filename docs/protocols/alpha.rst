Protocol Alpha
==============

This page lists the changes brought by protocol Alpha with respect
to Seoul (see :ref:`naming_convention`).
For the list of changes brought by Seoul with respect to Rio, see :doc:`../protocols/023_seoul`.

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
  list contains the fields present in the previous version of this RPC: ``delegate``, ``slots``,
  ``consensus_key``, and ``companion_key`` (optional).
  Also include a new field for delegates, ``attesting_power``, with their attesting power
  for the level.
  (MR :gl:`!18931`, :gl:`!18959`)

- Updated ``GET /chains/<chain_id>/blocks/<block_id>/context/issuance/expected_issuance``,
  changing ``baking_reward_bonus_per_slot`` with ``baking_reward_bonus_per_block``, and
  ``attesting_reward_per_slot`` with ``ættesting_reward_per_block``. (MR :gl:`!18959`)


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
  10800 (~1 day) to 1800 (~4 hours) on mainnet (with 8-second minimal
  block time). (MR :gl:`!17583`)

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

- Reduced blocks per commitment (``blocks_per_commitment``) to 14 to
  keep the number of nonces per cycle to 128. (:gl:`!17583`)

- Reduced the nonce revelation period (``nonce_revelation_threshold``)
  from 300 blocks to 150 blocks. Reduced accordingly the VDF
  difficulty (``vdf_difficulty``) as well. (:gl:`!17583`)

- Lowered the ``consensus_rights_delay`` protocol constant from 2
  cycles to 1 cycle. (MR :gl:`!18783`)

- Reduced the ``cache_stake_distribution_cycles`` and
  ``cache_sampler_state_cycles`` protocol constants from 5 cycles to 4
  cycles, in order to reduce memory consumption. Only
  ``consensus_rights_delay + slashing_delay + 2 = 1 + 1 + 2 = 4``
  cycles are needed. (MR :gl:`!18783`)

- Replaced the ``tolerated_inactivity_period`` protocol constant with
  three new protocol constants: ``tolerated_inactivity_period_low``,
  ``tolerated_inactivity_period_high`` and
  ``tolerated_inactivity_period_threshold``. The tolerated inactivity
  period now depends on the delegate's stake ratio over the total
  active stake. If the ratio is greater than the
  ``tolerated_inactivity_period_threshold = 10`` (expressed in 'per
  thousand'), we apply a low tolerance
  ``tolerated_inactivity_period_low = 1``. Otherwise, we apply a high
  tolerance ``tolerated_inactivity_period_high = 12``. If the stake is
  unknown, we apply a low tolerance (e.g., after the delegate's
  registration, reactivation, or decreasing its stake below
  ``minimal_stake``). (MR :gl:`!17582`)

Bug Fixes
---------

Minor Changes
-------------

Internal
--------
