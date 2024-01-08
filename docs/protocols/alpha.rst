Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to Oxford (see :ref:`naming_convention`).

For changes brought by Oxford with respect to Nairobi, see :doc:`../protocols/018_oxford`.

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Octez.

.. contents::

Environment Version
-------------------


This protocol requires a different protocol environment version than Oxford.
It requires protocol environment V12, compared to V11 for Oxford.

Smart Rollups
-------------

Zero Knowledge Rollups (ongoing)
--------------------------------

Data Availability Layer (ongoing)
---------------------------------

- Introduced a ``round`` field in DAL attestations, with a similar meaning as
  for consensus attestations. (MR :gl:`!11285`)

Adaptive Issuance (ongoing)
----------------------------

- The staking balance is now explicitly initialized when a delegate is registered. (MR :gl:`!11197`)

- The issuance reward coeff is now computed only once.
  It used to be computed twice, once for the bonus, assuming a zero bonus, and once afterwards taking the bonus into account. (MR :gl:`!10935`)

- The minimal frozen stake is now checked before applying limits and then re-checked after applying limits and edge. (MR :gl:`!11086')

Gas improvements
----------------

Breaking Changes
----------------

RPC Changes
-----------

Operation receipts
------------------

Protocol parameters
-------------------

- Added ``consensus_rights_delay`` parametric constant. (MR :gl:`!11188`)

- Added ``blocks_preservation_cycles`` parametric constant. (MR :gl:`!11280`)

- Added ``delegate_parameters_activation_delay`` parametric constant. (MR :gl:`!11279`)

- Set the number of blocks preservation cycles to 1. (MR :gl:`!11325`)

Bug Fixes
---------

10s Blocks Time (MR :gl:`!11288`)
---------------------------------

Blocks time have been reduced from 15 seconds to 10 seconds. That is, a block
can be produced with a delay of 10 seconds with respect to the previous block,
if both blocks have round 0. This change comes with updating many related
protocol parameters in order to match the reduced blocks time. In particular,
the following quantities are kept the same:

- the minimal time period of a cycle (namely, 2 days, 20 hours, and 16 minutes),
- the length of the nonce revelation period (namely, around 2 hours and 8 minutes)
- the number of nonce commitments per cycle (namely, 128),
- the number of stake snapshots per cycle (namely, 16),
- the maximum rewards per minute (namely 80 tez), and therefore roughly the same inflation,
- the minimal "time to live" of an operation (namely, 1 hour),
- the block gas limit per minute (namely 10400000 gas),
- the ratio between the liquidity baking subsidy and the maximum rewards per block (namely, 1/16).

.. list-table:: Changes to protocol parameters
   :widths: 50 25 25
   :header-rows: 1

   * - Parameter (unit)
     - Old (oxford) value
     - New value
   * - ``minimal_block_delay`` (seconds)
     - ``15``
     - ``10``
   * - ``delay_increment_per_round`` (seconds)
     - ``8``
     - ``5``
   * - ``blocks_per_cycle`` (blocks)
     - ``16384``
     - ``24576``
   * - ``blocks_per_commitment`` (blocks)
     - ``128``
     - ``192``
   * - ``nonce_revelation_threshold`` (blocks)
     - ``512``
     - ``768``
   * - ``blocks_per_stake_snapshot`` (blocks)
     - ``1024``
     - ``1536``
   * - ``max_operations_time_to_live`` (blocks)
     - ``240``
     - ``360``
   * - ``hard_gas_limit_per_block`` (gas unit)
     - ``2600000``
     - ``1733333``


Minor Changes
-------------

- Michelson error traces for elaboration of invalid data was made more
  consistent by adding errors in some cases (BLS12-381 values, Sapling
  transactions, and timelocks). (MR :gl:`!10227`)

Internal
--------
