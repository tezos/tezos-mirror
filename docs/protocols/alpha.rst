Protocol Alpha
==============

This page documents the changes brought by protocol Alpha with respect
to Paris (see :ref:`naming_convention`).

For changes brought by Paris with respect to Oxford, see :doc:`../protocols/019_paris`.

The code can be found in directory :src:`src/proto_alpha` of the ``master``
branch of Octez.

Environment Version
-------------------

This protocol requires a different protocol environment version than Paris.
It requires protocol environment V13, compared to V12 for Paris.

- Removed the 4th version of the WASM PVM, added the 5th version of the WASM
  PVM. (MR :gl:`!12999`)

Smart Rollups
-------------

- Michelson values of type ``contract _`` are now allowed in messages
  exchanged between smart contracts and smart rollups. In particular,
  it is now possible for smart rollups to use typed callbacks in
  outbox messages. (MR :gl:`!11130`)

- Bumped WASM PVM to V5. (MR :gl:`!12999`)

- Reworked constants migration to preserve the various period duration instead
  of indiscriminately imposing them (e.g., imposing 2 weeks for the challenge
  window or 15 minutes for the commitment period). (MR :gl:`!13821`)

- Fixed the traversal logic of the commitments tree, by considering previous
  commitment periods when computing what should be the level of a commitment
  successor. (MR :gl:`!13841`)

Zero Knowledge Rollups (ongoing)
--------------------------------

Data Availability Layer
-----------------------

Adaptive Issuance
-----------------

- Added the Dynamic Maximum system, that changes the maximum issuance as a
  function of the stake ratio. (MR :gl:`!13519`)

Gas improvements
----------------

Breaking Changes
----------------

RPC Changes
-----------

Operation receipts
------------------

- To better differentiate Deposits coming from 'rewards from bakers own stakes'
  from 'the edge bakers may take from their stakers rewards', the balance updates
  field has been specialized. The field {"staker":{"baker": <delegate_pkh>}} is now
  split into {"staker":{"baker_own_stake": <delegate_pkh>}} and {"staker":{"baker_edge":
  <delegate_pkh>}}. (MR :gl:`!12258`)

Errors
------

- The ``validate.operation.inconsistent_sources`` and
  ``validate.operation.inconsistent_counters`` errors have been
  expanded with information on the problematic sources and
  counters. The error messages have been updated accordingly, but the
  error IDs remain unchanged. (MR :gl:`!13138`)

Protocol parameters
-------------------

8s Blocks Time (MR :gl:`!12716`)
---------------------------------

Blocks time have been reduced from 10 seconds to 8 seconds. That is, a block can
be produced with a delay of 8 seconds with respect to the previous block, if
both blocks have round 0. This change comes with updating many related protocol
parameters in order to match the reduced blocks time. In particular, the
following quantities are kept the same:

- the minimal time period of a cycle (namely, 2 days, 20 hours, and 16 minutes),
- the length of the nonce revelation period (namely, around 2 hours and 8 minutes)
- the number of nonce commitments per cycle (namely, 128),
- the maximum rewards per minute (namely 80 tez), and therefore roughly the same issuance,
- the minimal "time to live" of an operation (namely, 1 hour),
- the block gas limit per minute (namely, 10400000 gas),
- the ratio between the liquidity baking subsidy and the maximum rewards per block (namely, 1/16).

.. list-table:: Changes to protocol parameters
   :widths: 50 25 25
   :header-rows: 1

   * - Parameter (unit)
     - Old (Paris) value
     - New value
   * - ``minimal_block_delay`` (seconds)
     - ``10``
     - ``8``
   * - ``delay_increment_per_round`` (seconds)
     - ``5``
     - ``4``
   * - ``blocks_per_cycle`` (blocks)
     - ``24576``
     - ``30720``
   * - ``blocks_per_commitment`` (blocks)
     - ``192``
     - ``240``
   * - ``nonce_revelation_threshold`` (blocks)
     - ``768``
     - ``960``
   * - ``max_operations_time_to_live`` (blocks)
     - ``360``
     - ``450``
   * - ``hard_gas_limit_per_block`` (gas unit)
     - ``1733333``
     - ``1386666``

Bug Fixes
---------

Minor Changes
-------------

Internal
--------

- ``balance_update_encoding_with_legacy_attestation_name`` has been removed.
  (MR :gl:`!13461`)

- Encoding that supports ``endorsement`` kind in apply_result JSON has been
  removed. (MR :gl:`!13974`)

- Encoding that supports ``endorsement`` kind in operation JSON has been
  removed. (MR :gl:`!13976`)
