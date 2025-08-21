Protocol Quebec
===============

This page lists the changes brought by protocol Quebec with respect
to Paris (see :ref:`naming_convention`).
For the list of changes brought by Paris with respect to Oxford, see :doc:`../protocols/020_paris`.

For a higher-level overview of the most salient new features see the
`Quebec announcement <https://research-development.nomadic-labs.com/quebec-announcement.html>`__.

The code can be found in directory :src:`src/proto_021_PsQuebec` of the ``master``
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

Data Availability Layer
-----------------------

No changes.

Adaptive Issuance
-----------------

- Added the :ref:`Adaptive Maximum<adaptive_maximum>` system,
  that imposes a new bound on the issuance as a function of the staked
  ratio. (MRs :gl:`!13519`, :gl:`!14635`)

- Changed the semantics of the
  :ref:`min-delegated-in-current-cycle<min_delegated>` used to
  compute baking rights. It now only considers the minimum in between
  blocks. (MR :gl:`!13945`)

- Changed the protocol constant value of ``edge_of_staking_over_delegating``
  from 2 to 3. (MR :gl:`!14555`)

- Bumped the
  :ref:`GLOBAL_LIMIT_OF_STAKING_OVER_BAKING<overstaking_alpha>`
  protocol constant from 5 to 9.  (MR :gl:`!14905`)

Gas improvements
----------------

- Updated a few gas parameters to be more accurate. (MR :gl:`!13171`)

.. _breaking_changes_beta:

Breaking Changes
----------------

- Reworked RPC ``GET
  /chains/<chain_id>/blocks/<block_id>/context/delegates/<baker_pkh>``,
  which returns a lot of information about a given baker. Its fields
  now match all non-deprecated ``delegates/<baker_pkh>/...`` RPCs
  one-for-one. This change is breaking for both JSON and binary
  encodings. (MR :gl:`!14209`)

..
   This link does not include the protocol version: this is
   intentional, because it is used by multiple protocol docs to refer
   to this specific pass of RPC changes.

.. _delegates_RPCs_normalization:

RPC Changes
-----------

- In the following paths, ``../`` is short for
  ``/chains/<chain_id>/blocks/<block_id>/context/delegates/<baker_pkh>/``.
  See :doc:`../active/baking_power` for more
  details on the renamed and new RPCs.

  * Renamed RPC ``GET ../current_frozen_deposits`` to ``GET
    ../total_staked``.  The old path is now **deprecated**. (MR
    :gl:`!14176`)

  * Added RPC ``GET ../total_delegated``, which returns the amount
    that counts as delegated to the baker for the purpose of computing
    its baking rights. This includes tez owned by all delegators
    including the baker itself, but excludes staked tez. (MR
    :gl:`!14176`)

  * **Deprecated** RPC ``GET ../staking_balance``. To get its value,
    you can call RPCs ``GET ../total_staked`` and ``GET
    ../total_delegated``, and add their outputs together. (MR
    :gl:`!14176`)

  * Renamed RPC ``GET ../total_delegated_stake`` to ``GET
    ../external_staked``.  The old path is now **deprecated**. (MR
    :gl:`!14187`)

  * Added RPC ``GET ../external_delegated``, which returns the amount
    of non-staked tez owned by all delegators except for the
    baker itself. (MR :gl:`!14187`)

  * **Deprecated** RPC ``GET ../delegated_balance``. To get its value,
    you can call RPCs ``GET ../external_staked`` and ``GET
    ../external_delegated``, and add their outputs together. (MR
    :gl:`!14187`)

  * **Deprecated** RPC ``GET ../frozen_deposits``. To get its value,
    you can call RPCs ``GET ../total_staked`` on the last block of 3
    cycles ago. (MR :gl:`!14192`)

  * **Deprecated** RPC ``GET ../frozen_deposits_limit``. The RPC has
    no effects since the activation of Adaptive Issuance and Staking
    during the Paris protocol. (MR :gl:`!14192`)

  * Renamed RPC ``GET ../current_baking_power`` to ``GET
    ../baking_power``.  The old path is now **deprecated**. (MR
    :gl:`!14192`)

  * Renamed RPC ``GET ../delegated_contracts`` to ``GET
    ../delegators``.  The old path is now **deprecated**. (MR
    :gl:`!14192`)

  * Renamed RPC ``GET ../full_balance`` to ``GET
    ../own_full_balance``.  The old path is now **deprecated**. (MR
    :gl:`!14154`)

  * Added RPCs ``GET ../own_staked`` and ``GET ../own_delegated``.
    They are similar to ``total_staked`` and ``total_delegated``,
    except that they only consider tez owned by the baker itself. (MR
    :gl:`!14244`)

  * Renamed RPC ``GET ../unstaked_frozen_deposits`` to ``GET
    ../total_unstaked_per_cycle``. The old path is now
    **deprecated**. (MR :gl:`!14244`)

- In the following paths, ``../`` is short for
  ``/chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>/``.

  * Added RPC ``GET ../spendable`` which is identical to ``GET
    ../balance``. (MR :gl:`!14154`)

  * Added RPC ``GET ../spendable_and_frozen_bonds`` which is identical
    to ``GET ../balance_and_frozen_bonds``. (MR :gl:`!14154`)

- Reworked RPC ``GET
  /chains/<chain_id>/blocks/<block_id>/context/delegates/<baker_pkh>``;
  see :ref:`breaking_changes_beta`.

Errors
------

- The ``validate.operation.inconsistent_sources`` and
  ``validate.operation.inconsistent_counters`` errors have been
  expanded with information on the problematic sources and
  counters. The error messages have been updated accordingly, but the
  error IDs remain unchanged. (MR :gl:`!13138`)

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

Internal
--------

- ``balance_update_encoding_with_legacy_attestation_name`` has been removed.
  (MR :gl:`!13461`)

- Encoding that supports ``endorsement`` kind in apply_result JSON has been
  removed. (MR :gl:`!13974`)

- Encoding that supports ``endorsement`` kind in operation JSON has been
  removed. (MR :gl:`!13976`)

- Moved the RPC files ``contract_services.ml*`` and
  ``delegate_services.ml*`` from ``lib_protocol`` to
  ``lib_plugin``. (MR :gl:`!14094`)
