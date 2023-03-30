:orphan:

Tenderbake
==========

This page contains a summary of the changes brought by the Tenderbake merge request (:gl:`!3738`).
Please refer to :doc:`the documentation<../alpha/consensus>` and to this `blog post <https://research-development.nomadic-labs.com/a-look-ahead-to-tenderbake.html>`_ for an overview of the Tenderbake consensus algorithm and its motivation.

Protocol
--------

The whole incentives scheme has been updated.
In particular the security deposit mechanism has been overhauled.
See :doc:`the documentation<../alpha/consensus>`.

Rolls do not play an essential role anymore, in that the computation of delegates' rights is based directly on the delegates' stake.
Snapshots are still used: it is the entire delegate's stake that is snapshot.
Rolls still play the following roles:

- A delegate can participate in consensus (receiving baking and endorsing rights) if it has at least ``tokens_per_roll`` tez.

- A delegate's voting power in the governance process is still in terms of rolls, as in Hangzhou.

Baking and endorsing rights are no longer independent of each other:
delegates that participate in consensus at a given level are called
:ref:`validators<tb_validator>` in Tenderbake and validator rights in
Tenderbake are similar to endorsing rights in Emmy*. Baking rights are
deduced from endorsing rights: the baker at round ``r`` is the
validator that owns the endorsing slot ``r``.

Operations
~~~~~~~~~~

The layout of the endorsement operations has changed. It consists of a level, a round, a slot, and a payload hash.
A validator emits at most one endorsement per round, but can emit more
endorsements per level; therefore, the current high-water mark
mechanism used by the signer has been adapted (see :ref:`Signer<signer_chgs>`).

There is a new consensus operation, ``preendorsement``, with the same layout as an endorsement.

There is a new anonymous operation, ``double preendorsement evidence``, with the same layout as a double endorsement evidence.

There is a new manager operation, ``set deposits limit``, which takes an
optional positive integer argument. When the limit argument is given,
the given limit on the signer's security deposit is set. When the
limit argument is not present, the previous limit is unset and no
limit is imposed.

The ``branch`` field of non-consensus operations is set by default by
the Octez client to ``HEAD~2``. Setting the ``branch`` field to
``HEAD`` or ``HEAD~1`` may result in the operation not being included
because it will not be anchored on a block belonging to the
chain. (The blocks at the current and previous levels are not
necessary final.)

Block headers
~~~~~~~~~~~~~

The block fitness (included in the shell part of the block header)
changes, mainly to allow block
:ref:`candidates<candidate_block>` to be accepted by nodes.

The protocol part of the block header changes as follows:

- it no longer contains the priority entry.
- it contains two additional fields:

 - ``payload_hash`` is the hash of the sequence of the block's
   non-consensus operations
 - ``payload_round`` is the round at which the block's payload has
   been first proposed (at the current level); it is equal to the
   block's round in case of a fresh
   :ref:`proposal<candidate_block>` and it is strictly smaller
   in case of a reproposal.

As in Emmy*, a block includes a set of endorsements for the
predecessor block. These endorsements constitute a
:ref:`quorum<quorum>` and serve as a justification that the previous
block has been agreed upon. In case of a reproposal, a block also
includes a quorum of preendorsements for the same round to justify
that the payload is indeed a reproposal.

Parameters
~~~~~~~~~~

The following protocol parameters have been removed:

* ``time_between_blocks``
* ``endorsers_per_block``
* ``blocks_per_roll_snapshot`` (it is replaced by ``blocks_per_stake_snapshot``)
* ``block_security_deposit``
* ``endorsement_security_deposit``
* ``baking_reward_per_endorsement``
* ``endorsement_reward``

The following protocol parameters have been introduced:

* ``baking_reward_fixed_portion`` = 10 tez
* ``baking_reward_bonus_per_slot`` = 0.004286 tez
* ``endorsing_reward_per_slot`` = 0.002857 tez
* ``delay_increment_per_round`` = 15
* ``minimal_participation_ratio`` = 2/3
* ``consensus_committee_size`` = 7000
* ``consensus_threshold`` = 4667
* ``max_slashing_period`` = 2 cycles
* ``frozen_deposits_percentage`` = 10
* ``double_baking_punishment`` = 640 tez
* ``ratio_of_frozen_deposits_slashed_per_double_endorsement`` = 1/2

The parameter ``minimal_block_delay`` is reused to specify the duration of round 0.

The values of the following protocol parameters has changed:

* ``tokens_per_roll`` has changed from 8000 to 6000 tez.


Metadata
~~~~~~~~

The receipt of a block has a new field: ``proposer``, which is the
public key hash of the block's payload
producer. :doc:`Recall<../alpha/consensus>` that the payload producer
may be different from the block producer, which is stored in the field
``baker``.

Previously, some internal transfers of tokens did not generate balance updates. Also, a credit balance update was not always balanced by a debit balance update. In order to make token movements easier to audit, we have remedied that by introducing new 'kinds' and 'types' of balance updates. Hence, balance updates in the metadata are now always balanced, i.e. the sum of credits is equal to the sum of debits.

The balance updates have been updated as follows:

- The new balance categories ``legacy_rewards``, ``legacy_deposits``, and
  ``legacy_fees`` correspond to the old ``rewards``, ``deposits``, and
  ``fees`` categories, and are only generated during migration (their
  ``origin`` field is ``migration``).

- There is a new type of ``origin``, called ``simulation`` which is for internal use only (when a smart contract simulation run is performed via the RPC ``../helpers/scripts/run_code``). This ``origin`` will not appear in metadata during normal operation on mainnet.

- The following new balance types have been introduced:

  - deposits, with the kind ``freezer``, category ``deposits``, and 3rd field ``delegate``;
  - nonce revelation rewards, with the kind ``minted`` and category ``nonce revelation rewards``;
  - double signing evidence rewards, with the kind ``minted`` and category ``double signing evidence rewards``;
  - endorsing rewards, with the kind ``minted`` and category ``endorsing rewards``;
  - baking rewards, with the kind ``minted`` and category ``baking rewards``;
  - baking bonuses, with the kind ``minted`` and category ``baking bonuses``;
  - block fees, with the kind ``accumulator`` and category ``block fees``;
  - storage fees, with the kind ``burned`` and category ``storage fees``;
  - double signing punishments, with the kind ``burned`` and category ``punishments``;
  - lost endorsing rewards, with the kind ``burned``, category ``lost endorsing rewards``, 3rd field ``delegate``, 4th field ``participation`` (a boolean with value ``true`` if and only if the reward was lost because of insufficient participation), and 5th field ``revelation`` (a boolean with value ``true`` if and only if the reward was lost because of unrevealed nonces);
  - liquidity baking subsidies, with the kind ``minted`` and category ``subsidy``;
  - commitments, with the kind ``commitment`` and category ``commitment``;
  - invoices, with the kind ``minted`` and category ``invoice``;

- The following new balance types are for internal use, they will not appear in the metadata during normal operation on mainnet,
  but may appear on test networks, or in sandboxed mode:

  - "bootstrap" with the kind ``minted`` and category ``bootstrap``;
  - "initial commitments", with the kind ``minted`` and category ``commitment``;
  - "burned", with the kind ``burned`` and category ``burned``;
  - "minted", with the kind ``minted`` and category ``minted``

- The following balance types represent external sources of tokens that can only be debited :
  nonce revelation rewards, endorsing rewards, baking rewards, baking bonuses, liquidity baking subsidies, invoices,
  initial commitments, bootstrap, minted

- The following balance types represent destinations of tokens burned that can only be credited :
  storage fees, double signing punishments, lost endorsing rewards, burned

The receipt for (pre)endorsement operations contains three fields:

- ``balance_updates``, which is always empty;
- ``delegate``, the signer's public key hash;
- ``(pre)endorsement_power``, the number of slots the delegate had at the corresponding level.

The receipt for double preendorsement evidence operations has the same format as for double endorsement evidence operations.

The receipt for set deposits limit operations has one field: the ``consumed_gas``.

A more detailed documentation of balance updates is available :doc:`here<../alpha/token_management>`.

RPCs
----

The following RPCs have been removed or renamed:

- ``../minimal_valid_time`` has been removed
- ``../context/delegates/<pkh>/frozen_balance_by_cycle`` has been removed
- ``../context/delegates/<pkh>/frozen_balance``, has been renamed to ``frozen_deposits``
- ``../context/delegates/<pkh>/balance``, renamed to ``full_balance``

The following RPCs have changed:

- ``../helpers/baking_rights``:

  - Instead of an optional list of ``cycle`` arguments, the RPC only takes one optional ``cycle`` argument.
  - The argument ``max_priority`` has been renamed to ``max_round``.
  - The output field ``priority`` has been renamed to ``round``.

- ``../helpers/endorsing_rights``:

  - Instead of an optional list of ``cycle`` arguments, the RPC only takes one optional ``cycle`` argument.
  - The output is now grouped per level. For each level, the output
    contains the delegates' rights and the estimated time at which the
    rights can be exercised. For each delegate that has some rights at
    the given level, the output contains the delegate's public key
    hash, the delegate's first slot, and the delegate's endorsing power.


The following RPCs are new:

- ``../helpers/round``: gives the round of a block.

- ``../helpers/validators``: is a variant of ``endorsing_rights`` RPC, used by the Octez baker daemon.

- ``../context/delegates/<pkh>/current_frozen_deposits``: gives the
  current amount of the delegate's frozen deposits, in contrast to
  ``../<pkh>/frozen_deposits`` which returns the initial amount (that
  is, at the beginning of a cycle) of the frozen deposits. The two
  amounts are different only when the delegate has been punished.

- ``../context/delegates/<pkh>/frozen_deposits_limit``: gives the frozen deposits limit of a registered delegate.

- ``../context/delegates/<pkh>/participation``: gives information on the participation (in consensus) of a registered delegate, as follows:

  - ``expected_cycle_activity`` indicates the number of endorsing
    slots the delegate is expected to have in the cycle based on its
    active stake. This number does not necessary equal the number of
    slots the delegate actually has, which are also dependent on the
    cycle's seed.

  - ``minimal_cycle_activity`` indicates the minimal endorsing slots
    in the cycle required to get endorsing rewards. It is computed
    based on the ``expected_cycle_activity``.

  - ``missed_slots`` indicates the number of missed endorsing slots in the cycle so far.

  - ``missed_levels`` indicates the number of missed levels for endorsing in the cycle so far.

  - ``remaining_allowed_missed_slots`` indicates the remaining amount
    of endorsing slots that can be missed in the cycle before
    forfeiting the rewards.

  - ``expected_endorsing_rewards`` indicates the endorsing rewards
    that will be distributed at the end of the cycle if activity at
    that point will be greater than the minimal required; if the
    activity is already known to be below the required minimum, then
    the rewards are zero.


.. _signer_chgs:

Signer
------

The signer's messages were of the form
``<magic_byte><chain_id><block|endorsement>`` and are now of the form
``<magic_byte><chain_id><block|preendorsement|endorsement>``, where
the magic byte has changed from ``0x01`` for blocks and ``0x02`` for
endorsements, to ``0x11`` for blocks, ``0x12`` for preendorsements,
``0x13`` for endorsements.

The high-water mark for blocks and (pre)endorsements is now given by
both the level and the round of the signed block, respectively of the
signed (pre)endorsement. The signer is authorized to sign whenever the
level is strictly higher than the previous level, or the level is the
same, but the round is strictly higher.


Daemons
-------

There is no endorser daemon anymore. Its role is performed by the baker daemon.
The baker daemon takes the same options as in Hangzhou.


Client
------

The command ``octez-client bake for`` has been changed:

- It takes a (possibly empty) list of delegate references. It then bakes a block and (pre)endorses this block, using the rights of all the specified delegates. When the list is empty is does so for all delegates whose secret keys are known.
- It performs a full consensus round: it "proposes" a block (that is, it injects a block candidate), it preendorses the block, and it endorses the block, if possible.

The following commands have been added:

- ``octez-client propose for``: forge and inject a candidate block (a ``proposal``).

- ``octez-client preendorse for``: forge and inject a preendorsement operation.

- ``octez-client endorse for``: forge and inject an endorsement operation.

- ``octez-client set deposits limit for <src> to <deposits_limit>``: sets the deposits limit for a registered delegate.

- ``octez-client unset deposits limit for <src>``: remove the deposits limit of a registered delegate.
