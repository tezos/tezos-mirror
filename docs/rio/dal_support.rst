===============
DAL integration
===============

The integration for the :doc:`DAL <../shell/dal>` within the economic protocol relies on three operations:

#. ``DAL_publish_commitment``: a manager operation, allowing anyone to publish a DAL commitment
#. ``attestation``: the existing :ref:`consensus operation <consensus_operations_rio>`, allowing bakers to attach a DAL payload attesting the data seen on the DAL P2P network
#. ``DAL_entrapment_evidence``: an anonymous operation to denounce a baker that has attested a trap shard

and on an :ref:`incentives scheme<DAL_incentives_scheme_rio>` for the DAL.

DAL publish commitment
======================

``DAL_publish_commitment`` is a manager operation that can be issued by a user wishing to publish data onto the DAL. The payload of this operation consists in the following fields:

- Slot index: Identifies the specific slot for which the data is being published. It is an integer between ``0`` and ``NUMBER_OF_SLOTS - 1``.
- Commitment: The `KZG commitment <https://dankradfeist.de/ethereum/2020/06/16/kate-polynomial-commitments.html>`__ over the data.
- Commitment proof: A proof that the commitment commits over data that does not exceed the size ``SLOT_SIZE``.

Users can create and manage these commitments and proofs through the :doc:`DAL node <../shell/dal_node>` using these RPCs:

- To create a commitment: ``POST /commitment``
- To retrieve a commitment’s proof: ``GET /commitments/<commitment>/proof``

Concurrent operations
---------------------

If a block contains a valid ``DAL_publish_commitment`` operation, any subsequent operations of the same kind within the same block (and the same slot index) will be recognized as valid but will fail during execution. They still incur transaction fees. More details about this can be found in the :doc:`validator documentation <../active/validation>`.

Economics
---------

Currently, the fees are estimated based on the execution cost of this operation alone. There are no additional charges related to the bandwidth required for bakers to download data from the DAL for this commitment. However, this might be subject to changes in the future.

DAL attestation payloads
========================

The attestation operation includes an optional field ``dal_content``. This field
allows attesters participating to the DAL to announce whether they were able to
successfully download the shards assigned to them. Concretely, this field is a
bitset reflecting the status of each slot. The size of the bitset corresponds to
the total number of slots. A value of 1 indicates successful retrieval of all
assigned shards by the baker for that slot, while 0 indicates an unsuccessful
attempt.  The least significant bit corresponds to the smallest slot index.

Attestation timing
------------------

When a commitment is published at a certain level, say level ``n``, the corresponding DAL payloads are expected to be included in the attestations contained in the block at level ``n + ATTESTATION_LAG``.

Block metadata
--------------

In the block’s metadata, there is a specific field for the DAL. This field reflects the availability of slots based on the DAL payloads received. It is a bitfield with one bit per slot (its format is the same as the attestation payload of the ``attestation`` operation). The bit is set to 1 if the slot is declared available. The smallest slot index corresponds to the least significant bit. To consider a slot as available, there must be a minimum number of shards, as defined by the ``AVAILABILITY_THRESHOLD`` parameter, marked as available by the attesters for that slot (e.g. if the number of shards is 2048 and the availability threshold is 50%, then 1024 shards are required).

Therefore, for data committed (published) at level ``n``, the slot's availability is determined by the metadata of the block at level ``n + ATTESTATION_LAG``. Consequently, a smart rollup can only utilize this data from level ``n + ATTESTATION_LAG + 1`` onward.

.. _DAL_incentives_scheme:
.. _DAL_incentives_scheme_rio:

DAL incentives scheme
=====================

Overview
--------

Bakers must meet a 64% minimal participation threshold in a cycle to earn a fixed percentage of the total participation rewards allocated for them.
As part of participation rewards, the DAL rewards are subject to the adjustments done by :ref:`Adaptive Issuance<adaptive_issuance_rio>`.

To ensure DAL attestations match the actual availability of data shards, there are special shards, known as *traps*, which are designed to test whether bakers have genuinely downloaded and processed their assigned shards. Bakers must correctly identify these traps to avoid losing the DAL rewards allocated to them.

Minimal participation
---------------------

Any baker that can take part in consensus is eligible for rewards.

Bakers meeting the minimum participation ratio ``MINIMAL_PARTICIPATION_RATIO`` over a cycle, set to 64%, receive rewards for that cycle.

The participation ratio of the baker is the proportion of slots the baker attested over the total number of slots deemed available by the protocol in the cycle.

DAL participation rewards
-------------------------

A fixed percentage, defined by a protocol parameter called ``REWARDS_RATIO``, set to 10%, of the total :ref:`participation rewards<adaptive_rewards_rio>` is allocated to the DAL.

The DAL rewards per level are implicitly given by their weight, ``DAL_REWARDS_WEIGHT``, as for the other types of :ref:`participation rewards<rewards_weights_rio>`.
The value of ``DAL_REWARDS_WEIGHT`` is such that it represents ``REWARDS_RATIO`` of all reward weights.

The rewards are distributed at the end of a cycle, and are computed in the same manner as for the other :ref:`participation rewards<adaptive_rewards_rio>`.
For instance, the stakers' share of these reward is proportional to the weight of their stake in relation to their baker's baking power.

Trap mechanism
--------------

A deterministic function ``trap(pkh, shard)`` returning a boolean flag indicates whether a shard is a trap for a specific baker identified by its public key hash (``pkh``).

The protocol parameter ``TRAPS_FRACTION`` controls the fraction of shards marked as traps.

Bakers detect traps by retrieving shard content via their DAL node and applying the trap function. A trap invalidates the corresponding attestation: the baker should not attest a slot if one of the slot’s shards assigned to him is a trap.

The ``DAL_entrapment_evidence`` accusation operation can be used to accuse a baker of wrongly attesting a slot due to an undetected trap.
This accusation operation includes the attestation operation containing the wrongly attested slot, the slot index, and the undetected shard.

As for double-signing accusations, any baker can include a DAL accusation in its block.
Accusations can be included during a period of ``DENUNCIATION_PERIOD`` cycles after the misbehavior event, which is that of the corresponding attestation operation.

Penalties
---------

A baker that is correctly accused, through an accusation operation included in a block, loses their DAL rewards for the cycle containing the block.

.. _dal_rollups_integration:
.. _dal_rollups_integration_rio:

Smart Rollups integration
=========================

The DAL is integrated with :doc:`smart rollups <../active/smart_rollups>` so that kernels can request pages from the DAL via the :ref:`reveal data channel <reveal_data_channel_smart_rollups_rio>`. A smart rollup can fetch any page from the DAL node if the commitment respects some conditions:

- The commitment should have been published after the rollup origination (this constraint will be leveraged so that the kernel can request any commitment in the past)
- The commitment should not have been published in a level in the future after the level of the next commitment of the state (at most 30 levels in the future).
  This constraint might be leveraged thanks to the internal messages ``Start of Level`` and ``End of Level`` (abbreviated as SOL and EOL), which allow the kernel to track the L1 level, so that the kernel cannot request a page for a commitment published after the current L1 level.

If the kernel requests a page that does not satisfy the mentioned conditions, then the rollup node must answer with an empty page indicating there is no data. Similarly, if the kernel requests for a commitment that was not attested, the rollup node must also provide an empty page. As a consequence, if a kernel requests a page for a commitment for which its availability is still unknown, the rollup node cannot answer and will have to wait for the availability status to be confirmed by the L1.

Moreover, the rollup kernel has access to the protocol constants so that the same kernel code can be used on different test networks.

.. _dal_constants:
.. _dal_constants_rio:

DAL-related protocol constants
==============================

This section describes the protocol constants specific to the DAL as well as their default values on mainnet (see :ref:`protocol_constants_rio` on how to find the values for tests networks):

- ``FEATURE_ENABLE`` (true): whether the DAL is available
- ``INCENTIVES_ENABLE`` (true): whether baker incentives are available
- ``NUMBER_OF_SLOTS`` (32): how many slots are available per block
- ``ATTESTATION_LAG`` (8 level): the timeframe for bakers to download shards between the published level of a commitment and the time they must attest the availability of those shards
- ``ATTESTATION_THRESHOLD`` (66%): the minimum percentage of shards attested for a given slot to declare the slot available
- ``PAGE_SIZE`` (3967B, ~4KiB): the size of a page (see :ref:`dal_slots`)
- ``SLOT_SIZE`` (126944B, ~128KiB): the size of a slot (see :ref:`dal_slots`)
- ``REDUNDANCY_FACTOR`` (8): the erasure-code factor (see :ref:`dal_slots`)
- ``NUMBER_OF_SHARDS`` (512): the number of shards per slot (see :ref:`dal_slots`)
- ``MINIMAL_PARTICIPATION_RATIO`` (64%): the minimum percentage of slots attested by a baker during a cycle (among all slots deemed available) that entitles them to rewards
- ``REWARDS_RATIO`` (10%): the ratio of the DAL rewards over the total participation rewards
- ``DAL_REWARDS_WEIGHT`` (2275): the weight of the DAL rewards (relative to other participation rewards)
- ``TRAPS_FRACTION`` (0.0005): the fraction of shards that are traps
