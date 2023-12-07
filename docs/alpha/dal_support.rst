===========
DAL support
===========

The support for the :doc:`DAL <../shell/dal>` within the economic protocol relies on two operations:

#. ``DAL_publish_slot_header``: Allowing anyone to publish a DAL commitment
#. ``DAL_attestation``: Allowing bakers to attest the data seen onto the DAL P2P network

DAL publish slot header
=======================

``DAL_publish_slot_header`` is a manager operation that can be issued by a user wishing to publish data onto the DAL. The payload of this operation consists in the following fields:

- Slot index: Identifies the specific slot for which the data is being published. It is an integer between ``0`` and ``number_of_slots - 1``.
- Commitment: The `KZG commitment <https://dankradfeist.de/ethereum/2020/06/16/kate-polynomial-commitments.html>`__ over the data.
- Commitment proof: A proof that the commitment commits over data that does not exceed the size ``slot_size``.

Users can create and manage these commitments and proofs through the :doc:`DAL node <../shell/dal_node>` using these RPCs:

- To create a commitment: ``POST /commitment``
- To retrieve a commitment’s proof: ``GET /commitments/<commitment>/proof``

Concurrent operations
---------------------

If a block contains a valid ``DAL_publish_slot_header`` operation, any subsequent operations of the same kind within the same block (and the same slot index) will be recognized as valid but will fail during execution. They still incur transaction fees. More details about this can be found in the :doc:`validator documentation <../active/validation>`.

Economics
---------

Currently, the fees are estimated based on the execution cost of this operation alone. There are no additional charges related to the bandwidth required for bakers to download data from the DAL for this commitment. However, this might be subject to changes in the future.

DAL attestation
===============

``DAL_attestation`` is a new :ref:`consensus operation <consensus_operations>` performed by attesters. It serves to verify whether they were able to successfully download the shards assigned to them. The payload of this operation consists of the following fields:

- Level: This refers to the level of the block that is being attested, specifically the level preceding the block that contains the ``DAL_attestation`` operation.
- Tenderbake slot: This element identifies the attester, in particular, it determines the public key required for signature verification and the assigned  DAL shards.
- Attestation: This is a bitset reflecting the status of each slot. The size of the bitset corresponds to the total number of slots. A value of 1 indicates successful retrieval of all assigned shards by the baker for that slot, while 0 indicates an unsuccessful attempt.
  The lowest field of the bitset corresponds to the lowest slot index.

Attestation timing
------------------

When a commitment is published at a certain level, say level ``PL``, the corresponding DAL attestations are expected to be included in the block at level ``PL + attestation_lag``.

Block metadata
--------------

In the block’s metadata, there is a specific field for the DAL. This field reflects the availability of slots based on the DAL attestations received. It is a bitfield with one bit per slot (its format is the same as the attestation part of the ``DAL_attestation`` operation). The bit is set to 1 if the slot is declared available. The lowest slot index corresponds to the lowest bit. To consider a slot as available, there must be a minimum number of shards, as defined by the ``availability_threshold`` parameter, marked as available by the attesters for that slot (e.g. if the number of shards is 2048 and the availability threshold is 50%, then 1024 shards are required).

Therefore, for data committed (published) at level ``L``, the slot's availability is determined by the metadata of the block at level ``L + attestation_lag``. Consequently, a smart rollup can only utilize this data from level ``L + attestation_lag + 1`` onward.

Smart rollups integration
=========================

The DAL is integrated with :doc:`smart rollups <../active/smart_rollups>` so that kernels can request pages from the DAL via the :ref:`reveal data channel <reveal_data_channel_smart_rollups>`. A smart rollup can fetch any page from the DAL node if the commitment respects some conditions:

- The commitment should have been published after the rollup origination (this constraint will be leveraged so that the kernel can request any commitment in the past)
- The commitment should not have been published in a level in the future after the level of the next commitment of the state (at most 30 levels in the future).
  This constraint might be leveraged thanks to the internal messages ``Start of Level`` and ``End of Level`` (abbreviated as SOL and EOL), which allow the kernel to track the L1 level, so that the kernel cannot request a page for a commitment published after the current L1 level.

If the kernel requests a page that does not satisfy the mentioned conditions, then the rollup node must answer with an empty page indicating there is no data. Similarly, if the kernel requests for a commitment that was not attested, the rollup node must also provide an empty page. As a consequence, if a kernel requests a page for a commitment for which its availability is still unknown, the rollup node cannot answer and will have to wait for the availability status to be confirmed by the L1.

Moreover, the rollup kernel has access to the protocol constants so that the same kernel code can be used on different test networks.

.. _dal_constants_alpha:

DAL-related protocol constants
==============================

This section describes the protocol constants specific to the DAL as well as their default values on mainnet (see :ref:`protocol_constants` on how to find the values for tests networks):

- ``feature_enable`` (false): Whether the DAL is available
- ``number_of_slots`` (256): How many slots are available per block
- ``attestation_lag`` (4): The timeframe for bakers to download shards between the published level of a commitment and the time they must attest the availability of those shards
- ``attestation_threshold`` (50): The percentage of shards attested for a given slot to declare the slot available
- ``blocks_per_epoch`` (1): Unused. Could be removed in the future
- ``page_size`` (4KiB): The size of a page (see :ref:`dal_slots`)
- ``slot_size`` (1MiB): The size of a slot (see :ref:`dal_slots`)
- ``redundancy_factor`` (16): The erasure-code factor (see :ref:`dal_slots`)
- ``number_of_shards`` (2048): The number of shards per slot (see :ref:`dal_slots`)
