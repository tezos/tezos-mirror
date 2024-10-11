===========
DAL support
===========

The support for the :doc:`DAL <../shell/dal>` within the economic protocol relies on two operations:

#. ``DAL_publish_commitment``: a manager operation, allowing anyone to publish a DAL commitment
#. ``attestation``: the existing :ref:`consensus operation <consensus_operations>`, allowing bakers to attach a DAL payload attesting the data seen on the DAL P2P network

DAL publish commitment
======================

``DAL_publish_commitment`` is a manager operation that can be issued by a user wishing to publish data onto the DAL. The payload of this operation consists in the following fields:

- Slot index: Identifies the specific slot for which the data is being published. It is an integer between ``0`` and ``number_of_slots - 1``.
- Commitment: The `KZG commitment <https://dankradfeist.de/ethereum/2020/06/16/kate-polynomial-commitments.html>`__ over the data.
- Commitment proof: A proof that the commitment commits over data that does not exceed the size ``slot_size``.

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

When a commitment is published at a certain level, say level ``PL``, the corresponding DAL payloads are expected to be included in the attestations contained in the block at level ``PL + attestation_lag``.

Block metadata
--------------

In the block’s metadata, there is a specific field for the DAL. This field reflects the availability of slots based on the DAL payloads received. It is a bitfield with one bit per slot (its format is the same as the attestation payload of the ``attestation`` operation). The bit is set to 1 if the slot is declared available. The smallest slot index corresponds to the least significant bit. To consider a slot as available, there must be a minimum number of shards, as defined by the ``availability_threshold`` parameter, marked as available by the attesters for that slot (e.g. if the number of shards is 2048 and the availability threshold is 50%, then 1024 shards are required).

Therefore, for data committed (published) at level ``L``, the slot's availability is determined by the metadata of the block at level ``L + attestation_lag``. Consequently, a smart rollup can only utilize this data from level ``L + attestation_lag + 1`` onward.

Smart rollups integration
=========================

The DAL is integrated with :doc:`smart rollups <../active/smart_rollups>` so that kernels can request pages from the DAL via the :ref:`reveal data channel <reveal_data_channel_smart_rollups_quebec>`. A smart rollup can fetch any page from the DAL node if the commitment respects some conditions:

- The commitment should have been published after the rollup origination (this constraint will be leveraged so that the kernel can request any commitment in the past)
- The commitment should not have been published in a level in the future after the level of the next commitment of the state (at most 30 levels in the future).
  This constraint might be leveraged thanks to the internal messages ``Start of Level`` and ``End of Level`` (abbreviated as SOL and EOL), which allow the kernel to track the L1 level, so that the kernel cannot request a page for a commitment published after the current L1 level.

If the kernel requests a page that does not satisfy the mentioned conditions, then the rollup node must answer with an empty page indicating there is no data. Similarly, if the kernel requests for a commitment that was not attested, the rollup node must also provide an empty page. As a consequence, if a kernel requests a page for a commitment for which its availability is still unknown, the rollup node cannot answer and will have to wait for the availability status to be confirmed by the L1.

Moreover, the rollup kernel has access to the protocol constants so that the same kernel code can be used on different test networks.

.. _dal_constants_quebec:

DAL-related protocol constants
==============================

This section describes the protocol constants specific to the DAL as well as their default values on mainnet (see :ref:`protocol_constants_quebec` on how to find the values for tests networks):

- ``feature_enable`` (true): Whether the DAL is available
- ``incentives_enable`` (false): Whether baker incentives are available
- ``number_of_slots`` (32): How many slots are available per block
- ``attestation_lag`` (8): The timeframe for bakers to download shards between the published level of a commitment and the time they must attest the availability of those shards
- ``attestation_threshold`` (66): The percentage of shards attested for a given slot to declare the slot available
- ``blocks_per_epoch`` (1): Unused. Could be removed in the future
- ``page_size`` (3967B, ~4KiB): The size of a page (see :ref:`dal_slots`)
- ``slot_size`` (126944B, ~1MiB): The size of a slot (see :ref:`dal_slots`)
- ``redundancy_factor`` (8): The erasure-code factor (see :ref:`dal_slots`)
- ``number_of_shards`` (512): The number of shards per slot (see :ref:`dal_slots`)
