The Octez Baker
===============


The Octez baker is an executable responsible for participating in the Tezos
blockchain consensus by producing blocks and creating consensus operations on
behalf of a set of delegated accounts.

Consensus Operations
--------------------

The baker plays a central role in the :doc:`consensus<../active/consensus>` protocol. When a new block is validated by the node, the baker receives this proposal and initiates the consensus mechanism.
The baker may have injected the block itself, if it was its turn to do so.

First, the baker ensures that the proposal is valid — meaning that the proposed block has the correct level and round, and meets other protocol criteria. Once validated, the baker begins the voting process.

Tezos uses a two-phase voting system for consensus:

Preattestation Phase
^^^^^^^^^^^^^^^^^^^^

The baker produces preattestation operations for the proposal on behalf of each delegate it manages. These preattestations are injected into the node and propagated through the network. At the same time, the baker monitors the node's mempool for preattestations from other bakers.

For each valid preattestation received, the baker retrieves the delegate’s baking power and updates a prequorum state. Once the baker observes preattestations representing at least 66% of the total baking power, a prequorum is considered reached, and the second voting phase begins.

Attestation Phase
^^^^^^^^^^^^^^^^^

Before attesting, the baker queries the DAL node to determine whether any DAL slots should be attested. It then crafts an attestation operation, optionally including this DAL data. Like with preattestations, the baker monitors attestations from other bakers.

If attestations representing at least 66% of the total baking power are observed, the proposal block is elected. This means that a sufficient number of delegates have confirmed the block, and the chain can proceed to the next level.

For more details, see :doc:`../active/consensus`.

Block Production
----------------

Blocks are produced at regular intervals, defined by the block_delay parameter.
The baker is responsible for producing a block when one of its delegates has
baking rights for that level and round. (See the :doc:`Baking power<../active/baking_power>` documentation for details.)

When the previous proposal block is elected, the baker can construct a new block at the next level. This block must include a proof of consensus for the elected block — essentially, a set of attestations supporting it.

Next, the baker selects operations from the node’s mempool. These operations include:

- Voting operations (e.g., protocol proposals and ballots)

- Anonymous operations (e.g., nonce revelations, denunciations)

- Manager operations (e.g., token transfers, smart contract calls)

Since a block has limits — such as the maximum allowed gas — the baker must solve an optimization problem (akin to the Knapsack Problem) to choose which operations to include.

Once the operations are selected, the baker creates a block header containing
metadata such as the block’s level, operation hashes, fitness, and more (see the
:ref:`shell_header` documentation and protocol-specific block details). After the header is signed, the block is ready to be injected into the node.

To ensure timely block injection, the baker performs all of these steps ahead of
time, so the block can be submitted exactly at the scheduled time.

Once injected, the block is validated by the node and propagated through the
network. Other bakers then begin the attestation process for a new block,
continuing the cycle of consensus.

Support for ``tz4`` (BLS) Keys
------------------------------

Tezos supports the use of :ref:`tz4 keys <tz4_accounts>` using BLS cryptography (starting with protocol Seoul) as consensus keys for delegates. This enables (pre)attestation aggregation, a feature that significantly improves efficiency by allowing many (pre)attestations to be compressed into a single operation within blocks.

BLS (Boneh–Lynn–Shacham) signatures have the unique property that multiple signatures over the same payload can be aggregated into a single signature. This makes them ideal for consensus, where many bakers attest to the same block. However, for this to work efficiently, all (pre)attestations must have identical payloads.

This becomes a challenge when DAL (Data Availability Layer) data is involved. DAL attestations are included as part of the regular Layer 1 attestation operation, and the set of DAL slots attested to is different for each delegate. This means the payloads differ, which prevents simple BLS aggregation.

The Role of the Companion Key
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To enable aggregation even when DAL slot data is included, a separate :ref:`companion_key` is used (introduced in protocol Seoul):

- This is a second BLS key, separate from the consensus key.

- The consensus key signs the common part of the attestation (the block reference and quorum vote).

- The companion key signs the delegate-specific DAL payload (the set of DAL slots being attested).

Both signatures, one from the consensus key and one from the companion key, are
aggregated and included in the attestation.
When a baker produces the next block, they aggregate all ``tz4`` attestations
(including both already aggregated signatures) into a single aggregated attestation.
