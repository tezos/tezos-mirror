Bakers & the DAL
================

This page documents some aspects that are most useful to know for bakers when participating to the DAL network (baking the DAL is a serious matter!).
We assume the reader is familiar with the DAL basics and terminology introduced in :doc:`./dal_overview` and :doc:`./dal_slots`.

Bakers play a crucial role in Tezos through the Tenderbake protocol, by validating transactions and securing the network. This participation involves producing new blocks when assigned the rights by the protocol, attesting proposed valid blocks to make the chain progress, and (indirectly) slashing misbehavior that would put the chain at risk. Due to their strategic role in Tezos, entrusting them with verifying and attesting data availability using the DAL is natural.

From a baker's point of view, DAL attestation is carried out in three main steps.

First, the baker is attributed DAL attestation rights (automatically, based on its consensus attestation rights). Similar to Tenderbake's baking and attestation rights, the L1 protocol assigns a range of DAL shards to bakers. For a given level, the list of shards attributed to a baker is the same for every DAL slot.

Second, Tezos bakers should launch DAL nodes and join the DAL P2P network subscribing to the appropriate topics. The topics of interest for a baker are those of the form ``(tz1TheBakerSPublicKeyHash, slot_index)`` where the first component is the baker's public key hash and the slot index ranges from ``0`` to ``number of DAL slots - 1``. Note that the baker binary automatically subscribes to those topics when launched with ``--dal-node <dal-node-RPC-endpoint>``. Once launched and connected to the baker's binary, the DAL node is responsible for downloading the shards assigned to the corresponding public key hash(es) for every slot via the DAL P2P network, assuming the slot commitment operation has been accepted by L1 and the slot's shards are published on the DAL/P2P (see steps 3 and 4 in :doc:`dal_overview`).

Finally, just like for Tenderbake pre-attestations or attestations, bakers inject DAL attestations after every L1 block to inform the protocol, for each slot, whether they managed to download all assigned shards or not. Concretely, this new consensus operation carries a bitset, where the bit at position ``n`` is set to ``1`` if and only if the DAL node connected to the baker managed to download all the shards assigned to that baker for the slot number ``n``.

Based on the DAL attestations included in a block , the protocol decides which slots are available and announces them in that block's metadata.

It's important for bakers to note the following:

- No DAL attestation is injected if there is no slot to attest (i.e. if the bitset is zero);
- If a slot header is included in an L1 block at some level PL (a.k.a ``published_level``), the DAL consensus operations that possibly attest its shards are injected on top of the block(s) at level ``PL + attestation_lag - 1`` and target inclusion at level ``PL + attestation_lag``;
- Theoretically, if a baker is assigned more than ``number_of_shards / redundancy_factor`` shards, it could declare the shards as available if it succeeds in downloading ```number_of_shards / redundancy_factor` shards at least. The missing shards could be calculated by reconstructing the whole slot (but this is not implemented yet);
- It is planned to merge Tenderbake and DAL attestations in the same operation in the future, as they share a few fields. This would avoid an extra signature verification per baker;
- As the number of shards is quite small compared to the number of Tenderbake slots, DAL and Tenderbake committees might not always coincide in practice: there might be bakers in the Tenderbake's committee that are not part of the DAL's for some levels. However, any baker in the DAL committee is part of the Tenderbake committee;
- Like Tenderbake attestations, bakers receive a proportionate number of shards to attest based on their staking balance, directly impacting the required bandwidth to download shards' content at each level. A baker with 1% of the stake will need a bandwidth between 5-10MiB/s. This is why we plan to give the opportunity to bakers to run multiple DAL nodes so that this bandwidth can be spread between those DAL nodes.
- With the DAL, it becomes easier to associate the baker identity with an IP address. Besides, the more stake a baker has, the easier it is. Fortunately, bakers could run their DAL nodes on distinct machines to mitigate possible DDoS attacks on their bakery infrastructure.
