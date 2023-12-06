The DAL node
============

A DAL node is an executable (called ``octez-dal-node``) whose main roles are to publish, store, and exchange data for the :doc:`DAL layer <./dal_overview>`. Interacting with a DAL node is done through the command-line interface (CLI) and through RPCs. In what follows, we describe the main components and features of a DAL node, and then present some operational aspects on configuring and running a DAL node.

Concepts and features
---------------------

DAL P2P network
^^^^^^^^^^^^^^^

The ``octez-dal-node`` executable runs a node in the DAL’s P2P network. Recall that :doc:`the DAL's P2P protocol <./dal_p2p>` is based on a gossip algorithm for distributing shards, running on top of a networking layer using the same p2p library as L1 nodes.

Actors with various roles may need to run the DAL node, for instance bootstrap DAL node operators, slot producers, bakers.

Bootstrap DAL nodes represent DAL network entry points. Their only role is to provide a way for other DAL nodes to become part of the DAL P2P network. A bootstrap node remains connected to a large number of peers, and is subscribed to all topics. It can thus provide another node with a list of peers to connect to for each of the topics that node is interested in.

Non-bootstrap DAL nodes distinguish themselves only in the topics they subscribe to, for instance:

- Slot producers subscribe to all topics containing some specified slot indexes.
- Bakers subscribe to all topics containing the attester identities they run for (for all possible slot indexes).

Profiles
~~~~~~~~

These differences in the behavior of a DAL node are specified using **profiles**. There are therefore three profiles: bootstrap, producer, and attester profiles. These can be given in the node’s configuration file, or as CLI arguments to the node’s commands, or via RPCs.
The attester and producer profiles can be combined, the node will then subscribe to the union of the corresponding topics, while the bootstrap profile is incompatible with the other profiles.

Storage
^^^^^^^

The DAL node essentially stores slots and shards. Slots are injected into the node through an RPC (see details at :ref:`slots_lifetime`), at which moment the corresponding commitment is computed and stored. Shards and their proofs are computed and stored via another RPC. It is important to also compute the shards’ proofs, because shards can be exchanged over the P2P network only if they are accompanied by their proof. Shards received over the P2P network are also stored. The node also tracks and stores the status of commitments by monitoring the L1 chain, connecting to this end to an L1 node specified at startup.

The size of the node’s storage depends on its profile. A bootstrap node uses negligible storage. A DAL node with a producer profile stores in the order of tens MiB per slot, per level. Note that this translates to at least 100GiB per day per slot. A DAL node with an attester profile with the attester having a stake fraction of 1% stores an order of magnitude less data per level.

L1 monitoring
^^^^^^^^^^^^^

The DAL node tracks the L1 chain, in particular it monitors the heads of the L1 chain via an RPC. At each final block, it retrieves and stores the newly published commitments, and if its storage contains shards for these commitments, it publishes them on the P2P network. The DAL node also retrieves the slot attestation status from the block’s metadata. Finally, based on this information it updates the status of the stored commitments, where the status of a commitment can be one of the following:

- *waiting for attestation*: The commitment was included and applied in a finalized L1 block but the corresponding slot remains to be attested.
- *attested*: The commitment was included in an L1 block and the corresponding slot is attested.
- *unattested*: The commitment was included in an L1 block but the corresponding slot was not timely attested.
- *not selected*: The commitment was included in an L1 block but was not selected for that slot index. This can happen if there is another commitment in the same block for the same slot index that was placed before the commitment in question (in operation order) by the baker.
- *unseen or not finalized*: The commitment was not seen in a final L1 block. For instance, this could happen if the RPC ``PATCH /commitments/<commitment>`` was called but the corresponding commitment was never included into a block; or the commitment was included in a non-final block. This means that the publish operation was not sent (yet) to L1, or sent but not included (yet) in a block, or included in a not (yet) final block.

RPC server
^^^^^^^^^^

The DAL node incorporates an RPC server which answers to RPC queries to update or retrieve information about the node’s state. For instance, one can post slots, ask the node to compute and store shards, to update profiles, or to connect or disconnect to peers. For the full list of the DAL node’s RPC see :doc:`./dal_rpc`.

Operational aspects
-------------------

Running a DAL node
^^^^^^^^^^^^^^^^^^

The DAL node has two commands ``config init`` and ``run``.

The command ``init config`` creates a new configuration file in the specified data directory or in the default location (ie ``~/.tezos-dal-node``) with the parameters provided on the command-line by the corresponding arguments, in case no configuration file exists already. If such a file already exists, it overrides it with the provided parameters (old parameters are lost).

The command ``run`` runs the DAL node. The CLI arguments take precedence over the configuration file arguments, except for the list of bootstrap peers and of profiles, which are considered in addition to the ones from the configuration file. The configuration file is however not overridden with the new values of the node’s parameters. However, at the end of the execution, the node’s profiles, which may have been given as arguments or set via RPCs, are written to the configuration file.

See the :ref:`DAL node manual <dal_node_manual>` for more details.

DAL configuration of the L1 node
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

All DAL nodes should use the same initialization parameters of the cryptographic primitives used by the DAL. These parameters are provided by running the script :src:`scripts/install_dal_trusted_setup.sh`, which downloads and installs them, and which should be run once by any DAL node operator. However, for simplicity, on some test networks the initialization parameters are mocked-up and built-in.

Also, in order for the nodes to be able to join the P2P network, a set of bootstrap nodes can be provided using the ``network.dal_config.bootstrap_peers`` configuration parameter of the L1 node (thus using the same mechanism as for L1 nodes, see :doc:`../user/multinetwork` and :ref:`configure_p2p`).

Monitoring
^^^^^^^^^^

As for L1 nodes, to monitor the DAL node, one can inspect the node’s logs, query the node through RPCs, or use the incorporated Octez Metrics server. The usage of metrics is analogous to that for the L1 nodes, and we therefore refer to the :doc:`L1 node’s documentation <../user/node-monitoring>` for details.

The bootstrap mechanism
^^^^^^^^^^^^^^^^^^^^^^^

The gossipsub algorithm establishes upper bounds on the size of a topic mesh. Whenever a peer B would like to join the mesh of a peer A, but the peer A cannot add B to its mesh due to the upper bound, peer A responds with a random list of peers it knows are subscribed to the same topic. Peer B will then try to connect to these advertised peers. This peer exchange mechanism is used to bootstrap the P2P network: for bootstrap DAL nodes, the upper bound is set to 0, and thus, while they do not participate in exchanging shards, they keep advertising known peers so that new peers can connect to them and thus enter the P2P network.


If a node is behind a NAT, it is important to make the distinction between its "public address" and its "network address". It is the public address that is advertised, and this should be the node’s external address, while the network address should be the node’s internal address. In other words, the public address behaves as the ``--advertised-net-port`` option of the L1 node, see :ref:`private-mode`.


.. _slots_lifetime:

Lifetime of slots and shards
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The life cycle of slots and shards is described by the following steps:

#. The slot producer posts the slot data to some DAL node of its choice. The node computes the corresponding commitment and adds the association commitment - slot to the store.
   This is done via the RPC ``POST /commitments/<slot_data>``, which returns the corresponding commitment.
#. The slot producer instructs the DAL node to compute and save the shards of the slot associated with the given commitment.
   It is important to set the query flag ``with_proof`` to true to be able to publish the shards on the P2P network.
   This is done via the RPC ``PUT /commitments/<commitment>/shards``.
#. The slot producer instructs the DAL node to compute the proof associated with the commitment.
   This is done via the RPC ``GET /commitments/<commitment>/proof``, which returns the corresponding commitment proof.
#. The slot producer selects a slot index for its slot, and posts the commitment to L1, via the ``publish_slot_header`` operation.
   This can be done via RPCs for injecting an operation into L1, or using the Octez client, via the following command::

     octez-client publish dal commitment <commitment> from <pkh> for slot <slot_index> with proof <proof>

#. Once the operation is included in a final block (that is, there is another block on top of the one including the operation), and the slot is considered published (see :doc:`./dal_overview`), all DAL nodes exchange the slot’s shards they have in their store on the P2P network, depending on their profile (see :doc:`./dal_p2p`), and they store previously unknown shards.
#. Attesters check, for all published slots, the availability of the shards they are assigned by interrogating their DAL node, via the RPC ``GET /profiles/<pkh>/attested_levels/<level>/attestable_slots``, where level is the level at which the slot was published plus ``attestation_lag``, and ``pkh`` is the attester’s public key hash. (See also :doc:`dal_bakers`)
#. Attesters inject via their baker binary a DAL attestation operation containing the information received at step 6. (See also :doc:`dal_bakers`)
#. The protocol aggregates the received DAL attestations, and declares each published slot as available or unavailable, depending on whether some threshold is reached, via the blocks metadata.
#. Rollups and other users can request stored pages or shards for an attested slot from any DAL node via the RPCs ``GET /slot/pages/<commitment>`` or ``GET /shards/<commitment>`` respectively. Only nodes that store enough shards to reconstruct the slot can provide the requested pages.

Step 2 can be done in parallel with steps 3-4, but before step 5.
