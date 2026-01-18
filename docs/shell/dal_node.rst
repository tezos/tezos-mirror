The DAL node
============

A DAL node is an executable (called ``octez-dal-node``) whose main roles are to publish, store, and exchange data for the :doc:`DAL layer <./dal_overview>`. Interacting with a DAL node is done through the command-line interface (CLI) and through RPCs. In what follows, we describe the main components and features of a DAL node, and then present some operational aspects on configuring and running a DAL node.

Concepts and features
---------------------

DAL P2P network
^^^^^^^^^^^^^^^

The ``octez-dal-node`` executable runs a node in the DAL’s P2P network. Recall that :ref:`the DAL's P2P protocol <dal_p2p>` is based on a gossipsub algorithm for distributing shards, running on top of a networking layer using the same P2P library as L1 nodes.

Actors with various roles run DAL nodes, but they need different views of the network depending on their role:

- Operators of Smart Rollups care about data on the slots that their Smart Rollups use, but not about the slots used by other rollups.
- Bakers only care about the shards that they are assigned to attest to, but they must watch all slots because the assigned shards can be in any slot.
- Bootstrap node operators care about other nodes connecting to peers that are interested in the same messages via the P2P network but don't care about the content of the messages on the network or any specific slot or shard more than any other.

Non-bootstrap DAL nodes distinguish themselves only in the topics they subscribe to:

- Operators subscribe to all topics containing some specified slot indexes.
- Bakers subscribe to all topics containing the attester identities they run for (for all possible slot indexes).

Bootstrap nodes are DAL network entry points that are used for network discovery.
A bootstrap node remains connected to a large number of peers and is subscribed to all topics.
When a DAL node starts, it gets the URLs of the bootstrap nodes from its Layer 1 node and uses these bootstrap nodes to connect to peers.
When a DAL node does not have the necessary connections to the P2P network, bootstrap nodes provide connection points with the relevant topics.

Modes
~~~~~

The DAL node has two modes:

- **Controller mode** is appropriate for most nodes; it is for nodes that post, share, or attest data.
  Nodes in this mode run in one or more profiles to determine what data they post, share, or attest, as described below.

- **Bootstrap mode** is for nodes that help other nodes connect to peers.
  Bootstrap nodes are already running on most Tezos networks, so in most cases, you don't need to run a node in bootstrap mode unless you are starting a new Tezos network.

  To run a DAL node in bootstrap mode, pass the ``--bootstrap-profile`` argument, as in this example:

  .. code-block:: shell

    octez-dal-node run --endpoint http://127.0.0.1:8732 --bootstrap-profile --data-dir $DATA_DIR

  The configuration file for a DAL node running in bootstrap mode shows only the base information about the node, as in this example:

  .. code-block:: json

    {
      "data-dir": "dal-node/",
      "endpoint": "http://127.0.0.1:8732",
      "profiles": {
        "kind": "bootstrap"
      }
    }

.. _dal_profiles:

Profiles
~~~~~~~~

As described above, different actors are interested in different roles when running DAL nodes.
Some want to produce data, others want to share data, and others want to attest data.
For this reason, DAL controller nodes run in different profiles; these profiles control which data the nodes post, share, and attest.
You can set these profiles in the node's configuration file, as CLI arguments to the node's commands, or via RPC calls.

Currently, a DAL node in controller mode can use any combination of profiles.
However, for future-proofing and for production installations, run a DAL controller node in a single profile.

A DAL node in controller mode can run in these profiles:

- The ``operator`` profile (formerly the ``producer`` profile) is for users who are running a Smart Rollup as an operator and want to post data. To run a DAL node with the ``operator`` profile, pass the ``--operator-profiles`` argument with the indexes of the slots to accept data for, as in this example:

   .. code-block:: shell

      octez-dal-node run --endpoint http://127.0.0.1:8732 --operator-profiles=0,1 --data-dir $DATA_DIR

- The ``attester`` profile is for bakers who want to attest to data. DAL nodes in this profile must pass the ``--attester-profiles`` argument with the public key hashes of the bakers to attest data for (their delegate or manager keys, not their :ref:`consensus keys<consensus_key>`), as in this example:

   .. code-block:: shell

      octez-dal-node run --endpoint http://127.0.0.1:8732 --attester-profiles=tz1QCVQinE8iVj1H2fckqx6oiM85CNJSK9Sx --data-dir $DATA_DIR

- The ``observer`` profile contributes to the resilience of network by reconstructing slots from received shards and republishing missing shards. Unlike operator nodes, observer nodes do not store data long-term and therefore cannot participate in refutation games.
  To run a DAL node with the ``observer`` profile, pass the ``--observer-profiles`` argument with the indexes of the slots to monitor or an empty string (as in ``--observer-profiles ''``) to use a random index, as in this example:

   .. code-block:: shell

      octez-dal-node run --endpoint http://127.0.0.1:8732 --observer-profiles=0,1 --data-dir $DATA_DIR

The configuration file for a DAL controller node shows its active profiles.
For example, this configuration file shows that it is running in the ``operator`` profile and accepting data for slots 0 and 1:

   .. code-block:: json

      {
        "data-dir": "dal-node/",
        "endpoint": "http://127.0.0.1:8732",
        "profiles": {
          "kind": "controller",
          "controller_profiles": {
            "operators": [ 0, 1 ],
            "observers": [],
            "attesters": []
          }
        }
      }

The configuration file for a DAL node running with the ``attester`` profile shows the public key hashes of the associated bakers, as in this example:

   .. code-block:: json

      {
        "data-dir": "dal-node/",
        "endpoint": "http://127.0.0.1:8732",
        "profiles": {
          "kind": "controller",
          "controller_profiles": {
            "operators": [],
            "observers": [],
            "attesters": [ "tz1QCVQinE8iVj1H2fckqx6oiM85CNJSK9Sx" ]
          }
        }
      }

By default, the DAL node runs in controller mode without any profile.

When a baker starts with the ``--dal-node`` argument, it checks the DAL node's configuration.
If the DAL node is not in bootstrap mode and not already set up with the ``attester`` profile, the baker configures the DAL node to use the attester profile associated with the keys that it is using.

Routing
~~~~~~~

DAL nodes must be able to initiate connections outside their local network and accept connections from outside their local network.
By default, the DAL node accepts P2P connections on port 11732, but you can change the address and port that the node listens on by setting the ``--net-addr`` argument.
In simple setups with a single DAL node, routing configuration is usually not necessary.
However, if you are using a load balancer or running multiple DAL nodes, you may need to configure port forwarding on your router and the ports and addresses that the DAL nodes use.

For more information, see :doc:`Routing <../user/routing>`.

Storage
^^^^^^^

The DAL node essentially stores slots and shards. Slots are injected into the node through an RPC (see details at :ref:`slots_lifetime`), at which moment the corresponding commitment is computed and stored. Shards and their proofs are computed and stored via another RPC. It is important to also compute the shards’ proofs, because shards can be exchanged over the P2P network only if they are accompanied by their proof. Shards received over the P2P network are also stored. The node also tracks and stores the status of commitments by monitoring the L1 chain, connecting to this end to an L1 node specified at startup.

The amount of storage space a DAL node needs depends on how long it keeps the data, and different profiles keep the data for different amounts of time:

- Bootstrap nodes store no DAL data and therefore require negligible storage.
- Attester and observer nodes store data in memory for a few blocks after the attestation delay by default.
- Operator nodes store data on disk for the slots registered with this profile for 3 months by default because the data may be needed for the Smart Rollup refutation game.

You can set how long the node stores data with the ``--history-mode`` option.

L1 monitoring
^^^^^^^^^^^^^

The DAL node tracks the L1 chain, in particular it monitors the heads of the L1 chain via an RPC. Each time a block becomes :ref:`final<finality>`, it retrieves and stores the commitments published in that block, and if its storage contains shards for these commitments, it publishes them on the P2P network. The DAL node also retrieves the slot attestation status from the block’s metadata. Finally, based on this information it updates the status of the stored commitments, where the status of a commitment can be one of the following:

- *waiting for attestation*: The commitment was included and applied in a finalized L1 block but the corresponding slot remains to be attested.
- *attested*: The commitment was included in an L1 block and the corresponding slot is attested.
- *unattested*: The commitment was included in an L1 block but the corresponding slot was not timely attested.
- *not selected*: The commitment was included in an L1 block but was not selected for that slot index. This can happen if there is another commitment in the same block for the same slot index that was placed before the commitment in question (in operation order) by the baker.
- *unseen or not finalized*: The commitment was not seen in a final L1 block. For instance, this could happen if the RPC ``PATCH /commitments/<commitment>`` was called but the corresponding commitment was never included into a block; or the commitment was included in a non-final block. This means that the publish operation was not sent (yet) to L1, or sent but not included (yet) in a block, or included in a not (yet) final block.

RPC server
^^^^^^^^^^

The DAL node incorporates an RPC server which answers to RPC queries to update or retrieve information about the node’s state.
For instance, one can post slots, ask the node to compute and store shards, to update profiles, or to connect to or disconnect from peers.

The default listening port for RPCs is 10732, but can be changed using option ``--rpc-addr``.
The RPC server is started by default, even if this option is not given.
Look at  :ref:`openapi description<dal-node-openapi>` for the list of available RPC.


Operational aspects
-------------------

.. _dal-node-commands:

DAL node commands
^^^^^^^^^^^^^^^^^

The DAL node has two commands ``config init`` and ``run``.

The command ``init config`` creates a new configuration file in the specified data directory or in the default location (ie ``~/.tezos-dal-node``) with the parameters provided on the command-line by the corresponding arguments, in case no configuration file exists already. If such a file already exists, it overrides it with the provided parameters (old parameters are lost).

The command ``run`` runs the DAL node. The CLI arguments take precedence over the configuration file arguments, except for the list of bootstrap peers and of profiles, which are considered in addition to the ones from the configuration file. The configuration file is however not overridden with the new values of the node’s parameters. However, at the end of the execution, the node’s profiles, which may have been given as arguments or set via RPCs, are written to the configuration file.

Both commands have the same arguments, which can be seen by executing, e.g., ``octez-dal-node config init --help``:

.. literalinclude:: ../api/octez-dal-node-config-init.txt
    :start-at: OPTIONS
    :end-before: COMMON OPTIONS

See the :ref:`DAL node manual <dal_node_manual>` for more details.

The concrete operational steps for participating in the DAL network are described in page :doc:`./dal_run`.

DAL configuration of the L1 node
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

All operator and observer DAL nodes should use the same initialization parameters of the cryptographic primitives used by the DAL, see section :ref:`setup_dal_crypto_params` (**NB:** just that section, not the rest of the page such as compiling sources, etc.).

Also, in order for the nodes to be able to join the P2P network, a set of bootstrap nodes can be provided using the ``network.dal_config.bootstrap_peers`` configuration parameter of the L1 node (thus using the same mechanism as for L1 nodes, see :doc:`../user/multinetwork` and :ref:`configure_p2p`).

.. _dal_node_specs:

System requirements
^^^^^^^^^^^^^^^^^^^

The system requirements for the DAL node depend on whether the node is being run by a baker for the purpose of attesting DAL data or by a slot producer for the purpose of publishing data to the DAL.
The following sections provide system requirements for these cases based on experimentation.
For more information about the experimentation, see `Hardware and bandwidth requirements for the Tezos DAL <https://forum.tezosagora.org/t/hardware-and-bandwidth-requirements-for-the-tezos-dal/6230>`_.

.. note::

    These requirements are for DAL nodes that run independently of any other Octez binary.
    The requirements are higher if they are running on the same system as other Octez binaries.

DAL attesters
~~~~~~~~~~~~~

The amount of data that a DAL node must attest to depends on how much baking power the associated baker has.
The larger the baking power, the more data the DAL sends to the DAL node to attest and the more system resources the DAL node needs.

This table shows the system requirements for a DAL node (independent of any other Octez binary) that attests 100% of the data assigned to it.
The specifications in this table are an estimate based on experimentation with Google Cloud Platform compute instances; you must monitor your DAL node to ensure that it attests all or nearly all of the data that it is assigned to attest.

+-----------------------+-----------------------+-----------------------+-----------------------+-----------------------+
| Baking power          | 0.5% of total         | 1% of total           | 2% of total           | 5% of total           |
+=======================+=======================+=======================+=======================+=======================+
| Machine type          | e2-small (ssd)        | e2-small (ssd)        | e2-small (ssd)        | e2-medium (ssd)       |
+-----------------------+-----------------------+-----------------------+-----------------------+-----------------------+
| CPU clock             | 2.25 GHz              | 2.25 GHz              | 2.25 GHz              | 2.25 GHz              |
+-----------------------+-----------------------+-----------------------+-----------------------+-----------------------+
| RAM                   | 2 GiB                 | 2 GiB                 | 2 GiB                 | 4 GiB                 |
+-----------------------+-----------------------+-----------------------+-----------------------+-----------------------+
| Disk space            | 20 GiB                | 20 GiB                | 20 GiB                | 20 GiB                |
+-----------------------+-----------------------+-----------------------+-----------------------+-----------------------+
| Bandwidth (upload)    | 250 KiB/s             | 250 KiB/s             | 250 KiB/s             | 250 KiB/s             |
+-----------------------+-----------------------+-----------------------+-----------------------+-----------------------+
| Bandwidth (download)  | 250 KiB/s             | 350 KiB/s             | 400 KiB/s             | 600 KiB/s             |
+-----------------------+-----------------------+-----------------------+-----------------------+-----------------------+

DAL producer node (operator/observer)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The system resources that a DAL producer node needs depends on how much data it needs to publish to the DAL.
This example is an estimate based on experimentation with a DAL producer node that publishes data into one slot in each block:

======================= ===============
CPU                     n2-standard-2
RAM                     4 GiB
Disk space              500 GiB
Bandwidth (upload)      2.5 MiB/s
Bandwidth (download)    0.5 MiB/s
======================= ===============

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

#. The operator posts the slot data to some DAL node of its choice. The node computes the corresponding commitment and commitment proof, as well as the corresponding shards with their proofs.
   This is done via the RPC ``POST /slots/<slot_data>``, which returns the commitment and its proof.
#. The operator selects a slot index for its slot, and posts the commitment to L1, via the ``publish_commitment`` operation.
   This can be done via RPCs for injecting an operation into L1, or using the Octez client, via the following command::

     octez-client publish dal commitment <commitment> from <pkh> for slot <slot_index> with proof <proof>

#. Once the operation is included in a final payload (that is, there is at least one block on top of the one including the operation), the slot is considered published (see :doc:`./dal_overview`), all DAL nodes exchange the slot’s shards they have in their store on the P2P network, depending on their profile (see :ref:`dal_p2p`), and they store previously unknown shards.
#. Attesters monitor the availability of their assigned shards on their DAL node, via the RPC ``GET /profiles/<pkh>/monitor/attestable_slots``, where ``pkh`` is the attester’s public key hash. (See also :doc:`dal_bakers`)
#. Attesters attach a DAL payload containing the information received at the previous step to their attestation operation, via their baker binary. (See also :doc:`dal_bakers`)
#. The protocol aggregates the received attestations, and declares each published slot as available or unavailable, depending on whether some threshold is reached, via the blocks metadata.
#. Rollups and other users can request stored pages or shards for an attested slot from any DAL node via the RPCs ``GET /slot/levels/<level>/slots/<slot_index>/pages`` or ``GET /levels/<level>/slots/<slot_index>/shards/<shard_index>/content`` respectively. Only nodes that store enough shards to reconstruct the slot can provide the requested pages.
