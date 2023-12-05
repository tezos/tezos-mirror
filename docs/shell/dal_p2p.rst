The DAL's P2P protocol
======================

The peer-to-peer (P2P) protocol for the DAL is made out of two components:

- A gossipsub algorithm, instantiated as detailed below.
- A transport layer for handling connections with peers.
  We reuse for that the P2P protocol used by the Octez node (see :doc:`./p2p`).

The gossip algorithm used for the DAL is an in-house version of the gossipsub v1.1 P2P protocol defined by the lib-p2p project. A detailed overview of this protocol is available `here <https://docs.libp2p.io/concepts/pubsub/overview/>`__ and an informal English specification can be found `here <https://github.com/libp2p/specs/blob/master/pubsub/gossipsub/gossipsub-v1.1.md>`__. This gossip algorithm allows to partition the network into virtual subnetworks, each identified by a **topic**. The topic also determines the valid data that can be exchanged over the corresponding virtual subnetwork, as any exchanged message has exactly one associated topic. Each peer subscribes to topics of interest to him. This protocol enhances the network's scalability compared to traditional gossip algorithms.

For each message, there is a **message id** that uniquely identifies this message. When a message is pushed, it comes with its message id. When the message is pulled, it is done via the message id.
For every topic a node subscribes to, it maintains a virtual subnetwork, or **mesh**, of peers also subscribed to that topic. When a node has a new message to share (originating from the application layer) or needs to relay a received message, it does so to all peers in the corresponding topic's mesh. Moreover, the node broadcasts the ids of the last received messages to a random selection of peers outside the mesh. Peers receiving these teasers can request the full message if they are interested in it.
For the DAL instantiation of gossipsub, a message is defined as a 3-tuple: a shard, the shard’s index, and the shard’s proof proving that the shard corresponds to the commitment given by the message id. The associated message id consists of the shard index and the associated slot index, (published) level, slot commitment, and attestor’s public key hash.

A topic is defined as a pair ``(slot_index, public_key_hash)``. The first component identifies the slot associated to any shard published under this topic, while the second component identifies the baker assigned to this shard.
Such a set of topics ensures that the bandwidth of bakers and slot producers is bounded (for valid messages) over a cycle.

A slot producer should subscribe to all relevant topics associated with their slot index. This includes every topic where a baker is assigned at least one shard for that slot index.
On the other hand, a baker should subscribe to all topics that feature their public key address.

Gossipsub also defines a notion of score which is used to only connect to peers with a good score.

Regarding peer discovery, the current implementation of the DAL relies on gossipsub v1.1 peer exchanges. In particular, DAL nodes can be configured in bootstrap mode to facilitate peer discovery.

.. note::

	The current topic structure in the DAL for Tezos may be revised in a future update. Presently, topics include the bakers’ address (public key hash), which leads to a potentially unbounded number of topics over time. Another approach under consideration involves using a ``(slot_index, shard_index)`` pair, offering a more scalable solution in the long run, when the number of attesters surpasses the number of slots.

.. warning::

	Attention must be paid to the security implications for bakers in the DAL network. Since a baker's bandwidth is proportional to their stake, it can become relatively straightforward to identify the IP address of their DAL node, particularly for those with substantial stakes. To mitigate this risk, bakers are advised to operate their DAL node using an IP address different from their L1 node. This separation helps in preventing the unintentional exposure of the L1 node's IP address.

	Plans are underway to address these concerns. One proposed solution is to enable bakers to divide their bandwidth across multiple DAL nodes, enhancing both security and operational flexibility.
