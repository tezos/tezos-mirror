Tezos Software Architecture
===========================

A Tezos network is composed of several running Tezos nodes, complemented by other running daemons, such as bakers and signers.
Any implementation of the Tezos node should adhere to a high-level software architecture, described in this page.

For instance, the Octez suite, which provides an implementation of the Tezos node and other executables, implements these principles in the `Octez software architecture <https://tezos.gitlab.io/shell/the_big_picture.html>`__.

Shell & Protocol
----------------

The characteristic that makes Tezos unique is its self-amending
property. The part of the Tezos node that amends itself is called the *economic protocol* (or shortly, the protocol).
The rest of a Tezos node is called the *shell*.

The protocol is responsible for interpreting the transactions and other
administrative operations. It also has the responsibility to distinguish between valid and erroneous blocks.

The protocol is subject to an amendment procedure in which on-chain operations can be 
used to switch from one protocol to another. The procedure is described in more detail
in :doc:`the protocol's voting procedure documentation <../active/voting>`.

A Tezos node has mainly three roles: it validates blocks and operations, it
broadcasts them to (and retrieves them from) other nodes, and it maintains a
main chain and its associated state (i.e. the ledger), which includes accounts
and their balances, among other things. Note that, as blocks only specify a
predecessor block, exchanged blocks do not necessarily form a chain, but rather
a tree. Nodes communicate over :doc:`a gossip network<../shell/p2p>`.

To implement these roles, the shell mainly includes:

- the peer-to-peer layer, allowing the nodes to exchange the chain data,
- the storage component, allowing to store blocks (the deltas composing the history of the blockchain), operations within blocks, and the versioned state of the ledger.

Tezos' client-server architecture
---------------------------------

A Tezos node also acts as a server, which responds to queries and requests from
clients. Such queries and requests are implemented via :doc:`RPC
calls<../developer/rpc>`. A client can query the chain’s state and can inject
blocks and operations into a node. One particular client is the :ref:`baker daemon <baker_run>`,
which is associated to an account. In particular the baker has access to the
account’s private key and thus can sign blocks and operations.

The main reason for using such a client-server architecture is safety: to insulate
the component that has access to the client keys, i.e. the baker, from the
component which is exposed to the internet, i.e. the node. Indeed, the node and
the baker can sit on different computers and the baker does not need to be
exposed to the internet. So nodes manage communication and shield bakers from
network attacks, and bakers hold secrets and bake blocks into the blockchain.

Another advantage of this architecture is that bakers can more easily have
different implementations, and this is important, for instance because different bakers may want
to implement different transaction selection strategies.

Finally, this client-server architecture allows other tools such as the client, daemons, and third-party applications such as wallets or indexers, to interact with the node and introspect its state.
