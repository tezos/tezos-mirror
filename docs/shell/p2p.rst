The peer-to-peer layer
======================

This document explains the concepts and internal architecture of the peer-to-peer (or P2P) layer of
the Tezos shell. This part is in charge of establishing and
maintaining network connections with other nodes (gossip).

The exposition is kept high-level so as to be useful for Tezos users needing to know the global working of this system and the meaning of some parameters that can be configured.

The P2P layer is instantiated by the node. It is parameterized by the
type of messages that are exchanged over the network (to allow
different P2P protocol versions/extensions), and the type of metadata
associated with each peer. The latter is useful to compute a score for
each peer that reflects the level of trust we have in it. Different
policies can be used when communicating with peers with different
score values.

The P2P layer is comprised of a pool of connections, a set of
operations on those connections, and a set of workers following the
worker pattern pervasively used in the codebase.

The P2P layer is packaged in module :package-api:`Tezos-p2p <octez-shell-libs/Tezos_p2p/index.html>`, which has
documentation for all the constituent modules.

General operation
-----------------

The main entry point of the P2P layer is the module :package-api:`P2p
<octez-shell-libs/Tezos_p2p/P2p/index.html>`.

The type :package-api:`P2p_conn.t <octez-shell-libs/Tezos_p2p/P2p_conn/index.html>` defined by the P2P layer represents a connection to a peer with some associated metadata.
It wraps a
:package-api:`P2p_socket.t
<octez-shell-libs/Tezos_p2p/P2p_socket/index.html#type-t>`
type that is basically a UNIX socket upgraded with I/O
scheduling, peer metadata, cryptographic keys, and two message queues
operated by dedicated workers which operate on those queues.

I/O Scheduling
~~~~~~~~~~~~~~

The P2P layer uses a scheduling mechanism to control its
bandwidth usage as well as implementing different policies
(e.g. read/write quotas) to different peers. For now, each peer is
granted a fair share of the global allocated bandwidth, but it is
planned for the individual allocated bandwidth to each peer to be a
function of the peer's score.

Message queues
~~~~~~~~~~~~~~

On top of basic I/O scheduling, two finite-size typed message queues
are used to store incoming (resp. outgoing) messages for each
peer, as implemented in :package-api:`P2p_socket.t
<octez-shell-libs/Tezos_p2p/P2p_socket/index.html#type-t>`.
This further restricts the speed at which communication is
possible with a peer; when a queue is full, it is not possible to read
(resp. write) an additional message.

Encryption
~~~~~~~~~~

The connection between each peer is encrypted using ``NaCl``
authenticated-encryption `API <http://nacl.cr.yp.to/box.html>`__. This
is done to provide an additional level of security and tamper-proof
guarantees in the communication between peers.

Pool of connections
~~~~~~~~~~~~~~~~~~~

All the above modules are used in :package-api:`P2p_pool
<octez-shell-libs/Tezos_p2p/P2p_pool/index.html>`, which
constitutes the core of the P2P layer, together with the worker
processes described below. It comprises various tables of connections
as well as methods to query them, also connections are extended with
another message queue where lower-level messages (like responses to
ping) are filtered out and only application-level messages are kept.

See below
for a description of workers acting onto the P2P layer.

Welcome worker & connect handler
--------------------------------

The welcome worker & the connect handler are responsible for accepting incoming
connections and register them into the pool of connections managed by the P2P
layer. They basically run the ``accept(2)`` syscall and build the stack of types
that compose a connection. They decide how this new connection must be
handled.

Peers and addresses
~~~~~~~~~~~~~~~~~~~

A peer (node) has 2 different ways to be identified,
both are useful to identify an active connection or to contact a peer.

First, a peer is identified by a cryptographic identifier.
This **peer identifier** is often named ``peer_id`` but sometimes it is simply named peer by abuse of notation.

Secondly, a peer can be identified by its **point**, which is an IP address plus a port (several peers can run on a same IP address, on different ports).
Points are sometimes called (peer) addresses, by abuse of notation.

A peer may change of point and on the other hand, at two different times, a point may lead to different peers.

{Black, White, Grey}lists
~~~~~~~~~~~~~~~~~~~~~~~~~

The welcome worker takes care of filtering all incoming connections using two
static lists of addresses configurable using ``octez-admin-client`` and a system
table that is handled automatically by the P2P layer. The node administrator can
block (blacklist) or enable (whitelist) individual IP addresses, while the P2P layer is in charge of
temporarily banning (greylisting) IP addresses and peers who misbehave.

IP addresses are automatically removed from the greylist table after some timeout, which is defined by the configuration variable
``greylist_timeout``.
On the other hand, peers that are greylisted are maintained in a limited-size list, managed according to a LRU policy (dropping least-recently used items).

The node administrator can also:

* flush greylist tables using ``octez-admin-client``;
* unban peers and IP addresses that were manually banned.

Indeed, as a peer has 2 different ways to be identified, it is important to be able to ban/unban both peers and IP addresses.

Beware that banning/unbanning a peer does not ensure that its IP address is also banned/unbanned. On the
other hand, banning an IP address will ban all
currently connected peers from that IP address (and disable incoming connections).
Therefore, if you want to unban a peer you should unban its IP address and/or the peer itself, depending on how it was banned.

Maintenance worker
------------------

The maintenance worker is in charge of establishing an appropriate
number of connections with other nodes to guarantee a
realistic view of the state of the blockchain. It is created with a
set of targets to reach regarding the desired amount of peers it needs
to keep an active connection to.

At the pool level, the minimum (resp. maximum) acceptable number of
connections is defined.

At the maintenance worker level, two other sets of thresholds are
defined: ``target`` (min and max) and ``threshold`` (min and max).

Given these bounds, the maintenance worker:

* Will be triggered periodically (e.g., every two minutes), when asked by the shell, and
  when the minimum or maximum number of acceptable connections is
  reached.

* Will perform the following actions when triggered: if the number of
  connections is above ``max_threshold``, it will kill connections
  randomly until it reaches ``max_target`` connections. If the number of
  connections is below ``min_threshold``, it will attempt to connect to
  peers until it reaches at least ``min_target`` connections (and never
  more than ``max_target`` connections).

The maintenance worker is also in charge of periodically running the
greylists GC functions to unban IP addresses from the greylist.
