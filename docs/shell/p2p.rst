The peer-to-peer layer
======================

This document explains the inner workings of the peer-to-peer layer of
the Tezos shell. This part is in charge of establishing and
maintaining network connections with other nodes (gossip).

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

The P2P layer is packaged in :package:`tezos-p2p`, which has
documentation for all modules.

General operation
-----------------

I/O Scheduling
~~~~~~~~~~~~~~

The P2P layer uses a scheduling mechanism to control its
bandwidth usage as well as implementing different policies
(e.g. read/write quotas) to different peers. For now, each peer is
granted a fair share of the global allocated bandwidth, but it is
planned for the individual allocated bandwidth to each peer to be a
function of the peer's score.

Encryption
~~~~~~~~~~

The connection between each peer is encrypted using ``NaCl``
authenticated-encryption `API <http://nacl.cr.yp.to/box.html>`__. This
is done to provide an additional level of security and tamper-proof
guarantees in the communication between peers.

Message queues
~~~~~~~~~~~~~~

On top of basic I/O scheduling, two finite-size typed message queues
are used to store incoming (resp. outgoing) messages for each
peer. This further restricts the speed at which communication is
possible with a peer; when a queue is full, it is not possible to read
(resp. write) an additional message. The high-level
`P2p_socket.t
<../api/odoc/_html/tezos-p2p/Tezos_p2p/P2p_socket/index.html#type-t>`__
type defined by the P2P layer is basically a UNIX socket upgraded with I/O
scheduling, peer metadata, cryptographic keys, and two message queues
operated by dedicated workers which operate on those queues.

Pool of connections
~~~~~~~~~~~~~~~~~~~

All the above modules are used in `P2p_pool
<../api/api-inline.html#tezos-p2p/Tezos_p2p/P2p_pool/index.html>`__, which
constitutes the core of the P2P layer, together with the worker
processes described below. It comprises various tables of connections
as well as methods to query them, also connections are extended with
another message queue where lower-level messages (like responses to
ping) are filtered out and only application-level messages are kept.

The main entry point of the P2P layer is in module `P2p
<../api/api-inline.html#tezos-p2p/Tezos_p2p/P2p/index.html>`__. See below
for a description of workers acting onto the P2P layer.

Welcome worker & connect handler
--------------------------------

The welcome worker & the connect handler are responsible for accepting incoming
connections and register them into the pool of connections managed by the P2P
layer. It basically runs the ``accept(2)`` syscall and build the stack of types
that composed a connection. They decide how this new connection must be
handled.

{Black, White, Grey}lists
~~~~~~~~~~~~~~~~~~~~~~~~~

The welcome worker takes care of filtering all incoming connections using two
static lists of addresses handled by ``octez-admin-client`` and a system
table that is handled automatically by the P2P layer. The node administrator can
block (blacklist) or enable (whitelist) individual IP addresses, while the P2P layer is in charge of
temporarily banning (greylist) IP addresses and peers who misbehave. The delay to remove an
IP address from the greylist table is defined by the configuration variable
``greylist_timeout``, while peers that are greylisted are periodically removed.

The node administrator can also flush greylist tables with the
``octez-admin-client``.

The node administrator can also choose to unban peers and addresses that
were manually banned or accidentally greylisted.

It is possible to ban/unban trust/untrust both peers and addresses. For
example, unbanning a peer does not ensure that its ip is also unbanned, on the
other hand, banning/unbanning an address has also the same effect on all
connected peers associated to that address.

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

* Will be triggered every two minutes, when asked by the shell, and
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
