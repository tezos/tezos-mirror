Protocol versioning
===================

The protocol is the part of the node software that provides logic for executing
transactions and building blockchain blocks. It embodies all the rules that a
Tezos network operates under. Therefore changes to the protocol must be
explicitly accepted by the community before nodes actually employ them. For this
reason the protocol is versioned independently from the rest of the software
engaged in running Tezos. Each protocol version is being proposed to the
community for acceptance. The community then decides whether to accept the new
protocol or keep the old one. This is done through a :doc:`voting procedure <../active/voting>`, which
takes place within the blockchain itself, so its rules are also a part of the
protocol.

.. _naming_convention:

Protocol naming
---------------

The protocols that have been used in the past are versioned by an increasing
sequence of three-digit numbers, starting with 000. From protocol versioned 004
upwards, each protocol version is traditionally named after an ancient city
(using anglicised name), where the first letters form an alphabetical order
sequence:

* 000 Genesis
* 001 Alpha-I
* 002 Alpha-II
* 003 Alpha-III
* 004 Athens
* 005 Babylon
* 006 Carthage
* 007 Delphi
* 008 Edo
* 009 Florence
* 010 Granada
* 011 Hangzhou
* 012 Ithaca
* 013 Jakarta
* 014 Kathmandu
* 015 Lima
* 016 Mumbai
* 017 Nairobi
* ...

Due to the evolving nature of the in-use protocols, the above absolute protocol
names are not enough. More naming conventions are introduced for the currently
in-use and upcoming protocols:

* The protocol in the ``src/proto_alpha`` directory of the ``master`` branch:
  "alpha protocol"

  - other terms: "protocol under development", "development protocol" (only when
    there is a single one)

* The currently active protocol: "current protocol".

  - other terms: "active protocol", "mainnet protocol"

* The protocol currently under voting, that is in any of the possible voting
  phases: "candidate protocol".

  - other possible terms: "current proposal"

External resources
------------------

The current protocol status can be found at election pages such as: tzstats.com_, tzkt.io_, or tezosagora.org_.

An interesting blog post on the history of the initial Tezos economic protocols
is: `A quick history of past upgrades
<https://research-development.nomadic-labs.com/amendments-at-work-in-tezos.html#a-quick-history-of-past-upgrades>`_.

.. _tzstats.com: https://tzstats.com/election/head
.. _tzkt.io: https://tzkt.io/governance/current/exploration
.. _tezosagora.org: https://www.tezosagora.org/period
