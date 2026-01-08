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

.. _protocol_changelogs:
.. _naming_convention:

Protocol changes
----------------

The protocols that have been used in the past are versioned by an increasing
sequence of three-digit numbers, starting with 000. From protocol versioned 004
upwards, each protocol version is traditionally named after an ancient city
(using anglicised name), where the first letters form an alphabetical order
sequence.

Here is the list of protocol names and links to the corresponding changelogs:

* 000 Genesis
* 001 Alpha-I
* 002 Alpha-II
* :doc:`003 Alpha-III <003_PsddFKi3>`
* :doc:`004 Athens <004_Pt24m4xi>`
* :doc:`005 Babylon <005_babylon>`
* :doc:`006 Carthage <006_carthage>`
* :doc:`007 Delphi <007_delphi>`
* :doc:`008 Edo <008_edo>`
* :doc:`009 Florence <009_florence>`
* :doc:`010 Granada <010_granada>`
* :doc:`011 Hangzhou <011_hangzhou>`
* :doc:`012 Ithaca <012_ithaca>`
* :doc:`013 Jakarta <013_jakarta>`
* :doc:`014 Kathmandu <014_kathmandu>`
* :doc:`015 Lima <015_lima>`
* :doc:`016 Mumbai <016_mumbai>`
* :doc:`017 Nairobi <017_nairobi>`
* :doc:`018 Oxford <018_oxford>`
* :doc:`020 ParisC <020_paris>`
* :doc:`021 Quebec <021_quebec>`
* :doc:`022 Rio <022_rio>`
* :doc:`023 Seoul <023_seoul>`
* ...

Due to the evolving nature of the in-use protocols, the above absolute protocol
names are not enough. More naming conventions are introduced for the currently
in-use and upcoming protocols:

* The protocol in the ``src/proto_alpha`` directory of the ``master`` branch:

  - :doc:`protocol Alpha <alpha>`
  - other terms: "protocol under development", "development protocol" (only when
    there is a single one)

* The currently active protocol:

  - "current protocol"
  - other terms: "active protocol", "mainnet protocol"

* Any protocol currently subject to the governance process, that is, being part of any of the possible voting
  phases:

  - "candidate protocol"
  - other possible terms: "(new) protocol proposal", "current proposal"

* A protocol proposal that has successfully passed all the votes in the :doc:`voting process <../active/voting>` and is waiting for activation during the Adoption period:

  - "voted protocol (proposal)"

External resources
------------------

The current status of a protocol in the governance process can be found at election pages such as: tzstats.com_, tzkt.io_, or tezosagora.org_.

An interesting blog post on the history of the initial Tezos economic protocols
is: `A quick history of past upgrades
<https://research-development.nomadic-labs.com/amendments-at-work-in-tezos.html#a-quick-history-of-past-upgrades>`_.

.. _tzstats.com: https://tzstats.com/election/head
.. _tzkt.io: https://tzkt.io/governance/current/exploration
.. _tezosagora.org: https://www.tezosagora.org/period
