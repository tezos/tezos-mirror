.. _octez:

Octez & Protocol overview
-------------------------

Octez, bundled with the Tezos protocol, are a complete implementation of the `Tezos blockchain <https://tezos.com>`__.

Tezos is a distributed consensus platform with meta-consensus
capability. This means that Tezos not only comes to consensus about the state of its ledger,
like Bitcoin or Ethereum. It also attempts to come to consensus about how the
protocol and the nodes should adapt and upgrade.


Octez includes a node, a client, a baker, an accuser, and other tools, distributed with the Tezos economic protocols of Mainnet for convenience.
The source code is placed under the MIT Open Source License, and
is available at https://gitlab.com/tezos/tezos.

This website contains technical documentation about both Octez and the Tezos protocol.

The current release of Octez is :doc:`../releases/version-18`.

A release candidate for the next version :doc:`../releases/version-19` is also available.

For installing instructions, see :doc:`./howtoget`.

Tezos is backed up by a vibrant :ref:`Tezos community <tezos_community>`.
For technical support, check the :doc:`Tezos technical support page <../global/support>`.

Mainnet & Test Networks
~~~~~~~~~~~~~~~~~~~~~~~

The Tezos network, called ``mainnet``, is the current incarnation of the Tezos blockchain.
It runs with real tez that have been allocated to the
donors of July 2017 fundraiser (see :ref:`activate_fundraiser_account`).
It has been live and open since June 30th 2018.

All the instructions in this documentation are valid for Mainnet
however we **strongly** encourage users to first try all the
introduction tutorials on some test network to familiarize themselves without
risks.

There are several :ref:`test networks <test-networks>` for the Tezos blockchain with a
faucet to obtain free tez (see :ref:`faucet`).
These networks are intended for developers wanting to test their
software before going to beta and for users who want to familiarize
themselves with Tezos before using their real tez.

Documentation source
~~~~~~~~~~~~~~~~~~~~

The source of this technical documentation website is an integral part of the :ref:`Octez <octez>` repository, following `Docs as Code <https://www.writethedocs.org/guide/docs-as-code/>`_ best practices. The documentation is automatically built from the current version of the master branch.
