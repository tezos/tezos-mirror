The Tezos blockchain
--------------------

Tezos is a distributed consensus platform with meta-consensus
capability.

Tezos not only comes to consensus about the state of its ledger,
like Bitcoin or Ethereum. It also attempts to come to consensus about how the
protocol and the nodes should adapt and upgrade.

`Tezos.com <https://tezos.com/>`_ contains more information on Tezos overall.

.. _octez:

Octez
~~~~~

Octez is an implementation of Tezos software, including a node, a client, a baker, an accuser, and other tools, distributed with the Tezos economic protocols of Mainnet for convenience.
The source code is placed under the MIT Open Source License, and
is available at https://gitlab.com/tezos/tezos.

The current release of Octez is :doc:`../releases/version-18`.

A release candidate for the next version :doc:`../releases/version-19` is also available.

For installing instructions, see :doc:`./howtoget`.

.. _tezos_community:

The Community
~~~~~~~~~~~~~

- The website of the `Tezos Foundation <https://tezos.foundation/>`_.
- `Tezos Agora <https://www.tezosagora.org>`_ is the premier meeting point for the community.
- Several community-built block explorers are available:

    - https://tzstats.com
    - https://tzkt.io (Baking focused explorer)
    - https://arronax.io (Analytics-oriented explorer)
    - https://mininax.io
    - https://baking-bad.org (Baking rewards tracker)
    - https://better-call.dev (Smart contracts explorer)

- A few community-run websites collect useful Tezos links:

    - https://tezos.com/ecosystem (resources classified by their kind: organisations, block explorers, wallets, etc.)
    - https://tezoscommons.org/ (featured resources classified by approach: technology, developing, contributing, etc.)
    - https://tezos.com/developers/ (resources for developers of applications built on Tezos)

- More resources can be found in the :doc:`support` page.


Mainnet
~~~~~~~

The Tezos network is the current incarnation of the Tezos blockchain.
It runs with real tez that have been allocated to the
donors of July 2017 fundraiser (see :ref:`activate_fundraiser_account`).

The Tezos network has been live and open since June 30th 2018.

All the instructions in this documentation are valid for Mainnet
however we **strongly** encourage users to first try all the
introduction tutorials on some :ref:`test network <test-networks>` to familiarize themselves without
risks.

Test Networks
~~~~~~~~~~~~~

There are several :ref:`test networks <test-networks>` for the Tezos blockchain with a
faucet to obtain free tez (see :ref:`faucet`).
These networks are intended for developers wanting to test their
software before going to beta and for users who want to familiarize
themselves with Tezos before using their real tez.

This website
~~~~~~~~~~~~

This website (https://tezos.gitlab.io/) provides online technical documentation. This documentation is about :ref:`octez`, although it also documents Tezos in general.

The technical documentation is an integral part of the :ref:`Octez <octez>` repository, and is automatically generated from the master branch, following `Docs as Code <https://www.writethedocs.org/guide/docs-as-code/>`_ best practices.
