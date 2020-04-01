.. Tezos documentation master file, created by
   sphinx-quickstart on Sat Nov 11 11:08:48 2017.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.


Welcome to the Tezos Developer Documentation!
=============================================

The Project
-----------

Tezos is a distributed consensus platform with meta-consensus
capability. Tezos not only comes to consensus about the state of its ledger,
like Bitcoin or Ethereum. It also attempts to come to consensus about how the
protocol and the nodes should adapt and upgrade.

 - Developer documentation is available online at https://tezos.gitlab.io/
   and is automatically generated from the master branch.
 - The website https://tezos.com/ contains more information about the project.
 - All development happens on GitLab at https://gitlab.com/tezos/tezos

The source code of Tezos is placed under the MIT Open Source License.


The Community
-------------

- The website of the `Tezos Foundation <https://tezos.foundation/>`_.
- `Tezos sub-reddit <https://www.reddit.com/r/tezos/>`_ is an
  important meeting point of the community.
- Several community-built block explorers are available:

    - https://tzstats.com
    - https://tezos.id
    - https://tezblock.io
    - https://teztracker.everstake.one
    - https://tzkt.io (Baking focused Explorer)
    - https://arronax.io
    - https://mininax.cryptonomic.tech/mainnet
    - https://baking-bad.org (Reward Tracker)
    - https://better-call.dev (Smart-contract Explorer)

- A few community-run websites collect useful Tezos links:

    - https://www.tezos.help
    - https://tezos.rocks

- More resources can be found in the :ref:`support` page.


The Networks
------------

Mainnet
~~~~~~~

The Tezos network is the current incarnation of the Tezos blockchain.
It runs with real tez that have been allocated to the
donors of July 2017 ICO (see :ref:`activate_fundraiser_account`).

The Tezos network has been live and open since June 30th 2018.

All the instructions in this documentation are valid for Mainnet
however we **strongly** encourage users to first try all the
introduction tutorials on some :ref:`test network <test-networks>` to familiarize themselves without
risks.

Test Networks
~~~~~~~~~~~~~

There are several test networks for the Tezos blockchain with a
faucet to obtain free tez (see :ref:`faucet`).
It is the reference network for developers wanting to test their
software before going to beta and for users who want to familiarize
themselves with Tezos before using their real tez.

See the list of test networks in :ref:`test network <test-networks>`.

Getting started
---------------

The best place to start exploring the project is following the How Tos
in the :ref:`introduction <howtoget>`.


.. toctree::
   :maxdepth: 2
   :caption: Introduction tutorials:

   introduction/howtoget
   introduction/howtouse
   introduction/howtorun
   introduction/test_networks
   introduction/support

.. toctree::
   :maxdepth: 2
   :caption: User documentation:

   user/key-management
   user/sandbox
   user/history_modes
   user/snapshots
   user/various
   user/glossary
   user/multinetwork

.. toctree::
   :maxdepth: 2
   :caption: White doc:

   whitedoc/the_big_picture
   whitedoc/p2p
   whitedoc/validation
   whitedoc/michelson
   whitedoc/proof_of_stake
   whitedoc/voting

.. toctree::
   :maxdepth: 2
   :caption: Developer Tutorials:

   developer/rpc
   developer/data_encoding
   developer/error_monad
   developer/michelson_anti_patterns
   developer/entering_alpha
   developer/protocol_environment
   developer/testing
   developer/flextesa
   developer/python_testing_framework
   developer/proposal_testing
   developer/profiling
   developer/contributing
   developer/merge_team

.. toctree::
   :maxdepth: 2
   :caption: Protocols:

   protocols/003_PsddFKi3
   protocols/004_Pt24m4xi
   protocols/005_babylon
   protocols/006_carthage

.. toctree::
   :maxdepth: 2
   :caption: Releases:

   releases/april-2019
   releases/may-2019
   releases/september-2019
   releases/october-2019
   releases/december-2019
   releases/january-2020

.. toctree::
   :maxdepth: 2
   :caption: APIs:

   README
   api/api-inline
   api/cli-commands
   api/rpc
   api/errors
   api/p2p


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
