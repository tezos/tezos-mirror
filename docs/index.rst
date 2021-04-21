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

Latest Release
--------------

The current version of Tezos is :ref:`version-9`.

The Community
-------------

- The website of the `Tezos Foundation <https://tezos.foundation/>`_.
- `Tezos sub-reddit <https://www.reddit.com/r/tezos/>`_ is an
  important meeting point of the community.
- Several community-built block explorers are available:

    - https://tzstats.com
    - https://tezblock.io
    - https://teztracker.com/
    - https://tzkt.io (Baking focused Explorer)
    - https://arronax.io
    - https://mininax.io
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
   user/mockup
   user/proxy
   user/history_modes
   user/snapshots
   user/node-configuration
   user/multinetwork
   user/various

.. toctree::
   :maxdepth: 2
   :caption: Shell doc:

   shell/the_big_picture
   shell/validation
   shell/storage
   shell/sync
   shell/p2p
   shell/p2p_api
   shell/micheline
   shell/cli-commands
   shell/rpc

.. toctree::
   :maxdepth: 2
   :caption: 008 Edo doc:

   008/michelson
   008/proof_of_stake
   008/sapling
   008/voting
   008/glossary
   008/cli-commands
   008/rpc

.. toctree::
   :maxdepth: 2
   :caption: 009 Florence doc:

   009/michelson
   009/proof_of_stake
   009/sapling
   009/voting
   009/glossary
   009/cli-commands
   009/rpc

.. toctree::
   :maxdepth: 2
   :caption: Alpha Development Protocol doc:

   alpha/michelson
   alpha/proof_of_stake
   alpha/sapling
   alpha/voting
   alpha/glossary
   alpha/cli-commands
   alpha/rpc

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
   developer/tezt
   developer/proposal_testing
   developer/profiling
   developer/snoop
   developer/contributing
   developer/merge_team
   developer/guidelines
   README

.. toctree::
   :maxdepth: 2
   :caption: Protocols:

   protocols/naming
   protocols/003_PsddFKi3
   protocols/004_Pt24m4xi
   protocols/005_babylon
   protocols/006_carthage
   protocols/007_delphi
   protocols/008_edo
   protocols/009_florence
   protocols/alpha

.. toctree::
   :maxdepth: 2
   :caption: Releases:

   releases/april-2019
   releases/may-2019
   releases/september-2019
   releases/october-2019
   releases/december-2019
   releases/january-2020
   releases/version-7
   releases/version-8
   releases/version-9

.. toctree::
   :maxdepth: 2
   :caption: APIs:

   api/api-inline
   api/openapi
   api/errors


Indices and tables
==================

* :ref:`genindex`
* :ref:`search`
