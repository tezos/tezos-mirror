.. Tezos documentation master file, created by
   sphinx-quickstart on Sat Nov 11 11:08:48 2017.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. TODO https://gitlab.com/tezos/tezos/-/issues/2170:
   search shifted protocol name/number & adapt

Welcome to the Tezos Developer Documentation!
=============================================

The Project
-----------

Tezos is a distributed consensus platform with meta-consensus
capability. Tezos not only comes to consensus about the state of its ledger,
like Bitcoin or Ethereum. It also attempts to come to consensus about how the
protocol and the nodes should adapt and upgrade.
The website https://tezos.com/ contains more information about the project.

.. _octez:

Octez
-----

Octez is an implementation of Tezos software, including a node, a client, a baker, an accuser, and other tools, distributed with the Tezos economic protocols of Mainnet for convenience.
This implementation is available at https://gitlab.com/tezos/tezos.
The source code is placed under the MIT Open Source License.

The current release of Octez is :doc:`../releases/version-12`.

This website
------------

This website (https://tezos.gitlab.io/) provides online developer documentation.
This documentation is about Octez, although it also documents Tezos in general.

The developer documentation is automatically generated from the master branch
of the above repository.

.. _tezos_community:

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

- More resources can be found in the :doc:`introduction/support` page.


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
   introduction/get_troubleshooting
   introduction/support

.. toctree::
   :maxdepth: 2
   :caption: User documentation:

   user/key-management
   user/node-configuration
   user/versioning
   user/snapshots
   user/history_modes
   user/multinetwork
   user/sandbox
   user/mockup
   user/proxy
   user/light
   user/proxy-server
   user/multisig
   user/fa12
   user/various
   user/logging

.. toctree::
   :maxdepth: 2
   :caption: Shell doc:

   shell/the_big_picture
   shell/validation
   shell/prevalidation
   shell/storage
   shell/sync
   shell/p2p
   shell/p2p_api
   shell/micheline
   shell/cli-commands
   shell/rpc

.. toctree::
   :maxdepth: 2
   :caption: Ithaca Protocol doc:

   active/protocol
   active/glossary
   active/cli-commands
   active/rpc

.. toctree::
   :maxdepth: 2
   :caption: Jakarta Protocol doc:

   jakarta/protocol
   jakarta/glossary
   jakarta/cli-commands
   jakarta/rpc

.. toctree::
   :maxdepth: 2
   :caption: Alpha Development Protocol doc:

   alpha/protocol
   alpha/glossary
   alpha/cli-commands
   alpha/rpc

.. toctree::
   :maxdepth: 2
   :caption: Developer Tutorials:

   developer/rpc
   developer/encodings
   developer/data_encoding
   developer/gadt
   developer/error_monad
   developer/clic
   developer/michelson_anti_patterns
   developer/michelson_instructions
   developer/entering_alpha
   developer/protocol_release_checklist
   developer/howto-freeze-protocols
   developer/protocol_environment
   developer/protocol_environment_upgrade
   developer/event_logging_framework
   developer/testing_index
   developer/profiling
   developer/snoop
   developer/contributing
   developer/merge_team
   developer/guidelines
   developer/repository_scope
   developer/time_measurement_ppx
   developer/openmetrics
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
   protocols/010_granada
   protocols/011_hangzhou
   protocols/012_ithaca
   protocols/013_jakarta
   protocols/alpha

.. toctree::
   :maxdepth: 2
   :caption: Releases:

   releases/releases
   releases/april-2019
   releases/may-2019
   releases/september-2019
   releases/october-2019
   releases/december-2019
   releases/january-2020
   releases/version-7
   releases/version-8
   releases/version-9
   releases/version-10
   releases/version-11
   releases/version-12

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
