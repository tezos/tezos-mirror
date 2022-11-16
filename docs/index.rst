.. Tezos documentation master file, created by
   sphinx-quickstart on Sat Nov 11 11:08:48 2017.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. TODO https://gitlab.com/tezos/tezos/-/issues/2170:
   search shifted protocol name/number & adapt

Welcome to the Tezos Developer Documentation!
=============================================

To start browsing, either follow one of the guided paths below, or directly pick any topics in the documentation menu.

.. image:: images/what_is_tezos_1.png
      :width: 100%
      :alt: What is Tezos

.. raw:: html

    <details>
    <summary><a>Tezos is a distributed consensus platform with meta-consensus
    capability. (See more) </a></summary>

Tezos not only comes to consensus about the state of its ledger,
like Bitcoin or Ethereum. It also attempts to come to consensus about how the
protocol and the nodes should adapt and upgrade.

`Tezos.com <https://tezos.com/>`_ contains more information on Tezos overall.

.. _octez:

Octez
~~~~~

Octez is an implementation of Tezos software, including a node, a client, a baker, an accuser, and other tools, distributed with the Tezos economic protocols of Mainnet for convenience.
This implementation is available at https://gitlab.com/tezos/tezos.
The source code is placed under the MIT Open Source License.

The current release of Octez is :doc:`../releases/version-15`.

.. _tezos_community:

The Community
~~~~~~~~~~~~~

- The website of the `Tezos Foundation <https://tezos.foundation/>`_.
- `Tezos sub-reddit <https://www.reddit.com/r/tezos/>`_ is an
  important meeting point of the community.
- Several community-built block explorers are available:

    - https://tzstats.com
    - https://tzkt.io (Baking focused explorer)
    - https://arronax.io (Analytics-oriented explorer)
    - https://mininax.io
    - https://baking-bad.org (Baking rewards tracker)
    - https://better-call.dev (Smart contracts explorer)

- A few community-run websites collect useful Tezos links:

    - https://www.tezos.help
    - https://tezoscommons.org/
    - https://tqtezos.com/

- More resources can be found in the :doc:`introduction/support` page.


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
These networks are intended for developers wanting to test their
software before going to beta and for users who want to familiarize
themselves with Tezos before using their real tez.

See the list of test networks in :ref:`test network <test-networks>`.

.. raw:: html

    </details><br/>

.. image:: images/getting_started_2.png
      :width: 100%
      :alt: Getting started

.. raw:: html

    <details>

    <summary><a>Start exploring by following the <var>Introduction</var> section in the documentation menu. (See more) </a> </summary>

These tutorials explain how to :doc:`get the latest implementation of Octez <introduction/howtoget>` in various forms, how to :doc:`start using Octez to join Tezos <introduction/howtouse>`, different :doc:`ways to participate to the network <introduction/howtorun>`, and more.

.. raw:: html

    </details><br/>

.. image:: images/using_octez_3.png
      :width: 100%
      :alt: Using Octez

.. raw:: html

    <details>

    <summary><a>Using Tezos/Octez (See more)</a></summary>

If you already installed Octez and can participate in the Tezos blockchain, the most useful resources are grouped in the ``User`` section in the documentation menu.
These pages:

- present the key concepts and mechanisms for setting up Octez, including :doc:`user/setup-client`, :doc:`user/setup-node`, and so on;
- empowers you to take advantage of Octez' basic and more advanced features, such as :doc:`user/key-management`, :doc:`user/multisig`, :doc:`user/logging`, and much more.

If you intend to participate to Tezos not just as a mere user, but also as a baker, you could also check more specialized resources such as the `NL Knowledge Center <https://docs.nomadic-labs.com/nomadic-labs-knowledge-center/>`__ or `Open Tezos <https://opentezos.com>`__.

.. raw:: html

    </details><br/>


.. image:: images/understanding_octez_4.png
      :width: 100%
      :alt: Understanding

.. raw:: html

    <details>

    <summary><a>Understanding Tezos/Octez (See more)</a></summary>


If you want to know more about the Tezos *technology*, there are several sections in the documentation presenting the main design principles of Tezos, and some high-level implementation principles of Octez:

- Section ``Shell`` introduces the common :ref:`architectural principles of any Tezos implementation <the_big_picture>`, mainly consisting of a "shell" and a "protocol". It also presents in particular the :ref:`architectural principles of the Octez implementation <packages>`. A group of pages in this section detail some major subsystems of :doc:`shell/shell`.

- Sections named ``<name> Protocol`` explain the design principles and the salient features of several Tezos protocols, current or upcoming, such as: the :doc:`the active protocol <active/protocol>`, a :doc:`protocol proposal under development <alpha/protocol>`, and possibly some protocol(s) that are currently candidate(s) for future adoption.


.. raw:: html

    </details><br/>


.. image:: images/building_on_tezos_5.png
      :width: 100%
      :alt: Building on Tezos

.. raw:: html

    <details>

    <summary><a>Building on Tezos (See more)</a></summary>

Tezos is an open platform. As any programmable blockchain, its value lies in the increasing base of smart contracts and distributed applications covering various domains, but also in the tools that make the ecosystem easier to use and more efficient, such as wallets, indexers, and many others.

For Tezos developers, this website mostly provides API documentation, but also some guidelines, including:

- A complete reference of :doc:`active/michelson`
- Important API concepts such as the :doc:`developer/rpc`
- API references such as :doc:`shell/rpc`, :doc:`api/openapi`, or :doc:`api/errors`
- Guidelines for writing smart contracts, such as :doc:`developer/michelson_anti_patterns`.

If you are looking for a more accessible and pedagogical exposition on how to write smart contracts or Dapps, there are many great resources out there for developing on Tezos, such as the `Tezos Developer Portal <https://developers.tezos.com>`__ and `Open Tezos <https://opentezos.com>`__.

.. raw:: html

    </details><br/>


.. image:: images/core_developer_6.png
      :width: 100%
      :alt: Contributing

.. raw:: html

    <details>

    <summary><a>Contributing to Octez (See more)</a></summary>

The main focus of this technical documentation website is on resources for core developers, that is, contributors to the Tezos platform, and in particular to its Octez implementation.

Core developers can find a rich set of explanations, tutorials, and howtos, mainly in the ``Core Developer`` section, including:

- a tutorial on the various forms of contributing (:doc:`developer/contributing`), and guidelines such as :doc:`developer/guidelines`
- programming tutorials covering various libraries and frameworks specific to the Octez OCaml implementation, such as using :doc:`developer/gadt`, using :doc:`developer/error_monad`, using :doc:`developer/clic`, :doc:`developer/event_logging_framework`, etc.
- howtos for specific maintenance tasks such as :doc:`developer/michelson_instructions`, :doc:`developer/protocol_environment_upgrade`, or :doc:`developer/howto-freeze-protocols`
- a whole subsection on the :doc:`various testing frameworks <developer/testing_index>` for Octez, explaining how to use them and how to add different kinds of tests
- presentations of various tools for core developers, such as support for :doc:`developer/profiling` and :doc:`developer/snoop`.

Core developers are also provided reference materials for internal APIs of Octez, such as:

- The :doc:`API of OCaml libraries and modules <api/api-inline>` reference
- The :doc:`shell/p2p_api` reference
- The :doc:`developer/merkle-proof-encoding-formats` reference.

.. raw:: html

    </details><br/>


.. toctree::
   :maxdepth: 2
   :caption: Introduction:
   :hidden:

   introduction/howtoget
   introduction/howtouse
   introduction/howtorun
   introduction/test_networks
   introduction/get_troubleshooting
   introduction/support

.. toctree::
   :maxdepth: 2
   :caption: User doc:
   :hidden:

   user/versioning
   user/setup-client
   user/setup-node
   user/proxy-server
   user/multisig
   user/fa12
   user/logging
   user/various

.. toctree::
   :maxdepth: 2
   :caption: Shell doc:
   :hidden:

   shell/the_big_picture
   shell/shell
   shell/p2p_api
   shell/cli-commands
   shell/rpc

.. toctree::
   :maxdepth: 2
   :caption: Lima Protocol doc:
   :hidden:

   active/protocol
   active/glossary
   active/cli-commands
   active/rpc

.. toctree::
   :maxdepth: 2
   :caption: Mumbai Protocol doc:
   :hidden:

   mumbai/protocol
   mumbai/glossary
   mumbai/cli-commands
   mumbai/rpc

.. toctree::
   :maxdepth: 2
   :caption: Alpha Development Protocol doc:
   :hidden:

   alpha/protocol
   alpha/glossary
   alpha/cli-commands
   alpha/rpc

.. toctree::
   :maxdepth: 2
   :caption: Developer doc:
   :hidden:

   developer/rpc
   developer/michelson_anti_patterns

.. toctree::
   :maxdepth: 2
   :caption: Core Developer doc:
   :hidden:

   developer/contributing_index
   developer/programming
   developer/testing_index
   developer/maintaining
   README
   developer/tools
   developer/encodings
   developer/merkle-proof-encoding-formats
   developer/openmetrics

.. toctree::
   :maxdepth: 2
   :caption: Protocols:
   :hidden:

   protocols/naming
   protocols/014_kathmandu
   protocols/015_lima
   protocols/016_mumbai
   protocols/alpha
   protocols/history

.. toctree::
   :maxdepth: 2
   :caption: Releases:
   :hidden:

   releases/releases
   releases/version-15
   releases/history

.. toctree::
   :maxdepth: 2
   :caption: APIs:
   :hidden:

   api/api-inline
   api/openapi
   api/errors
