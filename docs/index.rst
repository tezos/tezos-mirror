.. Tezos documentation master file, created by
   sphinx-quickstart on Sat Nov 11 11:08:48 2017.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. TODO https://gitlab.com/tezos/tezos/-/issues/2170:
   search shifted protocol name/number & adapt

Welcome to the Tezos Technical Documentation!
=============================================

To start browsing, either follow one of the guided paths below, or directly pick any topics in the documentation menu.

.. image:: images/discover_tezos_1.png
    :alt: Getting started

.. raw:: html

    <details>
    <summary><a>Never heard of Tezos? Let's get acquainted! (See more) </a></summary><div style="max-width:1000px; margin-top:1em; margin-left:2em">

Tezos is a distributed consensus platform (a blockchain) with meta-consensus
capability.

This means that, unlike other blockchains like Bitcoin or Ethereum, Tezos comes to consensus not only about the state of its ledger, but also about how the protocol and the nodes should adapt and upgrade.

This is a fundamental design choice, allowing Tezos to be seamlessly upgradable and continuosly evolving.
Due to this feature, Tezos is built to last, and always stay at the leading edge of blockchain technology.

To learn more about Tezos, its implementation, and its ecosystem, see :doc:`introduction/tezos`.

.. raw:: html

    </div></details><br/>

.. image:: images/getting_started_2.png
    :alt: Getting started

.. raw:: html

    <details>

    <summary><a>Newcomer to Tezos? Come and participate! (See more) </a> </summary><div style="max-width:1000px; margin-top:1em; margin-left:2em">

Start participating to Tezos by following the `Introduction` section in the documentation menu.

These tutorials explain:

- how to :doc:`get the latest release of Octez <introduction/howtoget>` (a complete, open-source implementation of Tezos) in various forms,
- how to :doc:`start using Octez to join Tezos <introduction/howtouse>`,
- different :doc:`ways to participate to the network <introduction/howtorun>`,

and more.

.. raw:: html

    </div></details><br/>

.. image:: images/using_octez_3.png
    :alt: Using Octez

.. raw:: html

    <details>

    <summary><a>Already a user? Here is everything you need to know. (See more)</a></summary><div style="max-width:1000px; margin-top:1em; margin-left:2em">

If you already installed Octez and can participate in the Tezos blockchain, the most useful resources are grouped in the ``User`` section in the documentation menu.
These pages:

- present the key concepts and mechanisms for setting up Octez, including :doc:`user/setup-client`, :doc:`user/setup-node`, and so on;
- empowers you to take advantage of Octez' basic and more advanced features, such as :doc:`user/key-management`, :doc:`user/multisig`, :doc:`user/logging`, and much more.

If you intend to participate to Tezos not just as a mere user, but also as a baker, you could also check more specialized resources such as the `NL Knowledge Center <https://docs.nomadic-labs.com/nomadic-labs-knowledge-center/>`__ or `Open Tezos <https://opentezos.com>`__.

.. raw:: html

    </div></details><br/>


.. image:: images/understanding_octez_4.png
    :alt: Understanding

.. raw:: html

    <details>

    <summary><a>Want to know how it works? It's no secret, let us explain! (See more)</a></summary><div style="max-width:1000px; margin-top:1em; margin-left:2em">

If you want to know more about the Tezos *technology*, there are several sections in the documentation presenting the main design principles of Tezos, and some high-level implementation principles of Octez:

- Section ``Shell`` introduces the common :ref:`architectural principles of any Tezos implementation <the_big_picture>`, mainly consisting of a "shell" and a "protocol". It also presents in particular the :ref:`architectural principles of the Octez implementation <packages>`. A group of pages in this section detail some major subsystems of :doc:`shell/shell`.

- Sections named ``<name> Protocol`` explain the design principles and the salient features of several Tezos protocols, current or upcoming, such as: the :doc:`active protocol <active/protocol>`, a :doc:`protocol proposal under development <alpha/protocol>`, and possibly some protocol(s) that are currently candidate(s) for future adoption.


.. raw:: html

    </div></details><br/>


.. image:: images/building_on_tezos_5.png
    :alt: Building on Tezos

.. raw:: html

    <details>

    <summary><a>Are you an application developer? Find how to program with Tezos. (See more)</a></summary><div style="max-width:1000px; margin-top:1em; margin-left:2em">

Tezos is an open platform. As any programmable blockchain, its value lies in the increasing base of smart contracts and distributed applications covering various domains, but also in the tools that make the ecosystem easier to use and more efficient, such as wallets, indexers, and many others.

If you are looking for an accessible and pedagogical exposition on how to write smart contracts or Dapps, there are many great resources out there for developing on Tezos, such as the `Tezos Developer Portal <https://tezos.com/developer-portal/>`__ and `Open Tezos <https://opentezos.com>`__.

For Tezos developers, this website mostly provides reference and API documentation, but also some guidelines, including:

- A complete reference of :doc:`active/michelson`
- Important API concepts such as the :doc:`developer/rpc`
- API references such as :doc:`shell/rpc`, :doc:`api/openapi`, or :doc:`api/errors`
- Guidelines for writing smart contracts in Michelson, such as :doc:`developer/michelson_anti_patterns`.

.. raw:: html

    </div></details><br/>


.. image:: images/contributing_to_octez_6.png
    :alt: Contributing

.. raw:: html

    <details>

    <summary><a>Are you a core developer? Here are the nuts and bolts! (See more)</a></summary><div style="max-width:1000px; margin-top:1em; margin-left:2em">

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

    </div></details><br/>


.. toctree::
   :maxdepth: 2
   :caption: Introduction
   :hidden:

   introduction/tezos
   introduction/howtoget
   introduction/howtouse
   introduction/howtorun
   introduction/test_networks
   introduction/get_troubleshooting
   introduction/support

.. toctree::
   :maxdepth: 2
   :caption: User doc
   :hidden:

   user/versioning
   user/setup-client
   user/setup-node
   user/multisig
   user/fa12
   user/logging
   user/various

.. toctree::
   :maxdepth: 2
   :caption: Shell doc
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
   :caption: Alpha Dev Protocol doc
   :hidden:

   alpha/protocol
   alpha/glossary
   alpha/cli-commands
   alpha/rpc

.. toctree::
   :maxdepth: 2
   :caption: Developer doc
   :hidden:

   developer/michelson_anti_patterns
   developer/rpc
   api/errors
   api/openapi

.. toctree::
   :maxdepth: 2
   :caption: Core Developer doc
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
   api/api-inline

.. toctree::
   :maxdepth: 2
   :caption: Protocols
   :hidden:

   protocols/naming
   protocols/015_lima
   protocols/016_mumbai
   protocols/alpha
   protocols/history

.. toctree::
   :maxdepth: 2
   :caption: Releases
   :hidden:

   releases/releases
   releases/version-16
   releases/history
