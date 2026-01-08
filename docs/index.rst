.. Tezos documentation master file, created by
   sphinx-quickstart on Sat Nov 11 11:08:48 2017.
   You can adapt this file completely to your liking, but it should at least
   contain the root ``toctree`` directive.

.. TODO https://gitlab.com/tezos/tezos/-/issues/2170:
   search shifted protocol name/number & adapt

Welcome to the Octez and Protocol Documentation!
================================================

This documentation describes an implementation of the `Tezos blockchain <https://tezos.com>`__ consisting of the **Octez toolsuite** and the **Tezos protocol**, for technical users of various kinds, including Tezos application developers, bakers, and Octez/protocol developers.
Enjoy reading this website and consider using the feedback button available in any page (:ref:`more details <feedback_doc>`) to improve it.
If you have questions about Tezos, consider using the ``Ask AI`` button available on the right side of each page (`more details <https://docs.tezos.com/overview/chatbot>`__) before reaching out to the :ref:`Tezos community <tezos_community>`.

.. note::

  For documentation of the whole Tezos ecosystem, including software beyond the Octez & protocol platform, such as smart contracts languages, developer tools, or web3 libraries, see https://docs.tezos.com.
  Training material about the Tezos ecosystem, consisting of learning modules with exercises, can be found at https://opentezos.com.

To start browsing the Octez & Protocol documentation, directly pick any topics in the menu on the left, or click one of the guided paths below:

.. raw:: html

    <details>
    <summary style="background-color:#019CF2"><b>Discover Octez & the Tezos protocol</b>
    </summary><div style="max-width:min(90%,1000px); margin-top:1em; margin-left:2em">

**Never heard of Octez?** Let's get acquainted!

Octez & the Tezos protocol are an implementation of the `Tezos blockchain <https://tezos.com>`__ , a
distributed consensus platform with meta-consensus
capability.

This means that, unlike other blockchains like Bitcoin or Ethereum, Tezos comes to consensus not only about the state of its ledger, but also about how the protocol and the nodes should adapt and upgrade.

This is a fundamental design choice, allowing Tezos to be seamlessly upgradable and continuously evolving.
Due to this feature, Tezos is built to last, and always stay at the leading edge of blockchain technology.

To learn more about Tezos, see <https://tezos.com>.

To learn more about how Octez & the protocol fit into Tezos and its ecosystem, see :doc:`introduction/tezos`.

.. raw:: html

    </div></details><br/>

    <details>
    <summary style="background-color:#019CF2"><b>Getting started with Octez</b>
    </summary><div style="max-width:min(90%,1000px); margin-top:1em; margin-left:2em">

**Newcomer to Octez?** Start participating in Tezos using Octez!

Start participating in Tezos by following the ``Introduction`` section in the documentation menu.

These tutorials explain:

- how to :doc:`get the latest release of Octez <introduction/howtoget>` (a complete, open-source implementation of Tezos) in various forms,
- how to :doc:`start using Octez to join Tezos <introduction/howtouse>`,
- different :doc:`ways to participate to the network <introduction/howtorun>`,

and more.

.. raw:: html

    </div></details><br/>

    <details>
    <summary style="background-color:#019CF2"><b>Using Octez</b>
    </summary><div style="max-width:min(90%,1000px); margin-top:1em; margin-left:2em">

**Already a user?** Here is everything you need to know!

If you already installed Octez and can participate in the Tezos blockchain, the most useful resources are grouped in the ``User manual`` section in the documentation menu.
These pages:

- present the key concepts and mechanisms for setting up Octez, including :doc:`user/setup-client`, :doc:`user/setup-node`, for different production or testing configurations;
- empowers you to take advantage of Octez' basic and more advanced features, such as :doc:`user/key-management`, :doc:`user/multisig`, :doc:`user/logging`, and much more.

If you intend to participate to Tezos not just as a user, but rather as a baker, you should also check more specialized documentation such as the  `Baking section on Open Tezos <https://opentezos.com/node-baking/overview/>`__.

.. raw:: html

    </div></details><br/>

    <details>
    <summary style="background-color:#019CF2"><b>Understanding Octez & the protocol</b>
    </summary><div style="max-width:min(90%,1000px); margin-top:1em; margin-left:2em">

**Want to know how it works?** It's no secret, let us explain!

If you want to know more about the *technology* underlying Octez and the Tezos protocol, the ``Reference manual`` section in the documentation present their rationale, main design principles, and some high-level implementation principles:

- Page ``Octez software architecture`` explains how the :ref:`architecture of the Octez implementation <packages>` instantiates the high-level :ref:`architectural principles of any Tezos implementation <the_big_picture>`, consisting in a "shell" and a "protocol" .

- Page ``Octez Shell`` details some major subsystems of :doc:`shell/shell`.

- Page ``Octez Protocol`` explains the design principles and the salient features of the Tezos protocol. In fact, these pages are versioned for several Tezos protocols, current or upcoming, such as: the :doc:`active protocol <active/protocol>`, a :doc:`protocol proposal under development <alpha/protocol>`, and possibly some protocol(s) that are currently candidate(s) for future adoption.

- Other pages are related to the important Smart Rollups feature, and present tools such as the Smart rollup node and the Data Availability Layer.

.. raw:: html

    </div></details><br/>

    <details>
    <summary style="background-color:#019CF2"><b>Developer reference</b>
    </summary><div style="max-width:min(90%,1000px); margin-top:1em; margin-left:2em">

**Are you a Tezos developer?** Find here some useful reference pages!

If you are a developer on the Tezos platform, you must know the `Tezos Developer Portal <https://tezos.com/developers/>`__ or `Open Tezos <https://opentezos.com>`__, giving accessible and pedagogical expositions on how to write smart contracts or Dapps.

This website complements those resources with reference documentation, mostly in section ``Developer reference``, including:

- Principles of the RPC interface such as the :doc:`developer/rpc`
- RPC references such as :doc:`shell/rpc`, :doc:`api/openapi`, or :doc:`api/errors`
- A complete reference of :doc:`active/michelson`
- Guidelines for writing smart contracts in Michelson, such as :doc:`active/michelson_anti_patterns`.

.. raw:: html

    </div></details><br/>

    <details>
    <summary style="background-color:#019CF2"><b>Contributing to Octez & the protocol</b>
    </summary><div style="max-width:min(90%,1000px); margin-top:1em; margin-left:2em">

**Are you a platform developer?** Here are the nuts and bolts!

One major focus of this website is on resources for platform developers, that is, contributors to Octez (Octez developers) and contributors to the Tezos protocol (protocol developers).

Platform developers can find a rich set of explanations, tutorials, and howtos, mainly in the ``Contributing`` section, including:

- a tutorial on the various forms of contributing (:doc:`developer/contributing`), and guidelines such as :doc:`developer/guidelines`
- programming tutorials covering various libraries and frameworks specific to the Octez OCaml implementation, such as using :doc:`developer/gadt`, using :doc:`developer/error_monad`, using :doc:`developer/clic`, :doc:`developer/event_logging_framework`, etc.
- howtos for specific maintenance tasks such as :doc:`developer/michelson_instructions`, :doc:`developer/protocol_environment_upgrade`, or :doc:`developer/howto-freeze-protocols`
- a whole subsection on the :doc:`various testing frameworks <developer/testing_index>` for Octez, explaining how to use them and how to add different kinds of tests
- presentations of various tools for platform developers, such as support for :doc:`developer/profiling` and :doc:`developer/snoop`
- a subsection on :doc:`architecture details<developer/octez_architecture>` providing detailed explanations of the internal structure and design of key components in the Octez codebase.

Platform developers are also provided reference materials for internal APIs of Octez, such as:

- The :doc:`API of OCaml libraries and modules <api/api-inline>` reference
- The :doc:`shell/p2p_api` reference
- The :doc:`developer/merkle-proof-encoding-formats` reference.

.. raw:: html

    </div></details><br/>

You may also access this whole documentation as a single `text file <https://octez.tezos.com/docs/octezdoc.txt>`__.

.. toctree::
   :maxdepth: 2
   :caption: Introduction
   :hidden:

   introduction/tezos
   introduction/howtoget
   introduction/howtouse
   introduction/howtorun
   introduction/services
   introduction/serokell
   introduction/versioning
   BREAKING CHANGES <introduction/breaking_changes>

.. toctree::
   :maxdepth: 2
   :caption: Octez User manual
   :hidden:

   user/setup-client
   user/setup-node
   user/multisig
   user/fa12
   user/logging
   user/exits

.. toctree::
   :maxdepth: 2
   :caption: Octez Reference manual
   :hidden:

   shell/the_big_picture
   shell/shell
   shell/baker
   shell/dal
   shell/smart_rollup_node
   shell/p2p_api
   shell/cli-commands
   shell/rpc

.. toctree::
   :maxdepth: 2
   :caption: Protocol Reference Manuals
   :hidden:

   Seoul Protocol Reference <active/index>
   Tallinn Protocol Reference <tallinn/index>
   Alpha Dev Protocol Reference <alpha/index>

.. toctree::
   :maxdepth: 2
   :caption: Tezos developer Reference
   :hidden:

   developer/rpc
   api/errors
   api/openapi

.. toctree::
   :maxdepth: 2
   :caption: Changes in Octez releases
   :hidden:

   releases/releases
   releases/version-23
   releases/version-24
   releases/history

.. toctree::
   :maxdepth: 2
   :caption: Changes in protocol versions
   :hidden:

   protocols/naming
   protocols/023_seoul
   protocols/024_tallinn
   protocols/alpha
   protocols/history

.. toctree::
   :maxdepth: 2
   :caption: Contributing
   :hidden:

   developer/contributing_index
   developer/programming
   developer/octez_architecture
   developer/testing_index
   developer/maintaining
   README
   developer/tools
   developer/encodings
   developer/merkle-proof-encoding-formats
   api/api-inline

.. toctree::
   :maxdepth: 2
   :caption: Grafazos
   :hidden:

   grafazos-doc/index
