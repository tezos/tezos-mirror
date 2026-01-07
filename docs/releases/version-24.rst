Version 24.0
============

Changes
-------

Summary
~~~~~~~

Version 24 introduces the following changes or new features:
  (1) Support of the **Tallinn Protocol proposal** (See :ref:`the protocol support section <protocol_support_v24>`)
  (2) Deprecation of the protocol-dependent executables  (See :ref:`the protocol-dependent executables section <octez_baker_v24>`)
  (3) New Octez Releases Page (See :ref:`the Octez Releases page section <octez_releases_page_v24>`)

An overview of breaking changes and deprecations introduced in Octez
v24 can be found :ref:`here <v24_breaking_changes>`.

.. _protocol_support_v24:

Protocol Support
~~~~~~~~~~~~~~~~

Version 24 is compatible with all protocols that depend on a protocol environment version up to V15
That includes `the Tallinn protocol proposal <https://research-development.nomadic-labs.com/tallinn-announcement.html>`__.

.. _octez_baker_v24:

Octez-baker executable
~~~~~~~~~~~~~~~~~~~~~~

Version 23 introduced protocol-independent baker and accuser executables, which are capable of operating with any supported protocol, and can work seamlessly across protocol upgrades – provided the protocol being activated is supported.

These executables replace protocol-dependent executables. As a result, the latter are deprecated starting from Octez v24 and will be removed in v25.

.. _octez_releases_page_v24:

New Octez releases page
~~~~~~~~~~~~~~~~~~~~~~~

A new Octez Releases website is now available at https://octez.tezos.com/releases/. It replaces the `Gitlab Releases page <https://gitlab.com/tezos/tezos/-/releases>`__, which is now deprecated.
This new Releases page centralizes  release information for the different Octez :ref:`components <component_releases>` developed on the tezos/tezos Gitlab repository: Octez, Grafazos, Teztale.

Moreover, it provides predictable links to assets (for instance, https://octez.tezos.com/releases/octez-v24.0/binaries/x86_64/octez-v24.0.tar.gz), a `machine-readable releases summary <https://octez.tezos.com/releases/versions.json>`__ file, and an `RSS feed <https://octez.tezos.com/releases/feed.xml>`__.

Minimal hardware specifications
-------------------------------

Our benchmarks suggest the following *minimal* specs for Octez node and baker operators:

- 3 CPU cores: 2 needed by the node and 1 needed by the baker (arm64 or amd64/x86-64)
- 8GB of RAM + 8GB of swap (or 16GB of RAM)
- 100GB SSD storage (or similar I/O performance)
- A low-latency reliable internet connection

This configuration has been tested for running an Octez node in :doc:`rolling history mode <../user/history_modes>`.
Other more specific uses may need adequate configurations.
For instance:

- running a node in full or archive mode requires extra storage space;
- bakers participating to the DAL should consult :ref:`The DAL node requirements <dal_node_specs>`.

Update Instructions
-------------------

This section contains update instructions specialized for the current release, handling only a few installation types that are most common or that require special handling for this version.
For full instructions on updating your Octez suite for any types of installation, refer to :doc:`../introduction/howtoget`.

RPM Octez packages
~~~~~~~~~~~~~~~~~~

.. code-block:: shell

  dnf -y update octez-node  octez-client octez-baker octez-dal-node octez-smart-rollup-node

If you are installing Ubuntu and Debian packages from scratch, follow :ref:`their install instructions <installing_rpm>`

From sources
~~~~~~~~~~~~

.. code-block:: shell

  git fetch
  git checkout octez-v24.0
  make clean
  opam switch remove . # To be used if the next step fails
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``octez-v24.0`` Docker images of Octez.

Changelog
---------

- `Version 24.0~rc2 <../CHANGES.html#version-24-0-rc2>`_
- `Version 24.0~rc1 <../CHANGES.html#version-24-0-rc1>`_
