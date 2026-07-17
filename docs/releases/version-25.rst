Version 25.1
============

Changes
-------

Summary
~~~~~~~

Version 25 introduces the following changes or new features:
  (1) Support for the **Ushuaia Protocol proposal** (See :ref:`the protocol support section <protocol_support_v25>`)
  (2) Deprecation of Octez packages (See :ref:`the Octez packages section <octez_packages_v25>`)
  (3) Removal of the adaptive issuance vote runtime argument from the baker (See :ref:`the baker section <baker_v25>`)

An overview of breaking changes and deprecations introduced in Octez
v25 can be found :ref:`here <v25_breaking_changes>`.

Octez v25.1
~~~~~~~~~~~

Octez v25.1 is a security release addressing three vulnerabilities:

- a case where the in-memory protocol cache could be reused across a chain reorganization even though it did not belong to the block's predecessor, risking validation against stale entries;
- a flaw in smart rollup refutation game validation that could allow an invalid refutation move to bypass block validation;
- a flaw (protocol PsUshuai) where malformed double-baking evidence was not rejected by block validation, the mempool, or the baker.

All node operators and bakers should upgrade promptly.

.. _protocol_support_v25:

Protocol Support
~~~~~~~~~~~~~~~~

Version 25 is compatible with all protocols that depend on a protocol environment version up to V16.
That includes the Ushuaia protocol proposal.

.. _octez_packages_v25:

Deprecation of Octez packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Octez v25 deprecates Octez binary packages. They will be removed in Octez v26.
Instead, the experimental ``octez-manager`` tool (see https://octez-manager.tezos.com) will handle installation, configuration,
and running of static binaries in a more user-friendly way than source or binary packages.
This tool is in early development and should be used for testnets only for now.

.. _baker_v25:

Baker: removal of the adaptive issuance vote
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Octez v25 removes the ``--adaptive-issuance-vote`` CLI option of the baker,
as well as the ``adaptive_issuance_vote`` field from the per-block vote
configuration file (``per_block_votes.json``). Both were deprecated in Octez
v24. This vote had no effect since the Paris protocol activated Adaptive
Issuance.

.. warning::

   If your ``per_block_votes.json`` still contains an
   ``adaptive_issuance_vote`` field, the baker will reject it at startup.
   Remove that field before upgrading to v25. Only ``liquidity_baking_vote``
   is accepted.

DAL node: interface changes
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The CLI of the DAL node has been slightly modified to get a more consistent experience with respect to other Octez tools, and to improve behavior with default parameters.
This introduced some minor :ref:`breaking changes <v25_dal_breaking>`.

Minimal hardware specifications
--------------------------------

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

Ubuntu packages
~~~~~~~~~~~~~~~

Starting from Octez v25, the Ubuntu APT repository is named after the
distribution version number instead of its codename:

- ``ubuntu/jammy`` becomes ``ubuntu/22.04``
- ``ubuntu/noble`` becomes ``ubuntu/24.04``
- ``ubuntu/26.04`` is newly supported

For the complete list of supported distributions and the support calendar, see
:doc:`../introduction/linux_support`.

If you installed Octez v24 (or earlier) from the APT repository, the old
codename-based path is no longer updated. Running ``apt-get update`` and
``apt-get upgrade`` will therefore not upgrade you to v25 until you repoint the
repository to the new version-number-based path.

To upgrade, set the ``release`` to your Ubuntu version number and re-add the
repository:

::

  export distribution=ubuntu
  export release=24.04  # or 22.04, 26.04

then follow :ref:`the install instructions <installing_deb>` to refresh
``/etc/apt/sources.list.d/octez.list``, and finally run::

  sudo apt-get update
  sudo apt-get upgrade

Debian users are not affected: the Debian repository keeps using codenames
(``debian/trixie``, ``debian/bookworm``).

From sources
~~~~~~~~~~~~

.. code-block:: shell

  git fetch
  git checkout octez-v25.1
  make clean
  opam switch remove . # To be used if the next step fails
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``octez-v25.1`` Docker images of Octez.

Changelog
---------

- `Version 25.1 <../CHANGES.html#version-25-1>`_
- `Version 25.0 <../CHANGES.html#version-25-0>`_
- `Version 25.0~rc1 <../CHANGES.html#version-25-0-rc1>`_
- `Version 25.0~beta1 <../CHANGES.html#version-25-0-beta1>`_
