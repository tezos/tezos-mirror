Version 25.0
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

From sources
~~~~~~~~~~~~

.. code-block:: shell

  git fetch
  git checkout octez-v25.0
  make clean
  opam switch remove . # To be used if the next step fails
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``octez-v25.0`` Docker images of Octez.

Changelog
---------

- `Version 25.0 <../CHANGES.html#version-25-0>`_
- `Version 25.0~rc1 <../CHANGES.html#version-25-0-rc1>`_
- `Version 25.0~beta1 <../CHANGES.html#version-25-0-beta1>`_
