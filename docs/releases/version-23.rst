Version 23.0~rc2
================

Changes
-------

Summary
~~~~~~~

Version 23 introduces the following changes or new features:
  (1) Support of the **Seoul Protocol proposal** (See :ref:`the protocol support section <protocol_support_v23>`)
  (2) Release of the **octez-baker** executable (See :ref:`the octez-baker executable section <octez_baker_v23>`)
  (3) BLS support (See :ref:`the BLS support section <bls_v23>`)
  (4) New snapshot version (See :ref:`the snapshot version section <snapshot_v23>`)

.. _protocol_support_v23:

Protocol Support
~~~~~~~~~~~~~~~~

Version 23 contains a new version (V15) of the protocol environment.
As a result, Octez version 23 is the first version compatible with all protocols that require this environment, such as `the Seoul protocol proposal <https://research-development.nomadic-labs.com/seoul-announcement.html>`__.

.. _octez_baker_v23:

Octez-baker executable
~~~~~~~~~~~~~~~~~~~~~~

Starting from v23, the ``octez-baker`` executable is released.
This binary is a protocol-independent that dynamically selects the
appropriate baking process based on the active protocol.

The bakers names with protocols are still released, but they will be deprecated in an upcoming version.

.. _bls_v23:

BLS support
~~~~~~~~~~~

Version 23 contains client commands and node RPCs that support `Tz4 delegates <https://research-development.nomadic-labs.com/seoul-announcement.html#aggregated-attestations>`__ and Native multisig.

See https://forum.tezosagora.org/t/heads-up-native-multisig-accounts-in-protocol-proposal-s/6805 to learn more

.. _snapshot_v23:

Snapshot version
~~~~~~~~~~~~~~~~

Octez version 23 has a new snapshot version (version 9), improving baking support just after importing a snapshot.

Snapshots of version 8 exported with previous versions of Octez can still be imported, snapshots of version 9 are not retro-compatible with previous Octez versions.

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

Ubuntu and Debian packages
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: shell

  sudo apt update
  sudo apt update octez-node octez-client octez-baker octez-dal-node octez-smart-rollup-node

If you are installing Ubuntu and Debian packages from scratch, follow :ref:`their install instructions <installing_deb>`

Fedora or Rocky Linux packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: shell

  dnf -y update octez-node  octez-client octez-baker octez-dal-node octez-smart-rollup-node

If you are installing Ubuntu and Debian packages from scratch, follow :ref:`their install instructions <installing_rpm>`

From sources
~~~~~~~~~~~~

.. code-block:: shell

  git fetch
  git checkout octez-v23.0-rc2
  make clean
  opam switch remove . # To be used if the next step fails
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``octez-v23.0-rc2`` Docker images of Octez.

Changelog
---------

- `Version 23.0~rc2 <../CHANGES.html#version-23-0-rc2>`_
- `Version 23.0~rc1 <../CHANGES.html#version-23-0-rc1>`_
