Version 23.2
============

Changes
-------

Summary
~~~~~~~

Version 23 introduces the following changes or new features:
  (1) Support of the **Seoul Protocol proposal** (See :ref:`the protocol support section <protocol_support_v23>`)
  (2) Release of the **octez-baker** executable (See :ref:`the octez-baker executable section <octez_baker_v23>`)
  (3) BLS support (See :ref:`the BLS support section <bls_v23>`)
  (4) New snapshot version (See :ref:`the snapshot version section <snapshot_v23>`)

Octez v23.1 addresses an issue with Ubuntu and Debian packages distribution, where an expired PGP key prevented users from updating to v23.0 using apt.

Octez v23.2 fixes an issue affecting ``systemd`` services of Debian and Ubuntu packages that could lead to having two baking binaries running at the same time.

In some cases, this issue could cause bakers to lose consensus rewards or, in certain conditions, could result in double-signing. Consequently, v23.0 and v23.1 Ubuntu and Debian packages were made unavailable via ``apt`` and ``dnf``. The release of v23.2 restores the availability of these distribution packages.

**For all other distributions, this version is the same as Octez v23.1**

An overview of breaking changes and deprecations introduced in Octez
v23 can be found :ref:`here<v23_breaking_changes>`.

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

Similarly, there is also a protocol-independent binary ``octez-accuser`` starting from v23.

.. _bls_v23:

BLS support
~~~~~~~~~~~

Version 23 contains client commands and node RPCs that support `Tz4 delegates <https://research-development.nomadic-labs.com/seoul-announcement.html#aggregated-attestations>`__ and Native multisig.

See https://forum.tezosagora.org/t/heads-up-native-multisig-accounts-in-protocol-proposal-s/6805 to learn more

.. warning::

   The introduction of this feature entailed a few :ref:`breaking changes <operation_encodings_s>`, so it is recommended to update any tool producing or
   processing reveal operations to a Seoul-compatible version…

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

This section contains update instructions specialized for the current release, handling only a few installation types that are most common or that require special handling for this version.
For full instructions on updating your Octez suite for any types of installation, refer to :doc:`../introduction/howtoget`.

Ubuntu and Debian packages
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. note::

  Because the PGP keys have been rotated, users updating packages need to overwrite the previously installed octez.gpg key.
  For example, in order to update to v23.2 from a pre-existing Ubuntu Noble deployment, a user should first update the keys with::

      curl -s "https://packages.nomadic-labs.com/ubuntu/octez.asc" |
          sudo gpg --yes --dearmor -o /etc/apt/keyrings/octez.gpg

  Then, Octez packages users are advised to stop ``systemd`` services before updating to v23.2::

      sudo apt-get update
      sudo apt-get upgrade octez-baker

See the :ref:`install instructions for Ubuntu and Debian packages <installing_deb>` for details, or if you are installing Ubuntu and Debian packages from scratch.

.. warning::

   If you are running the Octez node with a non-default data directory, check instructions :ref:`here <node_as_service>`.

Fedora or Rocky Linux packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: shell

  dnf -y update octez-node  octez-client octez-baker octez-dal-node octez-smart-rollup-node

If you are installing Ubuntu and Debian packages from scratch, follow :ref:`their install instructions <installing_rpm>`

From sources
~~~~~~~~~~~~

.. code-block:: shell

  git fetch
  git checkout octez-v23.2
  make clean
  opam switch remove . # To be used if the next step fails
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``octez-v23.2`` Docker images of Octez.

Changelog
---------

- `Version 23.2 <../CHANGES.html#version-23-2>`_
- `Version 23.1 <../CHANGES.html#version-23-1>`_
- `Version 23.0 <../CHANGES.html#version-23-0>`_
- `Version 23.0~rc2 <../CHANGES.html#version-23-0-rc2>`_
- `Version 23.0~rc1 <../CHANGES.html#version-23-0-rc1>`_
