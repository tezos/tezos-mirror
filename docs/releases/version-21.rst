Version 21.4
============

Version 21 contains a new version (V13) of the protocol environment.
As a result, Octez version 21 is the first version compatible with all protocols that require this environment.

Note that starting from v21, Octez releases will not distribute the ``octez-evm-node``. You can refer to the `Etherlink documentation <https://docs.etherlink.com/network/evm-nodes>`__ for information on how to get its latest version.

Octez v21.2 minor version is a bug fix meant for users of the ghostnet network.
This version is required to be able to participate on ghostnet, unless
you are starting from a snapshot that is more recent than level
``10,062,848``. It has no impact on any other networks.

Octez v21.3 brings a new option for the DAL node, in anticipation of the next major version and improvement to the Rollup node. More information on the specific section for :ref:`the DAL node <dal_node_v21_changes>` and :ref:`the Smart Rollup Node <smart_rollup_node_v21_changes>`

Changes
-------

Version 21 introduces the following changes or new features:

Octez node storage upgrade
~~~~~~~~~~~~~~~~~~~~~~~~~~

This new version allows the storage to scale to the increasing number of blocks per cycle, paving the way to reducing minimal block time further.
Octez v21 bumps the minimal storage and snapshot versions. When running a node with the legacy data directory, the user will encounter a message of the form::

  octez-node: Error:
              The data directory version is too old.
              Found '3.1', expected '3.2'.
              It needs to be upgraded with `octez-node upgrade storage`.

which they can solve using the following command::

  ./octez-node upgrade storage --data-dir <data-dir>

This operation should only take up to 10s in nodes operating in Rolling history mode, which is the recommended setup for bakers. However, it may require around 10 minutes for nodes operating on Full and Archive modes, depending on the performance of the machine. Users must be aware of the downtime during the upgrade that may impact their services.

Faster propagation of lower round block proposals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In previous Octez versions, the baker binary applied the block they
were creating from round 1 and beyond. This was a safety measure in
case the created block was valid but not applicable by the nodes.
The experience with the past 4 protocols proved that this safety net
was over-dimensioned, as no such issues were detected ever since.
Hence, starting with v21 and the Quebec protocol proposal, the Octez
baker binary will only apply rounds 3 and above block proposals, as
the new safety net.

Not applying round 1 -- 2 block proposals by default while creating
them will allow for faster propagation and earlier consensus
agreement. See :ref:`here <baker_manual>`
for further details on how bakers can fine-tune this parameter.

.. _dal_node_v21_changes:

DAL node
~~~~~~~~

Octez v21.0 increases the stability of the DAL node. In particular, the changes include:
faster loading of cryptographic material (the so-called SRS), reduced memory and
disk footprints, an improved RPC interface, resilience against Octez node's
disconnections. Also, after a restart, the DAL node cleans-up its storage and
catches up the Octez node. Finally, the DAL node can now start one of the 4 profiles:
bootstrap, attester, operator, and observer; the last two profiles match the
corresponding modes of an accompanying smart rollup node.

.. warning::

   The binary encoding of ``/config/netwok/dal`` changed.
   As a result, the v21 DAL node is not compatible with earlier versions of Octez -- it cannot interact with earlier versioned Octez nodes. If you are operating a Tezos DAL node, make sure to upgrade all Octez infra to v21.1 (or at least v21.0).
   Also, for attesters, it is recommended that they start their DAL node using the argument ``--attester-profile=<pkh>`` where <pkh> is the attester's address (not the alias).

Octez v21.1 contains several improvements concerning the DAL, mainly concerning network reliability and diagnostics enabling bakers to detect more easily whether their DAL node is working as expected.
In particular, the DAL node now does the following.
(1) It keeps established connections alive.
(2) It retries DNS resolution of bootstrap nodes' domain names every five minutes in order to not lose connections when their IP address changes.
(3) It can maintain a connection even when a peer has changed its P2P identity recently.
The baker daemon now fetches attestation statuses one level in advance, thus not delaying the injection of attestation operations.
The DAL node also emits more warnings for potential issues, such as attesters not attesting correctly or the node lagging (w.r.t. L1 node).

Octez v21.3 introduces a new option in the Baker daemons, ``--without-dal``. This option disables the DAL support for the baker.
This option **is not mandatory**, but a warning will be triggered if not used while running a baker without a DAL node.

.. warning::

   This option will be mandatory when running a baker without a DAL node in the next major version (Octez version 22). We recommend to use this option starting with Octez v21.3, or to run a DAL node. See :doc:`the DAL node documentation <../shell/dal_node>` for futher details.

.. _smart_rollup_node_v21_changes:

Smart Rollup Node
~~~~~~~~~~~~~~~~~

Octez v21.0 brings forth many quality of life improvements to the Smart Rollup
node, driven by the deployment of Etherlink Mainnet beta in May. These include:

- New SQLite store backend which is more robust. The store will be migrated on
  startup in the background so no user action is required, but the performance
  of the rollup node may be slightly degraded during that time. Importing
  snapshots with the old format will take more time as the store will be
  migrated during the final phase. Existing stores can also be migrated manually
  using the command ``migrate store``.
- The rollup node's *garbage collector* (the mechanism enabling it to only keep
  most recent rollup states) is now leaner and more reliable.
- New exposed metrics which can be used to monitor the rollup state (notably
  its commitments) and the node’s health.
- Opt-in automated execution of outbox messages automatically. This allows for
  an improved UX for decentralized applications relying on the outbox of a
  Smart Rollup, like the Etherlink native bridge.

Octez v21.3 brings improvements to the Rollup node, especially to its use for the Etherlink EVM nodes.
In particular, the batcher and injector now use heap structures for their queues which reduces the catch up time of the sequencer.

Better logging UX
~~~~~~~~~~~~~~~~~

Octez v21.0 introduces several UX improvements to the :doc:`logging infrastructure <../user/logging>` including:

- Clearer reporting of outdated nonces in baker logs
- Listing the set of delegates used by a baker at startup
- Coloured logs for most Octez binaries (including the node, baker and accuser)

Please see the `Changelog`_ for further information and other optimizations.

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
- bakers participating to the DAL should consult `this article <https://forum.tezosagora.org/t/hardware-and-bandwidth-requirements-for-the-tezos-dal/6230>`__.

Update Instructions
-------------------

To update from sources:

.. code-block:: shell

  git fetch
  git checkout octez-v21.4
  make clean
  opam switch remove . # To be used if the next step fails
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``octez-v21.`` Docker images of Octez.

You can also install Octez using Opam by running ``opam install octez``.

Packages
~~~~~~~~

The packages, as introduced since Octez version 20, are now **available via APT repository** (See :ref:`the documentation <installing_packages>`) for Ubuntu noble and jammy and Debian bookworm.
We recommend users to install them from the APT repository corresponding to their Linux distribution, rather than from the links available in the GitLab release page.

In addition, a **new set of Debian packages is now available for testing**. These packages will replace the current available packages in the future.
Check :ref:`the documentation <installing_packages>` for more details.

Changelog
---------

- `Version 21.4 <../CHANGES.html#version-21-4>`_
- `Version 21.3 <../CHANGES.html#version-21-3>`_
- `Version 21.2 <../CHANGES.html#version-21-2>`_
- `Version 21.1 <../CHANGES.html#version-21-1>`_
- `Version 21.0 <../CHANGES.html#version-21-0>`_
- `Version 21.0~rc3 <../CHANGES.html#version-21-0-rc3>`_
- `Version 21.0~rc1 and rc2 <../CHANGES.html#version-21-0-rc1-and-rc2>`_
