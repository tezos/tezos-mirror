Version 20.3
============

Version 20 contains a new version (V12) of the protocol environment,
which is the set of functions that a protocol can call.
This new version is used by the :doc:`Paris<../protocols/020_paris>` protocol,
which is the successor of :doc:`Oxford<../protocols/018_oxford>`.
The executables associated with the Nairobi protocol have been removed as this protocol
is now inactive.

Three potentially critical issues were discovered in the ParisB protocol, the protocol proposal selected via Tezos' on-chain governance process, after its injection but before its scheduled activation on Mainnet.
Version 20.0 thus contained the ParisB 2 protocol, a patched protocol upgrade for ParisB
as well as its associated protocol-specific executable binaries (baker, accuser, etc).
These issues were addressed in the `ParisB 2 <https://research-development.nomadic-labs.com/parisB2-announcement.html>`_ protocol, included in Octez v20.0 together with
its associated protocol-specific executable binaries (baker, accuser, etc).
ParisB 2 was included in v20.0 as a *User-Activated Protocol Override* (UAPO).
This meant that nodes running v20.0 activated ParisB 2,
``PtParisBxoLz5gzMmn3d9WBQNoPSZakgnkMC2VNuQ3KXfUtUQeZ`` instead of
the original ParisB proposal before level `#5,726,209 <https://tzkt.io/5726209>`__.

Additional issues were discovered after the activation of ParisB 2, including a critical liveness issue with Smart Rollups.
These issues are solved in the bug-fix protocol upgrade ParisC included in Octez v20.1.
ParisC is included in v20.1 as a *User-Activated Upgrade* (UAU).
This means that nodes running v20.1 will automatically activate ParisC at the end of cycle 749, just before `#5,898,241 <https://tzkt.io/5898241>`_.

Octez v20.1 also includes minor fixes to Smart Rollup nodes addressing issues observed after the activation of Paris. More precisely, the Smart Rollup nodes released with Octez v20 and earlier keep using the constants of the previous protocol even after the activation of a new one. This means that at activation of ParisB Smart Rollup nodes did not use the correct commitment period to compute and publish commitments. To fix that, in Octez v20.1, the Smart Rollup node:

- correctly handles constant updates at protocol activation;
- features a new command ``repair commitments`` to recompute commitments with the correct period if necessary.

Octez v20.2 fixes an implementation bug in the baker binary. We found that, under fringe degraded network health conditions, the baker might not behave as expected by the Tenderbake consensus algorithm.

**Even though this scenario is highly unlikely, we still recommend Tezos bakers to upgrade to at least v20.2.**

Octez v20.3 includes a new version (``3.2``) of the node's storage. The upgrade to this new version must be done manually, using ``octez-node upgrade storage``, before starting a node with a data directory with an earlier storage version.
If you are using Docker images, see the storage upgrade procedure at :ref:`using_docker_images`.

Upgrading the storage is irreversible and not backwards compatible: v20.3 nodes do not support data directories with an earlier storage version and earlier versions of Octez do not support the new version either.

This operation should only take up to 10s in nodes operating in Rolling history mode, which is the recommended setup for bakers. However, it may require around 10 minutes for nodes operating on Full and Archive modes, depending on the performance of the machine. Users must be aware of the downtime during the upgrade that may impact their services.

Changes
-------

Version 20 introduced the following changes or new features:

Node
~~~~

Since Octez version 18, RPCs accept both ``endorsements`` and ``attestations`` as input and/or output, with endorsement as default value but deprecated.
Starting from version 20, ``attestation`` output is now the default. ``endorsement`` is still available, but it is deprecated and will be removed in a future version.

Some deprecated RPCs have been removed. Please check `the changelog <../CHANGES.html#version-20-0>`__ to see the list of removed RPCs.

The RPC ``/health/ready`` has been introduced to get the status of the RPC server.

Starting from Octez v20, we strongly advise nodes and bakers operators to synchronise their clocks using NTP as issued in :ref:`start_node`.

Smart rollup
~~~~~~~~~~~~

The Smart Rollup node now supports snapshots, to allow bootstrapping a rollup node without having
to replay the whole L2 chain since the rollup genesis.

The smart rollup node now allows to export a snapshot, using ``octez-smart-rollup-node snapshot export``, and to import a snapshot, using ``octez-smart-rollup-node snapshot import``.

See :ref:`the Smart Rollup node guide <rollup_snapshots>` for more details on the Smart rollup node snapshots.

DAL
~~~

The DAL node is released with Octez v20.0, as the Paris protocol would activate the :doc:`DAL <../shell/dal_overview>` on Tezos Mainnet upon protocol activation.

Check :doc:`the documentation <../shell/dal_node>` for more details on the DAL node and how to operate it.

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
  git checkout octez-v20.3
  make clean
  opam switch remove . # To be used if the next step fails
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``octez-v20.3`` Docker images of Octez.

You can also install Octez using Opam by running ``opam install octez``.

Packages
~~~~~~~~

Starting from Octez v20, a distribution of packages are provided along with each release. Binaries packages are now available for each Octez executable for the following distributions:

- Debian Linux
- Fedora Linux
- Rocky Linux
- Ubuntu

You may now install binaries by downloading their packages from the `Octez release page
<https://gitlab.com/tezos/tezos/-/releases>`__, browsing to your distribution
and then installing them with your package tool manager.

.. warning::

   If you are upgrading from a different package distributor such as `Serokell's tezos-packaging <https://github.com/serokell/tezos-packaging>`__,
   please pay attention to the possible differences between the two kinds of packages, in
   particular regarding the home directory for the ``tezos`` user.

For more details, see :ref:`the installation instructions <installing_binaries>`.

Changelog
---------

- `Version 20.3 <../CHANGES.html#version-20-3>`_
- `Version 20.2 <../CHANGES.html#version-20-2>`_
- `Version 20.1 <../CHANGES.html#version-20-1>`_
- `Version 20.0 <../CHANGES.html#version-20-0>`_
- `Version 20.0~rc1 <../CHANGES.html#version-20-0-rc1>`_
