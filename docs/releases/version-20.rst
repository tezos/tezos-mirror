Version 20.0~rc1
================

Version 20 contains a new version (V12) of the protocol environment,
which is the set of functions that a protocol can call.
This new version is used by the :doc:`Paris <../protocols/019_paris>`,
protocol proposal for the successor of Oxford.
This release contains the Paris A protocol proposal itself, as well as its associated protocol-specific executable binaries (baker, accuser, etc).
The stable release 20.0 will contain the protocol selected by the ongoing vote, i.e., Paris A or Paris B.

Note that the binaries associated to the Nairobi protocol have been removed as this protocol is now unactive.

Version v20 introduces the following changes or new features:

Node
~~~~

Since Octez version 18, RPCs accept both ``endorsements`` and ``attestations`` as input and/or output, with endorsement as default value but deprecated.
Starting from version 20, ``attestation`` output is now the default. ``endorsement`` is still available, but it is deprecated and will be removed in a future version.

Some deprecated RPCs have been removed. Please check `the changelog <../CHANGES.html#version-20-0-rc1>`_ to see the list of removed RPCs.

The RPC ``/health/ready`` has been introduced to get the status of the RPC server.

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

Update Instructions
-------------------

To update from sources::

  git fetch
  git checkout v20.0-rc1
  make clean
  opam switch remove . # To be used if the next step fails
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v20.0~rc1`` Docker images of Octez.

You can also install Octez using Opam by running ``opam install octez``.

Packages
~~~~~~~~

Starting from Octez v20, a distribution of packages are provided along with each release. Binaries packages are now available for each Octez executable for the following distributions:

- Debian Linux
- Fedora Linux
- Rocky Linux

You may now install binaries by downloading their packages from the `Octez release page
<https://gitlab.com/tezos/tezos/-/releases>`__, browsing to your distribution
and then installing them with your package tool manager.

.. warning::

   If you are upgrading from a different package distributor such as `Serokell's tezos-packaging <https://github.com/serokell/tezos-packaging>`__,
   please pay attention to the possible differences between the two kinds of packages, in
   particular regarding the home directory for the ``tezos`` user.

For more details, see :ref:`the installation instructions <installing_binaries>`.
