Development Changelog
'''''''''''''''''''''

**NB:** The changelog for releases can be found at: https://octez.tezos.com/docs/CHANGES.html


This file lists the changes added to each version of octez-node,
octez-client, and the other Octez executables. The changes to the economic
protocol are documented in the ``docs/protocols/`` directory; in
particular in ``docs/protocols/alpha.rst``.

When you make a commit on master, you can add an item in one of the
following subsections (node, client, …) to document your commit or the
set of related commits. This will ensure that this change is not
forgotten in the final changelog, which can be found in ``docs/CHANGES.rst``.
By having your commits update this file you also make it easy to find the
commits which are related to your changes using ``git log -p -- CHANGES.rst``.
Relevant items are moved to ``docs/CHANGES.rst`` after each release.

Only describe changes which affect users (bug fixes and new features),
or which will affect users in the future (deprecated features),
not refactorings or tests. Changes to the documentation do not need to
be documented here either.

Breaking changes and deprecated features should be prefixed with the
appropriate tag **Breaking change** or **Deprecation**, and also added
to the breaking changes page in
``docs/introduction/breaking_changes.rst``, section "Upcoming Octez
Release".


General
-------

Node
----

Client
------

Signer
------

Baker
-----

Accuser
-------

Proxy Server
------------

Protocol Compiler And Environment
---------------------------------

Codec
-----

Docker Images
-------------

Packaging
---------

Smart Rollup node
-----------------

- Registered the missing handler for the ``/global/last_cemented_commitment``
  RPC, which previously returned 404 even though the service was declared. (MR
  :gl:`!21757`)

- Skip context reconstruction during ``snapshot import`` when the head's commit
  is already present in the imported context, so non-compact snapshots no longer
  pay for unnecessary PVM replay. Fixes a regression. (MR :gl:`!21810`)

- Add a ``--dal-node`` option to ``snapshot import`` so reconstruction of
  compact snapshots can fetch DAL pages from a DAL node. (MR :gl:`!21810`)

- Make ``snapshot import`` more robust when verifying that the snapshot's
  commitment is published on L1: search around the snapshot's head level
  rather than only at the L1 head, and report a clear error suggesting an
  archive L1 node when the snapshot is older than the savepoint. (MR
  :gl:`!21841`)

- The rollup node no longer exits when the L1 RPC is unreachable at startup;
  the initial connection is retried with the configured
  ``--reconnection-delay`` exponential backoff, matching the existing behaviour
  for runtime disconnections. A ``reconnected`` notice is emitted once the
  connection is re-established. (MR :gl:`!21854`)

Smart Rollup WASM Debugger
--------------------------

Data Availability Layer (DAL)
-----------------------------

DAL node
~~~~~~~~

Miscellaneous
-------------
