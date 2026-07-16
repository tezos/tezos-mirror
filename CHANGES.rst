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

- Fixed a file descriptor leak in the RPC client: the connection of each
  followed HTTP redirect was never released. This affected every Octez
  executable issuing RPC calls through an endpoint answering redirects
  (e.g. behind a reverse proxy).

- RPC servers now enable TCP keepalive on accepted connections, so that
  connections whose peer has disappeared without closing (e.g. behind a
  NAT or load balancer that drops idle flows) are eventually closed and
  their resources released, instead of being retained until restart.

- **Security** Fixed a bug where the in-memory protocol cache could be
  reused across a chain reorganization even though it did not belong to
  the block's predecessor, risking validation against stale entries. The
  cache is now rebuilt from the committed context whenever this is
  detected. Node operators should upgrade promptly. (MR :gl:`!22462`)

- **Security** Fixed a vulnerability in smart rollup refutation game
  validation (protocols PtTALLiN, PsUshuai, Alpha) that could allow an
  invalid refutation move to bypass block validation. Node operators
  and bakers should upgrade promptly. (MR :gl:`!22476`)

- **Security** Fixed a vulnerability (protocol PsUshuai) where malformed
  double-baking evidence was not rejected by block validation, the
  mempool, or the baker. Node operators and bakers should upgrade
  promptly. (MR :gl:`!22484`)

Node
----

- **Breaking change** The node ``/metrics`` endpoint is no longer served
  on the ``--rpc-addr`` (or ``--external-rpc-addr``) listener; querying
  ``/metrics`` on the RPC port now returns ``404``. Metrics are served
  only by the dedicated metrics server enabled with ``--metrics-addr``.
  This aligns the node with the smart rollup and
  DAL nodes. Setups scraping ``/metrics`` on the RPC port must now run the
  node with ``--metrics-addr``.

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
- **Breaking change**: The DAL node RPC server now binds to ``127.0.0.1:10732``
  by default instead of ``0.0.0.0:10732``. Operators who need to expose the RPC
  interface publicly must explicitly pass ``--rpc-addr 0.0.0.0:10732``. When
  bound to a non-loopback address, a warning is emitted and a restrictive ACL is
  applied (only safe/read-only endpoints are accessible). The ACL can be
  overridden per-bind-address via the new ``acl`` field in ``config.json``,
  using the same policy syntax as the L1 node's ``rpc.acl``.

- Added a ``--skip-shards`` flag to the ``snapshot export`` and ``snapshot
  import`` commands. When set, shards are neither exported nor imported,
  producing smaller snapshots for nodes that only need slot data.

Miscellaneous
-------------
