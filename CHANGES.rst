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

- The Tallinn protocol (024) has been frozen. The ``octez-baker-PtTALLiN`` and
  ``octez-accuser-PtTALLiN`` executables have been removed.

- Fixed a file descriptor leak in the RPC client: the connection of each
  followed HTTP redirect was never released. This affected every Octez
  executable issuing RPC calls through an endpoint answering redirects
  (e.g. behind a reverse proxy).

- RPC servers now enable TCP keepalive on accepted connections, so that
  connections whose peer has disappeared without closing (e.g. behind a
  NAT or load balancer that drops idle flows) are eventually closed and
  their resources released, instead of being retained until restart.

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

- Fixed the baker silently stopping to include DAL attestations after its DAL
  node restarted or the connection to it was reset or left half-open. The baker
  now detects a stalled ``/profiles/<pkh>/monitor/attestable_slots`` stream (via
  a periodic heartbeat) and reconnects on its own, instead of relying on a clean
  end-of-stream that a reset or half-open connection never delivers. (MR
  :gl:`!22471`)

- Fixed the baker blocking on its DAL node health check: the periodic health
  request was awaited while building attestations, so an unresponsive DAL node
  (e.g. an unreachable or half-open connection) could stall block production and
  attestation for all of the baker's delegates. The health check now runs
  asynchronously, off the consensus path, and is bounded by a timeout.
  (MR :gl:`!22538`)

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

- Octez release Docker images now carry a SLSA provenance attestation
  (``--provenance=mode=max``) and a CycloneDX/SPDX SBOM
  (``--sbom=true``) emitted by BuildKit. The provenance records the
  build inputs (source revision, build args, layer digests, builder
  version); the SBOM enumerates the packages installed in the image.
  Both are attached as OCI referrers preserved across multi-arch
  merges and release promotions. See
  ``docs/introduction/cosign-verify.rst`` for how to inspect them.
  (MR :gl:`!22018`)

Packaging
---------

- Added ``octez-archive-keyring`` Debian package containing the APT repository
  signing key, ensuring users can verify package authenticity even after key
  rotation. The keyring is recommended by the ``octez-node``, ``octez-client``,
  ``octez-signer``, and ``octez-dal-node`` packages.
  (MR :gl:`!21826`)

- Dropped support for Debian 12 (``bookworm``). Debian packages are now only
  built and tested for Debian 13 (``trixie``). Users on Debian 12 can upgrade
  their system to Debian 13 to keep receiving Octez packages, or build Octez
  from source (see the installation documentation).
  (MR :gl:`!22355`)

Smart Rollup node
-----------------

- Fixed the public ``secure`` RPC ACL, which did not deny the mutating
  ``/local/dal/**`` endpoints (``POST /local/dal/batcher/injection``,
  ``POST /local/dal/slot/indices`` and ``POST /local/dal/injection/<id>/forget``).
  On a publicly exposed node an unauthenticated caller could enqueue DAL
  payloads that the operator node published as an operator-funded
  ``dal_publish_commitment`` operation on L1. These endpoints are now denied
  like the other ``/local/**`` mutating endpoints.

- The injector now splits batches when individual operations fail simulation
  with gas exhaustion (instead of repeatedly dropping them), and re-queues the
  operations set aside by a batch split so they are reconsidered in a later
  injection round. This fixes a permanent stall with large backlogs of
  same-tag operations (e.g. cementation). (MR :gl:`!22417`)

- The injector now gives cementation operations strictly higher priority than
  commitment publication, and the rollup node no longer enqueues commitments
  that are beyond the protocol's ``smart_rollup_max_lookahead_in_blocks``
  window. This prevents a backlog of failing publish operations from starving
  cementation when the ``operating`` and ``cementing`` purposes share the same
  key. (MR :gl:`!22414`)

- **Breaking change** The RPC ``GET /global/last_cemented_commitment`` now
  always returns the last cemented commitment ``commitment_hash`` and
  ``level`` known from L1, with the commitment content in an optional
  ``commitment`` field, instead of returning ``null`` when the content is
  not available in the node's storage (e.g. after a snapshot import in
  full mode). (MR :gl:`!22415`)

Smart Rollup WASM Debugger
--------------------------

Data Availability Layer (DAL)
-----------------------------

DAL node
~~~~~~~~

- Fixed an issue where the attester branch of the shard reception callback
  could issue L1 committee RPCs for message levels far beyond the current
  head. The fetch is now skipped when the committee level exceeds
  ``head_level + attestation_lag + validation_slack``. Additionally, the
  application callback in the P2P receive loop is now spawned
  asynchronously, so a slow callback on one peer connection no longer
  delays shard ingestion from other peers.
  (MR :gl:`!22542`)

- The wire encodings for the Gossipsub IHave, IWant, and Prune control
  messages now enforce upper bounds on the lengths of their list fields
  (``message_ids`` for IHave/IWant, ``px`` for Prune), consistent with
  the limits already applied on the send side. (MR :gl:`!22537`)

- Decoupled the storage lifetimes of slot payloads and shards. Shards are now
  always retained for a single, profile-independent period (about 150 levels);
  slot payloads of slot indices in the node's operator profile are retained
  according to ``--history-mode``, which defaults to the new ``archive`` mode
  (keep slot payloads indefinitely). The previous ``--history-mode full`` value
  remains accepted as a synonym for ``archive``, and the configuration file is
  migrated to version 3. (MR :gl:`!21939`)

- Added a ``--ignore-l1-history-check`` flag to the ``run`` command, which
  downgrades the startup check that the L1 node retains enough block history
  from an error to a warning. This is intended for test networks where the
  operator does not control the L1 node and cannot recompile the DAL node. The
  flag is not honored on mainnet, where the check is always enforced. (MR
  :gl:`!21934`)

- Fixed the ``/monitor/synchronized`` RPC, which previously emitted no output
  on a steady-state node already synchronized with L1 (the underlying watcher
  only fires on status transitions). The current L1 crawler status is now
  pushed as the first stream element on subscription. (MR :gl:`!21864`)

- The ``/profiles/<pkh>/monitor/attestable_slots`` streaming RPC now emits a
  periodic heartbeat event (one per level), letting consumers detect a stalled
  connection during periods with no attestable-slot activity. (MR :gl:`!22471`)

- The DAL node now stops if it detects that a registered attester attested a slot
  containing traps, preventing further reward loss. (MR :gl:`!21544`)

- Bounded the amplification reconstruction queue of the DAL node and stopped it
  from enqueuing duplicates. This prevents a sustained flow of shards from
  growing the queue without bound and driving observer/operator nodes out of
  memory. When the bound is reached, excess amplification requests are dropped
  and a warning is logged. (MR :gl:`!22169`)

- Fixed a DAL node crash that could occur after a restart with a persisted
  store: the startup status backfill could leave a slot's status inconsistent
  with the skip-list store, tearing down the node's daemon (gossipsub, P2P and
  RPC) while the process stayed alive. (MR :gl:`!22469`)

- Fixed the attester monitoring logs (``attested slot(s) …``) which, with the
  dynamic attestation lag, could attribute an attestation to the wrong delegate
  and report slots as attested by a baker that did not attest them.
  (MR :gl:`!22472`)

- The DAL gossipsub worker now caps the number of distinct topics a remote peer
  may subscribe to (default 65,536) and the depth of the P2P input queue
  (default 8,192); excess subscriptions and messages are silently dropped.
  (MR :gl:`!22611`)

Miscellaneous
-------------

- Teztale archiver: ``--backup-dir`` now backs up **every** failed POST,
  including ``/rights`` and ``/dal_shards`` which were previously dropped
  silently. Each failed POST is stored verbatim (path + body) as one record, so
  no data is lost when the server is unreachable.

- Teztale archiver: added a ``--send-timeout`` option (default 30s) bounding
  each POST to the teztale-server, and capped the number of concurrent
  in-flight POSTs. A stalled server now makes individual POSTs time out and be
  reported as failures (the usual failure path) instead of leaving the archiver
  hung indefinitely with leaked connections.

- Teztale archiver: added ``--replay-backups`` (requires ``--backup-dir``). When
  set, the archiver periodically re-sends the backed-up POSTs and deletes each
  one once the server accepts it, so the local backup is drained instead of
  growing without bound after an outage.
