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

- ``data-encoding``: the binary decoder now bounds its recursion depth,
  rejecting deeply-nested input with a new ``Too_many_recursive_calls`` decode
  error instead of recursing on the native OCaml stack. The bound is above the
  maximum nesting any protocol accepts, so no valid value is affected.

- Building the smart rollup node from source now requires ``libclang`` and
  clang's builtin headers, used by RocksDB's ``bindgen``. Install them from
  your system package manager: ``libclang-dev`` on Debian and Ubuntu.
  (MR :gl:`!22818`, MR :gl:`!22827`)

- Bounded the read that follows the rejection of an oversized RPC request
  body: the remainder is discarded within a bounded time and number of
  bytes, after which the connection is released rather than held for as
  long as the client keeps sending. Without this bound, one endless upload
  per connection slot would suffice to exhaust the server's active
  connection slots. As a consequence, a client still uploading more than
  twice the cap when its request is rejected may see its connection reset
  instead of reading the 413 response. (MR :gl:`!22629`)

- An RPC request body within the cap must now arrive within 60 seconds, with
  no pause over 10 seconds, or the server answers with HTTP 408 and closes the
  connection. Sending a body at the default cap thus takes an upload rate of at
  least about 273 KiB/s on the node and the DAL node (16 MiB cap), and 546 KiB/s
  on the smart rollup node (32 MiB cap). (MR :gl:`!22629`)

- An RPC request whose body ends before the length declared in its
  ``Content-Length`` is now answered with HTTP 400, where the truncated body
  used to be handed to the RPC as if complete. (MR :gl:`!22629`)

Node
----

- The RPC server now rejects with HTTP 413 requests whose body exceeds the cap
  set by the new ``--rpc-max-request-body`` option (in bytes, 16 MiB by
  default).

- **Breaking change** The node ``/metrics`` endpoint is no longer served
  on the ``--rpc-addr`` (or ``--external-rpc-addr``) listener; querying
  ``/metrics`` on the RPC port now returns ``404``. Metrics are served
  only by the dedicated metrics server enabled with ``--metrics-addr``.
  This aligns the node with the smart rollup and
  DAL nodes. Setups scraping ``/metrics`` on the RPC port must now run the
  node with ``--metrics-addr``.

- Added the RPC ``GET /chains/<chain>/blocks/<block>/context/smart_rollups/all/signals``
  for protocols Ushuaia (025) and Alpha, returning the signals recorded by the
  protocol, each with the level of the block that recorded it.
  (MR :gl:`!22838`)

- Fixed handling of the source optional argument of the ``POST
  chains/<chain>/blocks/<block>/helpers/scripts/run_script_view`` RPC
  for Ushuaia and Alpha protocols. The argument had no effect until
  now, it now controls the value returned by the ``SENDER``
  instruction in the called view. (MR :gl:`!22791`)

Client
------

- Added an ``--unlimited-gas`` switch to the ``typecheck script`` and
  ``typecheck data`` commands of ``octez-client``. Typechecking is then
  performed with unaccounted gas, which produces more detailed error messages.

Signer
------

- Fixed remote signing with tz5 (ML-DSA-44) keys over the TCP and Unix
  socket signers: the client failed to encode the request with
  ``Data_encoding.Write_error(No case matched)``. (MR :gl:`!22917`)

- Fixed a signer started with ``--require-authentication`` answering
  ``bls_prove_possession`` requests to anyone able to reach it:
  authorization was checked on signing requests only, so an
  unauthenticated caller could obtain a proof of possession over a
  public key of its choosing (via ``--override-public-key``). These
  requests now carry an authentication signature, bound to the request
  kind and the overridden public key. (MR :gl:`!22920`)

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

- Made the baker's (pre)quorum accounting more robust: the voting power of
  each delegate is now counted exactly once when tracking the progress of a
  (pre)quorum.

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

- The Octez suite no longer ships ``octez-smart-rollup-node``. The node is now
  released on its own, from a dated tag ``octez-smart-rollup-node-vYYYYMMDD``,
  and listed at https://octez.tezos.com/releases/octez-smart-rollup-node/. Its
  changes are listed in :doc:`/releases/smart_rollup_node_changelog`. Install
  it with octez-manager, the standalone static binaries, or the
  ``tezos/tezos-bare:octez-smart-rollup-node-latest`` Docker image. A
  docker-compose file that runs it next to an Octez node is available at
  :src:`scripts/docker/smart-rollup-node.yml`. The Debian package
  ``octez-smart-rollup-node`` is removed. The other Octez packages are
  deprecated, see :ref:`octez_packages_v25`.

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

- The RPC server now rejects with HTTP 413 requests whose body exceeds the cap
  set by the new ``--rpc-max-request-body`` option (in bytes, 16 MiB by
  default).

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

- Fixed two leaks in the atomic file writer shared by the node, the client and
  the DAL node: when the write it wraps failed or was cancelled, the temporary
  ``.tmp`` file was left behind in the destination directory and the file
  descriptor it had opened was never closed.

- The recommended Rust version was bumped from 1.88.0 to 1.94.0.

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
