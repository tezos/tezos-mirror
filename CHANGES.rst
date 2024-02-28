Development Changelog
'''''''''''''''''''''

**NB:** The changelog for releases can be found at: https://tezos.gitlab.io/CHANGES.html


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

General
-------

- Removed binaries for Nairobi (MR :gl:`!12043`)

Node
----

- Introduced a new process, forked by the node, that is responsible of
  managing the RPC server: the RPC-process. It is used by default by
  the node.

- Introduced a new ``--local-rpc-addr`` that starts the RPC server
  locally, not using the dedicated RPC-process.

- Bump RPCs ``GET ../mempool/monitor_operations``, ``POST
  ../helpers/preapply/operations``, ``GET ../blocks/<block>``, ``GET
  ../blocks/<blocks>/metadata``. and ``GET ../blocks/<blocks>/operations``
  default version to version ``1``. Version ``0`` can still be used with
  ``?version=0`` argument. (MR :gl:`!11872`)

- Bump RPC ``GET ../mempool/pending_operations`` default version to version
  ``2``. Version ``0`` has been removed and version ``1`` can still be used
  with ``?version=1`` argument. (MR :gl:`!11872`)

- Bump RPCs ``POST ../helpers/parse/operations``, ``POST
  ../helpers/scripts/run_operation`` and ``POST
  ../helpers/scripts/simulate_operation`` default version to version ``1``.
  Version ``0`` can still be used with ``?version=0`` argument. (MR :gl:`!11889`)

- **Breaking change** Removed the deprecated ``endorsing_rights`` RPC,
  use ``attestation_rights`` instead. (MR :gl:`!11952`)

- Removed the deprecated ``applied`` parameter from RPCs ``GET
  ../mempool/monitor_operations`` and ``GET
  ../mempool/pending_operations``. Use ``validated`` instead. (MR
  :gl:`!12157`)

- Removed the deprecated RPCs ``GET /network/version`` and ``GET
  /network/versions``. Use ``GET /version`` instead. (MR :gl:`!12289`)

- Removed the deprecated RPCs ``GET /network/greylist/clear``. Use ``DELETE
  /network/greylist`` instead. (MR :gl:`!12289`)

- Removed the deprecated RPCs ``GET /network/points/<point>/ban``, ``GET
  /network/points/<point>/unban``, ``GET /network/points/<point>/trust`` and
  ``GET /network/points/<point>/untrust``. Use ``PATCH
  /network/points/<point>`` with ``{"acl":"ban"}``, ``{"acl":"open"}`` (for
  both unban and untrust) or ``{"acl":"trust"}`` instead. (MR :gl:`!12289`)

- Removed the deprecated RPCs ``GET /network/peers/<peer>/ban``, ``GET
  /network/peers/<peer>/unban``, ``GET /network/peers/<peer>/trust`` and ``GET
  /network/peers/<peer>/untrust``. Use ``PATCH /network/peers/<peer>`` with
  ``{"acl":"ban"}``, ``{"acl":"open"}`` (for both unban and untrust) or
  ``{"acl":"trust"}`` instead. (MR :gl:`!12289`)

- Introduced a new RPC ``GET
  /chains/main/blocks/<block>/context/delegates/<pkh>/is_forbidden``, to check
  if a delegate is forbidden after being denounced for misbehaving. (MR :gl:`!12341`)

- Introduced a new ``/health/ready`` RPC endpoint that aims to return
  whether or node the node is fully initialized and ready to answer to
  RPC requests (MR :gl:`!6820`).

Client
------

- Extended the support for the TZT format when using the ``run unit
  tests`` client command. (MR :gl:`!4474`)

- The ``timelock create`` command now takes the message to lock in hexadecimal
  format. (MR :gl:`!11597`)

- Added optional argument ``--safety-guard`` to specify the amount of gas to
  the one computed automatically by simulation. (MR :gl:`!11753`)

- For the protocols that support it, added an
  ``operation_with_legacy_attestation_name`` and
  ``operation_with_legacy_attestation_name.unsigned`` registered encodings that
  support legacy ``endorsement`` kind instead of ``attestation``. (MR
  :gl:`!11871`)

- **Breaking change** Removed read-write commands specific to Nairobi (MR :gl:`!12058`)

Baker
-----

- Added optional ``--pre-emptive-forge-time t`` argument that, when
  set, will cause the baker to pre-emptively forge its block if
  the current level quorum has been reached, and it is the round 0
  proposer of the next level. The amount of time to wait before forging
  is ``round_time - t``. This optimization increases the chance for the
  proposed block to reach quorum by slightly reducing the operation
  inclusion window. Note that a ``t`` value that is too high could
  cause forging to begin too early and result in lower baking rewards.
  If not given, defaults to ``0.15 * block_time``. Set to ``0`` to
  ignore. (MR :gl:`!10712`)

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

Smart Rollup node
-----------------

- Added RPC ``/describe?recurse=true`` to retrieve schema of RPCs for the rollup
  node. (MR :gl:`!10118`)

- Added RPC ``/openapi?protocol={ProtoHash}`` to retrieve the OpenAPI
  specification for RPCs of the rollup node. (MR :gl:`!10118`)

- Introduced a new command ``generate openapi``, to generate the OpenAPI JSON
  specification and output it to stdout. (MR :gl:`!10118`)

- Registered in ``octez-codec`` some of the protocol smart rollup
  related encodings. (MRs :gl:`!10174`, :gl:`!11200`)

- Added Snapshot inspection command. (MR :gl:`!11456`)

- Added Snapshot export options. (MRs :gl:`!10812`, :gl:`!11078`, :gl:`!11256`,
  :gl:`!11454`)

- Added Snapshot import. (MR :gl:`!10803`)

- Pre-images endpoint (configurable on the CLI of the config file) to allow the
  rollup node to fetch missing pre-images from a remote server. (MR
  :gl:`!11600`)

- Higher gas limit for publish commitment operations to avoid their failing due
  to gas variations. (MR :gl:`!11761`)

- **Breaking change** Removed RPC ``/helpers/proofs/outbox?message_index=<index>&outbox_level=<level>&serialized_outbox_message=<bytes>``.
  Use ``helpers/proofs/outbox/<level>/messages?index=<index>`` to avoid generating the ```serialized_outbox_message`` yourself.
  (MR :gl:`!12140`)

- Compact snapshots with context reconstruction. (MR :gl:`!11651`)

- Prevent some leak of connections to L1 node from rollup node (and avoid
  duplication). (MR :gl:`!11825`)

- Playing the refutation games completely asynchronous with the rest of the
  rollup node. (MR :gl:`!12106`)

- Rollup node can recover from degraded mode if they have everything necessary
  to pick back up the main loop. (MR :gl:`!12107`)

- Added RPC ``/local/synchronized`` to wait for the rollup node to be
  synchronized with L1. (MR :gl:`!12247`)

Smart Rollup WASM Debugger
--------------------------

Data Availability Committee (DAC)
---------------------------------

Miscellaneous
-------------

- **Breaking change** Switch encoding of ``nread_total`` field of
  ``P2p_events.read_fd`` in Octez-p2p library to ``Data_encoding.int64`` to fix an
  overflow.
