:orphan:

Changelog Smart Rollup Node
'''''''''''''''''''''''''''

Version 20251031 (2025-10-31) 🎃
================================

- Update opentelemetry library to 0.12 which should fix the issue where a log
  protobuf encoding crashes the node when telemetry is activated. (MR
  :gl:`!19516`)

- Ensure metrics are initialized before starting metrics server. (MR
  :gl:`!19707`)

- Allow to only monitor finalized L1 blocks with CLI switch
  ``--l1-monitor-finalized``. This allows a more efficient processing when the
  consumer is only interested in finalized blocks. (MR :gl:`!19568`)

- New RPC **GET** ``/global/monitor_finalized_blocks`` to stream only finalized
  blocks (similarly to ``/global/monitor_blocks``). (MR :gl:`!19568`)

- Fix streaming RPC ``/global/monitor_blocks``
  (resp. ``/global/monitor_finalized_blocks``) which could return an empty body
  if they were called before the first (resp. finalized) block is produced. (MR
  :gl:`!19569`)

- Reduce number of RPCs to L1 node by fetching chain id on startup. (MR
  :gl:`!19788`)

- The rollup node now properly supports DAL on Shadownet. (MRs :gl:`!19765`,
  :gl:`!19809`)

Version 20250911 (2025-09-11)
=============================

- Improved Opentelemetry traces for Etherlink block evaluation (MR :gl:`!18592`)

- The rollup node now exports its logs to Opentelemetry when enabled in the
  configuration file or with `--profiling true`. (MR :gl:`!18910`)

- The rollup node now ensures pending telemetry data is exported on exit (MR
  :gl:`!19157`)

- Allow patching durable storage from a file using the CLI command ``patch
  durable storage`` (for development purposes only). (MR :gl:`!19195`)

Version 23.0 (2025-08-21)
=========================

- The RPCs ``/global/block/{block_id}/dal/slot_headers`` and
  ``/global/block/{block_id}/dal/processed_slots`` have been deleted, since they
  are now obsolete, the DAL-related indexing logic having been moved out of the
  rollup-node (MR :gl:`!17466`).

- **Breaking change** Enforced stricter validation for the JSON configuration
  file. Previously, the parser would silently ignore any content that appeared
  after the first valid JSON object. Now, any extraneous data will cause the
  function to return an error. (MR :gl:`!18745`)

Version 20250627 (2025-06-27)
=============================

- Add query parameter ``outbox_level`` for RPCs
  ``/local/outbox/pending/executable`` and
  ``/local/outbox/pending/unexecutable``. (MR :gl:`!16831`)

- Add a new RPC ``/local/outbox/pending`` to fetch all known outbox messages
  with their status. (MR :gl:`!16831`)

- Add a CLI argument ``--config-file`` to allow specifying a configuration file
  outside the data directory. (MR :gl:`!17225`)

- Allow to provide a remote URL for downloading snapshots in commands ``snapshot
  info`` and ``snapshot import``. (MRs :gl:`!17407`, :gl:`!17420`, :gl:`!17477`)

- Display logging levels in logs by default. (MR :gl:`!17479`)

- Allow to import snapshots from standard input with ``-``. (MR :gl:`!17463`)

- Retire old store implementation. The rollup node cannot read stores produced
  by versions < v21.0 anymore. (MR :gl:`!17933`)

- Aggregate performance metrics for all child processes (including Irmin
  GC). (MR :gl:`!17973`)

- Add new commands ``replay block`` and ``replay blocks`` to replay one or
  multiple L1 blocks. The results are compared with what the rollup node has
  stored on disk. (MR :gl:`!18160`)

- Produce Opentelemetry traces with ``--profiling``, configurable with a field
  ``"opentelemetry"`` in the configuration. (MRs :gl:`!18274`, :gl:`!18290`,
  :gl:`!18331`, :gl:`!18344`, :gl:`!18368`, :gl:`!18433`, :gl:`!18434`,
  :gl:`!18441`, :gl:`!18466`)

- The rollup node now identifies Etherlink rollups (the rollup can also be
  forced to be identified as Etherlink with the CLI switch ``--etherlink``) and
  emit events for processed Etherlink blocks in the logs and in the traces. (MR
  :gl:`!18466`)

Version 22.1 (2025-06-11)
=========================

- Fixed an issue with the execution of outbox messages (e.g. withdrawals on
  Etherlink) during reorgs, when messages are not in the new chain. (MR :gl:`!17961`)

Version 22.0 (2025-04-07)
=========================

- In the bailout mode there was a bug where the wrong key was used
  when recovering the bond. The node uses the ``cementing`` key and not
  the ``operating`` key. (MR :gl:`!16016`).

- Extended the ``DELETE /admin/injector/queues`` RPC endpoint with new query to
  clear injector queues based on priority order. The RPC can takes two
  optional arguments:

  + ``order_below``: an integer that filters out all operations with
    order strictly inferior to it.

  + ``drop_no_order``: a boolean that if true remove all operations
    that has no order specified. ``false`` by default.

  When ``tag`` is specified only operation of that type will be
  considered, else all operations are considered.(MR :gl:`!15929`)

- Added an RPC ``DELETE /admin/batcher/queue`` endpoint, which can take two optional
  arguments:

  + ``order_below``: an integer that filters all messages with order
    inferior to it.

  + ``drop_no_order``: a boolean that if true remove all messages that
    has no order specified. ``false` by default. If no ``order_below``
    is specified it completely clear the queue.

  (MR :gl:`!15929`)

- Improved error messages for RPC
  ``/global/block/<block_id>/helpers/proofs/outbox/<outbox_level>/messages?index=<message_index>``. (MR :gl:`!15507`)

- Fixed potential issue with store with SQLite < 3.35. (MR :gl:`!15631`)

- Improved error messages for RPC
  ``/global/block/<block_id>/helpers/proofs/outbox/<outbox_level>/messages?index=<message_index>``. (MR :gl:`!15507`)

- Fix potential issue with store with SQLite < 3.35. (MR :gl:`!15631`)

- Addeed a new CLI switch ``--unsafe-disable-wasm-kernel-checks`` which allows to bypass
  invalid kernel checks in the WASM VM, for use by jstz. (MR :gl:`!15910`)

- Support ``remote`` signer scheme and check remote signer available on
  startup. (MR :gl:`!16651`)

Version 21.3 (2025-01-23)
=========================

- Updated batcher with a new order structure. The RPC
  ``/local/batcher/injection`` now has a new query argument
  possibility ``"order": <int>``. The batcher will batch the
  received chunk with the following priority order: First chunks with
  ascending order then chunks by order of arrival. (MR :gl:`!15672`)

- Injector now uses a heap structure for its queue which allows
  to prioritize operations to send on L1. (MR :gl:`!15864`)

- New RPC to retrieve values under a key in the durable storage
  ``/global/block/<block_id>/durable/wasm_2_0_0/values?key=<key>&offset=<offset>&length=<length>``.
  (MR :gl:`!15627`)

- Added RPCs ``/global/block/<block_id>/committed_status`` to retrieve commitment
  and cementation status for a given block (or an estimated timestamp
  otherwise). (MR :gl:`!15409`)

- Fixed an issue in the background store migration which could make the rollup
  node send old heads in its stream at the end of the migration.  (MR :gl:`!15739`)

- Fixed an issue in the background store migration which could make the rollup
  node send old heads in its stream at the end of the migration.  (MR :gl:`!15739`)

Version 21.0 (2024-11-14)
=========================

- Storage now uses SQLite as a backend instead of the custom indexed-file based
  store. This change makes the rollup node more robust but entails a migration
  of the store data. (MRs :gl:`!15053`, :gl:`!15026`, :gl:`!15059`,
  :gl:`!15073`, :gl:`!15218`, :gl:`!15257`)

- Allow to import snaphosts for older stores by migrating the data on import.
  (MR :gl:`!15422`)

- Fixed a bug which would make injection of messages in the batcher with the RPC
  ``/local/batcher/injection`` fail if called too early. (MR :gl:`!15459`)

- Updated RPC ``/local/batcher/injection`` with a new query argument
  possibility. When the rpc contains ``"drop_duplicate": true`` then
  the batcher will drop the messages that were already injected with a
  previous RPC call.  If ``"drop_duplicate": false`` then the rollup
  node defaults to its the previous behavior, where messages are
  injected again, even if the exact same one was previously
  injected. By default ``"drop_duplicate": false``. (MR :gl:`!13165`)

- RPC ``/health`` now returns meaningful health related data to asses if the
  rollup node operates correctly. Old ``/health`` RPC is renamed to ``/ping``.
  (MR :gl:`!12940`)

- Use a local cache per game for intermediate states of dissections. (MR
  :gl:`!12899`)

- Introduced the 5th version of the WASM PVM, which defaults to a higher tick
  limit to delegate refutability to the kernel. (MR :gl:`!12999`)

- Trigger GC every 1000 blocks (instead of 100) by default to reduce CPU
  consumption. (MR :gl:`!13177`)

- Default history mode is now "full". (MR :gl:`!13178`)

- Allow to import archive snapshots in "full" rollup node. (MR :gl:`!13186`)

- Fixed a bug in how commitments are computed after a protocol migration
  where the commitment period changes. (MR :gl:`!13588`)

- Ensure penultimate commitment is published on snapshot export as a
  failsafe. (MR :gl:`!13544`)

- Included commitment publication information in snapshots. (MR :gl:`!13724`)

- Under-approximate publication level for cementation when it is missing. (MR
  :gl:`!13725`)

- New metrics for the rollup node, including performance ones which can be
  enabled with the flag ``--enable-performance-metrics`` (requires
  ``lsof``). (MR :gl:`!12290`)

- Rotate multiple batcher keys in injector so that they are used evenly. (MR
  :gl:`!14194`)

- RPC ``/global/block/<block_id>?outbox=true`` now returns the outbox messages
  produced by the PVM for ``block_id`` if the query parameter ``outbox`` is
  present. (MR :gl:`!14140`)

- Introduced the 6th version of the WASM PVM. (MR :gl:`!14493`)

- New RPC ``GET /admin/cancel_gc`` to cancel any on-going garbage collection in
  the rollup node. (MR :gl:`!14693`)

- Refined GC for rollup node is now triggered every ~3 days to make it less
  wasteful on resources. Gc is not run anymore after importing an archive
  snapshot in a full node. (MR :gl:`!14717`)

- The command ``snapshot export`` tries to cancel ongoing GC, if any. Add
  ``--rollup-node-endpoint`` to specify the RPC server endpoint, if the address
  and port of the running node have been changed via command-line arguments. (MR
  :gl:`!14694`)

- Fix an issue which could introduce a discrepancy between the snapshot header
  and its content. (MR :gl:`!14777`)

- The command ``generate openapi`` now exports mimified JSON. (MR :gl:`!14908`)

- The rollup node can be configured to execute outbox message automatically with
  filters. (MRs :gl:`!14498`, :gl:`!14499`)

Version 20.2 (2024-07-16)
=========================

- New command ``repair commitments`` which allows the rollup node to recompute
  correct commitments for a protocol upgrade which did not. (MR :gl:`!13615`)

Version 20.0 (2024-05-28)
=========================

- Added support for custom, and user defined, PVM patches for rollup genesis
  (to be used on private rollups). (MRs :gl:`!12907`, :gl:`!12957`, :gl:`!12983`)

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

- Secure ACL by default on remote connections. Argument ``--acl-override
  secure`` to choose the secure set of RPCs even for localhost, *e.g.*, for use
  behind a proxy. (MR :gl:`!12323`)

- Fixed an issue with catching up on rollup originated in previous protocol with an
  empty rollup node. (MR :gl:`!12565`)

- Fixed issue with catching up on rollup originated in previous protocol with an
  empty rollup node. (MR :gl:`!12565`)

- Added new administrative RPCs ``/health``, ``/version``, ``/stats/ocaml_gc``,
  ``/stats/memory``, and ``/config``. (MR :gl:`!12718`)

- Added administrative RPCs to inspect injector queues and clear them. (MR :gl:`!12497`)

Version 19.2 (2024-04-16)
=========================

- Fixed an issue with the way the rollup node computes dissections in the refutation games. (MR :gl:`!12534`)

Version 19.1 (2024-02-06)
=========================

- Fixed a critical bug that could lead to data loss when chain
  reorganizations happen while a GC is running. (MR :gl:`!11358`)

- Fixed issue with constants fetching during protocol migration. (MR :gl:`!11804`)

Version 19.0 (2024-01-22)
=========================


- A new bailout mode that solely cements and defends existing
  commitments without publishing new ones. Recovers bonds when
  possible, after which the node exits gracefully. (MR :gl:`!9721`, MR
  :gl:`!9817`, MR :gl:`!9835`)

- RPC ``/global/block/<block-id>/simulate`` accepts inputs with a new optional
  field ``"log_kernel_debug_file"`` which allows to specify a file in which
  kernel logs should be written (this file is in
  ``<data-dir>/simulation_kernel_logs``). (MR :gl:`!9606`)

- The protocol specific rollup nodes binaries are now deprecated and replaced
  by symbolic links to the protocol agnostic rollup node. In the future, the
  symbolic links will be removed. (MR :gl:`!10086`)

- Released the protocol agnostic rollup node ``octez-smart-rollup-node`` as part
  of the Octez distribution. (MR :gl:`!10086`)

- Added the rollup node command inside the docker entrypoint (MR :gl:`!10253`)

- Added the argument ``cors-headers`` and ``cors-origins`` to specify respectively the
  allowed headers and origins. (MR :gl:`!10571`)

- Fix header in messages store to use predecessor hash to avoid missing pointer
  in case of reorganization and GC. (MR :gl:`!10847`)

- Added a garbage collection mechanism that cleans historical data before the LCC.
  (MRs :gl:`!10050`, :gl:`!10135`, :gl:`!10236`, :gl:`!10237`, :gl:`!10452`)

- Added a ``history-mode`` option, which can be either ``archive`` or
  ``full``. In ``archive``, the default, the rollup node has the whole L2 chain
  history, no GC happens. In ``full`` the rollup node retains data for possible
  refutations. (MRs :gl:`!10475`, :gl:`!10695`)

- Snapshot export with integrity checks. (MR :gl:`!10704`)

- Now smart rollup node allows multiple batcher keys. Setting multiple
  keys for the batching purpose allows to inject multiple operations
  of the same kind per block by the rollup node. ( MR :gl:`!10512`, MR
  :gl:`!10529`, MR :gl:`!10533`, MR :gl:`!10567`, MR :gl:`!10582`, MR
  :gl:`!10584`, MR :gl:`!10588`, MR :gl:`!10597`, MR :gl:`!10601`, MR
  :gl:`!10622`, MR :gl:`!10642`, MR :gl:`!10643`, MR :gl:`!10839`, MR
  :gl:`!10842`, MR :gl:`!10861`, MR :gl:`!11008` )

Version 18.0 (2023-09-20)
=========================

- Fixed an issue where the rollup node could forget to update its Layer 2 head for a
  block. (MR :gl:`!9868`)

- Remove the batcher simulation. This simulation was generic and could
  not catch problematic transaction. Batcher configuration now has a
  one less field ``simulate``. (MR :gl:`!10842`)

- Faster bootstrapping process. (MR :gl:`!8618`, MR :gl:`!8767`)

- Single, protocol-agnostic, rollup node binary. The rollup node
  ``octez-smart-rollup-node`` works with any protocol and supports protocol
  upgrades. The other protocol specific rollup nodes still exist but will be
  deprecated. (MR :gl:`!9105`)

- Added a new metrics ``head_inbox_process_time`` to report the time the rollup
  node spent to process a new Layer 1 head. (MR :gl:`!8971`)

- **Breaking change** Field ``"messages"`` of RPC ``/global/block/{block_id}``
  now contains *serialized* messages (external messages start with ``01`` and
  internal start with ``00``). (MR :gl:`!8876`)

- **Breaking change** RPC ``/global/helpers/proof/outbox`` is moved to
  ``/global/block/head/helpers/proof/outbox``. (MR :gl:`!9233`)

- Fixed an issue with level association which could allow the rollup node
  to publish invalid commitments. (MR :gl:`!9652`)

Version 17.0 (2023-06-07)
=========================

- Fixed inverted logic for playing a timeout move in a refutation game (MR
  :gl:`!7929`).

- Stopped the node when the operator deposit is slashed (MR :gl:`!7579`).

- Improved computations of refutation games’ dissections (MRs :gl:`!6948`,
  :gl:`!7751`, :gl:`!8059`, :gl:`!8382`).

- Improved WASM runtime performances (MR :gl:`!8252`).

- Made the Fast Execution aware of the newly introduced WASM PVM versionning
  (MR :gl:`!8079`).

- Fixed UX issues related to the rollup node configuration (MRs :gl:`!8148`,
  :gl:`!8254`, :gl:`!8156`).

- Quality of life improvements in the Layer 1 injector (MRs :gl:`!7579`, :gl:`!7673`, :gl:`!7675`, :gl:`!7685`, :gl:`!7681`, :gl:`!7846`, :gl:`!8106`).

- Fixed logs for kernel debug messages (MR :gl:`!7773`).

- New argument ``--boot-sector-file`` to specify a path to the boot sector used
  for the rollup. This is an optional argument that is required *only* if the
  smart rollup was bootstrapped and not originated (MR :gl:`!8556`).

- Fixed legacy run command (MR :gl:`!8547`).

- Fixed missing commitment for genesis by using local computation instead of
  RPC (MR :gl:`!8617`).

- Fixed issue where rollup node believed it disagreed with L1 regarding cemented
  commitments (MR :gl:`!8615`).

- **Breaking Change** Made the simulation RPC take serialized inbox messages
  as inputs instead of external message payloads. It can be used to simulate
  internal messages as well. It means that previously used format of inputs
  needs to be adapted, i.e. they need to be wrapped in external messages. (MR :gl:`!8888`).
