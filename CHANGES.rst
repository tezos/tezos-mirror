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

General
-------

- Hardened ``lib_bees`` worker lifecycle for OCaml 5.x Eio domains: switched
  the worker registry to Saturn lock-free tables, serialized worker creation
  with mutex protection, added retries under resource pressure, ensured worker
  launch/initialization runs on the Event_loop main switch, and exposed clean
  shutdown for tests. Improves reliability under domain contention and low
  resources. (MR :gl:`!19990`)

Node
----

- **Breaking change** Only the short hash of blocks are output in the following
  events from the block validator ``validation_or_application_failed``,
  ``application_failed``, ``application_failure_after_validation``,
  ``validation_failure``, ``validation_canceled``, ``commit_block_failure``,
  ``validated_block``, ``validation_and_application_success`` and
  ``updated_to_checkpoint`` events. From the prevalidator
  ``request_completed_info`` event. And from the store ``<block_hash> (level:
  <level>)`` into ``<short_block_hash> (level: <level>)`` events. (MR
  :gl:`!19720`)

- **Breaking change** events ``validator.block.validating_block`` level has been
  changed from ``Debug`` to ``Info`` (output in the daily-logs) and now show the long
  block hash, the level, predecessor, fitness and timestamp of the block. (MR
  :gl:`!19720`)

- **Breaking change** ``validator.chain.block_info`` level is now ``Debug``
  (previously ``Info``) and is no longer output in daily-logs. (MR :gl:`!19720`)

- Brassaia context backend is no longer experimental. (MR :gl:`!20079`)

- A new store version (v3.3) is introduced. At node startup, an existing
  ``data_dir`` containing a store with a supported version (v3.1 and v3.2) is
  automatically upgraded to v3.3. (MR :gl:`!19967`)

- **Breaking change** Brassaia becomes the default context backend. During the
  v3.3 store upgrade, any store containing an Irmin context will automatically
  migrate to Brassaia unless the user explicitly opts out using
  ``TEZOS_CONTEXT_BACKEND=irmin``. (MR :gl:`!20079`)

Client
------

Signer
------

Baker
-----

- **Deprecation** The ``adaptive-issuance-vote`` argument (placeholder
  ``vote``) is now deprecated, and will be removed in the next major
  version of Octez. It was meant to decide the activation of the
  Adaptive Issuance feature, and has had no effects since the Paris
  protocol has been voted in. (MR :gl:`!19215`)

- **Deprecation** The ``octez-baker-<protocol>`` binaries are
   deprecated, and will be removed in the next major version of
   Octez. Please use ``octez-baker`` instead, which automatically
   handles protocol switches. (MR :gl:`!19641`)

- Fix issue when connecting to Octez nodes behind a proxy that can rearrange
  chunks. (MR :gl:`!20057`)


Accuser
-------

- **Deprecation** The ``octez-accuser-<protocol>`` binaries are
   deprecated, and will be removed in the next major version of
   Octez. Please use ``octez-accuser`` instead, which automatically
   handles protocol switches. (MR :gl:`!19641`)


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

- **Breaking change**: Debian/Ubuntu packages now install services as disabled
  by default. Users must explicitly enable services with
  ``systemctl enable octez-node`` (and similar for other services) before
  starting them. This prevents accidental service starts during package
  installation or upgrades. (MR :gl:`!19996`)

- Octez-node service is now automatically restarted after package upgrade
  if it was running before the upgrade. If the service was stopped, it
  remains stopped. Note: octez-baker and octez-dal-node services are
  currently NOT automatically restarted after upgrade. (MR :gl:`!19996`)

Smart Rollup node
-----------------

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

- Fix issue when connecting to Octez nodes behind a proxy that can rearrange
  chunks. (MR :gl:`!20057`)

- Fix issue where setting for ``l1_monitor_finalized`` would be ignored from the
  configuration file. (MR :gl:`!20239`)

- Fix issue where finalized blocks would not be notified when using
  ``--l1-monitor-finalized``. (MR :gl:`!20256`)

Smart Rollup WASM Debugger
--------------------------

Data Availability Layer (DAL)
-----------------------------

DAL node
~~~~~~~~

- **Breaking change** The ``/levels/<slot_level>/slots/<slot_index>/status``
  RPC now answers with ``unpublished`` status for unpublished slots instead
  of a 404 empty response. (MR :gl:`!19613`)

- Added RPC ``GET /profiles/{pkh}/monitor/attestable_slots`` to open a monitoring
  stream that emits a JSON ``slot_id`` each time a slot becomes attestable for the
  given public key hash (``pkh``). A slot id is emitted when all shards assigned to
  that ``pkh`` at the corresponding attestation level are available in the DAL
  node's store. If traps are detected within the slot, then it should not be attested,
  so the id is not sent via the stream. (MR :gl:`!19459`)

- The DAL node now starts propagating shards one level after the inclusion of the
  corresponding published slot header operation (i.e., when the operation is finalized),
  instead of two levels after, when the block is finalized. (MR :gl:`!19366`)

- **Breaking change** Slots status are not stored in dedicated files on disk
  anymore, but found in a cache and the skip list. A consequence of this is that
  the ``/levels/<slot_level>/slots/<slot_index>/status`` will only work with nodes that store the
  skip list, and therefore not with observer nodes. Also, the RPC will now answer
  a 500 error if querying a level at which the DAL was not supported instead
  of a 404 error. (MR :gl:`!19471`)

- **Breaking change** Enforced stricter validation for the JSON configuration
  file. Previously, the parser would silently ignore any content that appeared
  after the first valid JSON object. Now, any extraneous data will cause the
  function to return an error. (MR :gl:`!18745`)


Miscellaneous
-------------
