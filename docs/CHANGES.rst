:orphan:

Changelog
'''''''''

Version 24.0
============

No changes compared to octez-v24.0~rc2.

Version 24.0~rc2
================

Nodes
-----

- Fixed a memory leak in lib_bees. (MR :gl:`!20261` and :gl:`!20258`)

Smart Rollup node
-----------------

- Fixed an issue where setting for ``l1_monitor_finalized`` would be ignored from the
  configuration file. (MR :gl:`!20239`)

- Fixed an issue where finalized blocks would not be notified when using
  ``--l1-monitor-finalized``. (MR :gl:`!20256`)

Version 24.0~rc1
================

Node
----

- **Breaking change** Only the short hash of blocks is output in the following
  events from the block validator ``validation_or_application_failed``,
  ``application_failed``, ``application_failure_after_validation``,
  ``validation_failure``, ``validation_canceled``, ``commit_block_failure``,
  ``validated_block``, ``validation_and_application_success`` and
  ``updated_to_checkpoint`` events. From the prevalidator
  ``request_completed_info`` event. And from the store ``<block_hash> (level:
  <level>)`` into ``<short_block_hash> (level: <level>)`` events. (MR
  :gl:`!19720`)

- **Breaking change** ``validator.block.validating_block`` level has been
  changed from ``Debug`` to ``Info`` (output in the daily-logs) and now shows the long
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

Baker
-----

- **Deprecation** The optional baker argument
  ``--adaptive-issuance-vote <vote>`` is now deprecated, and will be
  removed in the next major version of Octez. It was meant to decide
  the activation of the Adaptive Issuance feature, and has had no
  effects since the Paris protocol has been voted in. The
  ``adaptive_issuance_vote`` field of the per-block-vote configuration
  file is similarly deprecated. (MR :gl:`!19215`)

- **Deprecation** The ``octez-baker-<protocol>`` binaries are
   deprecated, and will be removed in the next major version of
   Octez. Please use ``octez-baker`` instead, which automatically
   handles protocol switches. (MR :gl:`!19641`)

- Fixed an issue when connecting to Octez nodes behind a proxy that can rearrange
  chunks. (MR :gl:`!20057`)

Accuser
-------

- **Deprecation** The ``octez-accuser-<protocol>`` binaries are
   deprecated, and will be removed in the next major version of
   Octez. Please use ``octez-accuser`` instead, which automatically
   handles protocol switches. (MR :gl:`!19641`)

Smart Rollup node
-----------------

- Updated opentelemetry library to 0.12 which should fix the issue where a log
  protobuf encoding crashes the node when telemetry is activated. (MR
  :gl:`!19516`)

- Ensured metrics are initialized before starting metrics server. (MR
  :gl:`!19707`)

- Allow to only monitor finalized L1 blocks with CLI switch
  ``--l1-monitor-finalized``. This allows a more efficient processing when the
  consumer is only interested in finalized blocks. (MR :gl:`!19568`)

- New RPC **GET** ``/global/monitor_finalized_blocks`` to stream only finalized
  blocks (similarly to ``/global/monitor_blocks``). (MR :gl:`!19568`)

- Fixed streaming RPC ``/global/monitor_blocks``
  (resp. ``/global/monitor_finalized_blocks``) which could return an empty body
  if they were called before the first (resp. finalized) block is produced. (MR
  :gl:`!19569`)

- Reduced number of RPCs to L1 node by fetching chain id on startup. (MR
  :gl:`!19788`)

- The rollup node now properly supports DAL on Shadownet. (MRs :gl:`!19765`,
  :gl:`!19809`)

- Fixed an issue when connecting to Octez nodes behind a proxy that can rearrange
  chunks. (MR :gl:`!20057`)


DAL node
--------

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


Version 23.3
============

Baker
-----

- Improved keep-alive handling during bootstrap. This feature may still exhibit
  undesirable behavior, but less likely. (MR :gl:`!18717`)

- Baker exits with code 111 if the node becomes unreachable via the
  ``monitor_operations`` RPC for more than 1m15sec. (MR :gl:`!19559`)

Version 23.2
============

No changes compared to octez-v23.1

Version 23.1
============

General
-------

- Fixed a potential resource leak ensuring that the child process spawned to
  execute the uname command is properly waited for. This prevents the creation
  of zombie processes. (MR :gl:`!19122`)

Version 23.0
============

No changes compared to octez-v23.0~rc2.

Version 23.0~rc2
================

General
-------

Node
----

- Fixed snapshot import backward compatibility (MR :gl:`!18791`)

- **Breaking change** Enforced stricter validation for the JSON configuration
  file. Previously, the parser would silently ignore any content that appeared
  after the first valid JSON object. Now, any extraneous data will cause the
  function to return an error. (MR :gl:`!18745`)

Client
------

- **Breaking change** Enforced stricter validation for the JSON files
  manipulated by the client. Previously, the parser would silently ignore any
  content that appeared after the first valid JSON object. Now, any extraneous
  data will cause the function to return an error. (MR :gl:`!18745`)

Signer
------

- **Breaking change** Enforced stricter validation for the JSON files
  manipulated by the signer. Previously, the parser would silently ignore any
  content that appeared after the first valid JSON object. Now, any extraneous
  data will cause the function to return an error. (MR :gl:`!18745`)

Baker
-----

- **Breaking change** Enforced stricter validation for the JSON files
  manipulated by the baker. Previously, the parser would silently ignore any
  content that appeared after the first valid JSON object. Now, any extraneous
  data will cause the function to return an error. (MR :gl:`!18745`)

Agnostic Baker
--------------

- **Breaking change** Enforced stricter validation for the JSON files
  manipulated by the agnostic baker. Previously, the parser would silently
  ignore any content that appeared after the first valid JSON object. Now, any
  extraneous data will cause the function to return an error. (MR :gl:`!18745`)

Accuser
-------

- **Breaking change** Enforced stricter validation for the JSON files
  manipulated by the accuser. Previously, the parser would silently
  ignore any content that appeared after the first valid JSON object. Now, any
  extraneous data will cause the function to return an error. (MR :gl:`!18745`)

Agnostic Accuser
----------------

- **Breaking change** Enforced stricter validation for the JSON files
  manipulated by the agnostic accuser. Previously, the parser would silently
  ignore any content that appeared after the first valid JSON object. Now, any
  extraneous data will cause the function to return an error. (MR :gl:`!18745`)

Smart Rollup node
-----------------

- **Breaking change** Enforced stricter validation for the JSON configuration
  file. Previously, the parser would silently ignore any content that appeared
  after the first valid JSON object. Now, any extraneous data will cause the
  function to return an error. (MR :gl:`!18745`)

Data Availability Layer (DAL)
-----------------------------

DAL node
~~~~~~~~

- **Breaking change** Enforced stricter validation for the JSON configuration
  file. Previously, the parser would silently ignore any content that appeared
  after the first valid JSON object. Now, any extraneous data will cause the
  function to return an error. (MR :gl:`!18745`)

Version 23.0~rc1
================

General
-------

- Removed binaries for Quebec. (MR :gl:`!17983`)

Node
----

- **Breaking change** modified RPC ``../context/contracts/<pkh>``: the result
  now contains, when called on an implicit account, a boolean field ``revealed``
  that tells if the public key of the manager has been revealed.

- Added RPC ``POST /bls/aggregate_public_keys`` to aggregate BLS
  public keys. (MR :gl:`!17461`)

- Added RPC ``POST /bls/aggregate_signatures`` to aggregate BLS
  signatures. (MR :gl:`!17461`)

- Added RPC ``POST /bls/check_proof`` to check a BLS proof. (MR
  :gl:`!17461`)

- Added RPC ``POST /bls/threshold_signatures`` to recover a BLS
  threshold signature. (MR :gl:`!17467`)

- Fixed a race condition in the external process restart mechanism which would
  prevent the validator from properly restarting. (MR :gl:`!15322`)

- Small mempool optimization: conflicts between two valid consensus
  operations are now always resolved in favor of the preexisting
  operation, regardless of contents. (MR :gl:`!18208`)

- **Breaking change** Bumped the snapshot version from 8 to 9, to support
  early baking, just after importing a snapshot. Snapshots of version 8 exported
  with previous versions of Octez (v22) can still be imported. Snapshots of
  version 9 are not retro-compatible with previous Octez versions (MR
  :gl:`!18387`).

- __Experimental__ Add support for UPnP manual port mapping.
  (MR :gl:`!18135`)

Client
------

- Update signer messages encoding. Signing message for BLS (Tz4) addresses now
  contain signing version. (MR :gl:`!16986`)

- Add a new command ``list known remote keys <remote_signer_addr>`` that lists
  the public key hashes known to the remote signer. The signer returns them only
  if it is launched with ``--allow-list-known-keys`` argument. (MR :gl:`!17403`)

- Add a new command ``aggregate bls public keys`` to aggregate BLS
  public keys. (MR :gl:`!17461`)

- Add a new command ``aggregate bls signatures`` to aggregate BLS
  signatures. (MR :gl:`!17461`)

- Add a new command ``create bls proof`` to create a BLS proof. (MR
  :gl:`!17461`)

- Add a new command ``check bls proof`` to check a BLS proof. (MR
  :gl:`!17461`)

- It is possible to specify a different public key for ``create bls proof``
  and ``check bls proof``, using the command line option ``--override-public-key``.
  (MR :gl:`!18110`)

- Add a new command ``share bls secret key <sk> between <n> shares
  with threshold <m>`` to share a BLS secret key between ``n``
  participants so that any ``m`` participants can collaboratively sign
  messages, while fewer than ``m`` participants cannot produce a valid
  signature. Note that this command requires a secret key: make sure
  that you are in a secure environment before using it. Alternatively,
  one can implement their own version of secret sharing. (MR
  :gl:`!17467`)

- Add a new command ``threshold bls signatures`` to recover a BLS
  threshold signature. (MR :gl:`!17467`)

- Added ``--consensus-key`` and ``--companion-key`` arguments setting
  companion or consensus key at the same time as registering a given
  key as a delegate. (MRs :gl:`!17960`, :gl:`!18317`)

- Added ``--consensus-key-pop`` and ``--companion-key-pop`` arguments when updating
  bls consensus or companion key. These argument allow to provide a pre-computed
  proof of possession for the bls key instead of asking the client to compute
  it. (MR :gl:`!18084`)

- Added ``octez-client set companion key for <delegate> to <bls_key>``, setting a
  companion key for the given delegate. (MR :gl:`!17320`)

- Added ``--initial-stake`` argument to stake at the same time as
  registering a given key as a delegate or setting a delegate for a
  contract. (MR :gl:`!18282`)

- Secret key URI is no longer printed if an alias conflict occurs during key
  generation or import. (MR :gl:`!18509`)

- Add a new command ``update delegate parameters`` that allows to change either
  one or both of the staking parameters of a delegate. (MR :gl:`!17472`)

- Added ``--unencrypted`` argument for ``gen keys`` command. By default keys are
  encrypted on mainnet, this argument allow users to explicitly create
  unencrypted keys. (MR :gl:`!18601`)

Signer
------

- Add a ``--allow-list-known-keys`` argument at signer launch to allow client to
  ask for the signer list of known public key hashes. The signer returns ``List
  known keys request not allowed.`` otherwise. (MR :gl:`!17403`)

- Add a ``--allow-to-prove-possession`` argument at signer launch to allow
  client to request proof of possession of known public key hashes. The signer
  returns ``Request to prove possession is not allowed`` otherwise.
  (MR :gl:`!18137`)

- Enables daily logs by default, located under
  ``<base-dir>/logs/octez-signer/``. (MR :gl:`!18429`)

- Rename the preendorsement_high_watermarks and endorsement_high_watermarks
  files in preattestation_high_watermarks attestation_high_watermarks.
  (MR :gl:`!18481`)

Baker
-----

- Enables ``advertises_level`` in baker logs by default, logs are prefixed by
  the logging level. It can be disabled by setting ``"log" : { advertises_level
  : false }`` in the client configuration. (MR :gl:`!17737`)

- Deprecates the adaptive issuance vote from the CLI (and vote file
  configuration), the feature has been enabled and no longer requires a vote.
  Please remove it from your CLI and configuration, as the support will be
  removed in the next release. (MR :gl:`!18138`)

- Updates many baker events to better display information related to
  consensus operations, delegates, and keys. Note that these changes
  may break automatic event parsing. (MR :gl:`!18330`)

Agnostic Baker
--------------

- Deprecates the adaptive issuance vote from the CLI (and vote file
  configuration), the feature has been enabled and no longer requires a vote.
  Please remove it from your CLI and configuration, as the support will be
  removed in the next release. (MR :gl:`!18138`)

- Add support for ``run dal` and all other commands of the ``octez-dal-node`` to
  the baker. (MR :gl:`!18050`)

- Add ``run accuser`` command to replicate the behaviour of the accuser.
  (MR :gl:`!17767`)

- Enables ``advertises_level`` in baker logs by default, logs are prefixed by
  the logging level. It can be disabled by setting ``"log" : { advertises_level
  : false }`` in the client configuration. (MR :gl:`!17737`)

- Fix the support of ``--keep-alive`` for the agnostic baker. (MR :gl:`!17685`)

- The agnostic baker binary becomes ``octez-baker``. (MR :gl:`!17491`, :gl:`!17747`)

- The agnostic baker now has the same CLI as the classical baker, getting rid of the
  ``--`` separator. (MR :gl:`!17348`)

- The agnostic baker has a unified CLI such that incompatibilities between baking arguments
  for consecutive protocols can occur much harder, and will probably generate compilation
  errors. (MR :gl:`!16968`)

- The agnostic baker no longer requires the protocol specific baking binaries, instead
  it directly spawns baking processes using a protocol plugin to retrieve the necessary
  functionalities. (MR :gl:`!16583`)

- Release agnostic baker binary as experimental. (MR :gl:`!16318`)

- Use of a generic watchdog. (MR :gl:`!15508`)

- Change the binary name to ``octez-experimental-agnostic-baker``. (MR :gl:`!16434`)

- Added a mechanism for the agnostic baker to switch on new protocol. (MR :gl:`!15305`)

- Introduced a dummy agnostic baker. (MR :gl:`!15029`)

Overview: The Agnostic Baker is a protocol-independent binary that dynamically determines
and executes the appropriate baking binary based on the active protocol. It continuously
monitors the blockchain state and automatically transitions to the correct binary whenever
a new protocol is detected, such as during migrations or at startup.

Please note that this feature is in an EXPERIMENTAL phase, as clearly suggested by its name.
Therefore, it should NOT be used on ``mainnet``. For further clarifications, you can consult
the README from ``src/bin_agnostic_baker``.

Accuser
-------

- Consensus operations with different slots are no longer denunced, as this no
  longer considered a punishable misbehaviour (MR :gl:`!18049`).

- Can now denounce double consensus operations where one or both
  involved operations are aggregates. (MR :gl:`!18091`)

- Enables daily logs by default, located under
  ``<base-dir>/logs/octez-accuser-<proto>/``. (MR :gl:`!18429`)

- Updated many baker events to better display information related to
  consensus operations, delegates, and keys. Note that these changes
  may break automatic event parsing. (MR :gl:`!18330`)

Agnostic Accuser
----------------

- Add ``octez-accuser`` agnostic accuser binary. This behaves in a similar way
  to the agnostic baker binary, automatically switching the underlying accuser
  process at protocol migration. (MR :gl:`!17738`)

- Enables daily logs by default, located under
  ``<base-dir>/logs/octez-accuser/``. (MR :gl:`!18429`)

Protocol Compiler And Environment
---------------------------------

- Environment V15 uses signature V2. This change impacts the way BLS signatures
  are handled. In previous environments that used signature V1, the BLS
  signatures were expected to be produced with the ``Augmented`` cryptographic
  scheme. Starting from V15, they are expected to be produced with the ``Proof
  of possession`` cryptographic scheme. (MR :gl:`!17036`)

Smart Rollup node
-----------------

- Added query parameter ``outbox_level`` for RPCs
  ``/local/outbox/pending/executable`` and
  ``/local/outbox/pending/unexecutable``. (MR :gl:`!16831`)

- Added a new RPC ``/local/outbox/pending`` to fetch all known outbox messages
  with their status. (MR :gl:`!16831`)

- Added a CLI argument ``--config-file`` to allow specifying a configuration file
  outside the data directory. (MR :gl:`!17225`)

- Allowed to provide a remote URL for downloading snapshots in commands ``snapshot
  info`` and ``snapshot import``. (MRs :gl:`!17407`, :gl:`!17420`, :gl:`!17477`)

- Display logging levels in logs by default. (MR :gl:`!17479`)

- Allowed to import snapshots from standard input with ``-``. (MR :gl:`!17463`)

- Retired old store implementation. The rollup node cannot read stores produced
  by versions < v21.0 anymore. (MR :gl:`!17933`)

- Aggregate performance metrics for all child processes (including Irmin
  GC). (MR :gl:`!17973`)

- Added new commands ``replay block`` and ``replay blocks`` to replay one or
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

- The RPCs ``/global/block/{block_id}/dal/slot_headers`` and
  ``/global/block/{block_id}/dal/processed_slots`` have been deleted, since they
  are now obsolete, the DAL-related indexing logic having been moved out of the
  rollup-node (MR :gl:`!17466`).

Smart Rollup WASM Debugger
--------------------------

- Moved from Released to Unreleased and removed the deb/rpm packages

Data Availability Layer (DAL)
-----------------------------

- Added an option ``--ignore-l1-config-peers`` to run nodes in isolation, without
  trying to connect to peers provided via L1 config (MR :gl:`!17632`)

DAL node
~~~~~~~~

- **Breaking change** The CLI experimental flag ``--sqlite3-backend``
  and its corresponding configuration file field have been removed
  since SQLite is now the default storage backend for storing skip
  list cells of DAL slots. (MR :gl:`!17424`)

- **Feature** The DAL node stores now a peers.json file in its
  directory when it is shutdown with SIGINT. This file is read if it
  exists when starting the DAL node to restore previous known
  connections quickly.

- **Bugfix** When shutting down the DAL node using SIGINT, it does a
  best effort to shutdown properly its running P2P connections

- The DAL node now supports a ``config update`` command to update an
  existing configuration. It takes the same arguments as for the other
  commands. (MR :gl:`!15759`)

- **Breaking change** The configuration value ``metrics-addr`` is now an option.
  It should not break unless the value differs from the default value
  (``0.0.0.0:11733``). The new default value is ``None``, so no metrics are
  exported by default.

- **Breaking change** For the RPCs ``/p2p/gossipsub/topics/peers``,
  ``/p2p/gossipsub/pkhs/peers``, and ``/p2p/gossipsub/slot_indexes/peers``, the
  flag ``subscribed`` is removed and a new flag ``all`` is introduced. The
  default behavior is now to list peers only for topics the current peer is
  subscribed to, while the ``all`` flag can be used to recover the previous
  behavior. (MR :gl:`!14518`)

- Fixed file descriptor leak in resto affecting connections to the L1 node.
  (MR :gl:`!15322`)

- **Feature** The DAL node downloads trusted setup files when launched in observer
   or operator mode. (MR :gl:`!16102`)

- Added a new RPC ``/last_processed_level`` to retrieve the last (finalized) L1
  level processed by a DAL node (MR :gl:`!16420`)

- A warning is now emitted when registering a public key hash (as an attester
  profile) that does not correspond to that of a delegate. (MR :gl:`!16336`)

- Set the message validation function at node startup, fixing
  https://gitlab.com/tezos/tezos/-/issues/7629. (MR :gl:`!15830`)

- A warning has been introduced in case it is observed that the DAL node lags
  behind the L1 node. (MR :gl:`!15756`)

- **Change** The DAL node store version has been upgraded from 1 to 2.
  The DAL node store will automatically upgrade without requiring any
  user action. For users running the DAL node with the
  ``--operator-profiles`` flag enabled, the node now uses SQLite
  specifically for managing skip list cells (MR :gl:`!15780`),
  preventing inode exhaustion. All other stores remain unchanged.

- Added a new RPC ``GET /protocol_parameters/`` that retrieve the protocol
  parameters that the DAL node uses for a given level, which by default is the
  last finalized level the node is aware of. (MR :gl:`!16704`)

- The configuration file was updated to version 2. Unused field ``neighbors``
  has been deleted. Field ``network_name`` is also deleted since it is now inferred
  from the layer 1 node (MR :gl:`!17284`). Profile encoding has been modified
  (MR :gl:`!17200`).

- RPC ``GET /p2p/gossipsub/mesh`` now accepts 2 optional flags ``slot_index`` and
  ``delegate`` which restrict the output mesh to topics related to specified slot index
  or delegate pkh (MR :gl:`!17770`).

- The DAL node now supports retrieving missing slot content from backup URIs
  specified via the ``--slots-backup-uri`` option. Current supported URI schemes
  include http(s):// and file://, allowing both remote and local fallback
  sources. An optional ``--trust-slots-backup-uris`` flag can be used to skip
  cryptographic verification of retrieved data. This is especially useful when
  replaying history or debugging. (MRs :gl:`!18059`, :gl:`!18074`, :gl:`!18124`
  and :gl:`!18181`).

- A command line option ``--config-file`` has been added, allowing to have a
  configuration file out of the data directory. (MR :gl:`!18464`)

- Added two new RPCs to track the synchronization status between the DAL node and the L1 node:

  + ``GET /synchronized``: returns the current synchronization status.

  + ``GET /monitor/synchronized``: provides a streamed view of the status as it changes
    over time.

  These endpoints help operators monitor whether the DAL node is catching up, lagging,
  or fully synchronized with the L1 chain. (MR :gl:`!18686`)

Grafazos
--------

- Fixed netdata metrics used for hardware monitoring, and add more flexibility
  over the mountpoint allowing to observe only / and /opt mountpoints if needed . Also,
  fixed the network IOs panel presentation, avoiding a grafana panel transformation.

- Added a filter on the selected ``node_instance`` variable over all metrics (was
  previously showing data from all sources on some panels even when a specific source had
  been selected in the grafana dashboard's variable)


Miscellaneous
-------------

- Reverted Renamed ``Bls`` file from the crypto library in ``Bls_aug.ml``. (MR :gl:`!17051`).

- Logs: Fixed lines with milliseconds part as ``0000`` so that all timestamps have
  the same width. (MR :gl:`!18040`)

Version 22.1
============

Smart Rollup node
-----------------

- Fixed an issue with the execution of outbox messages (e.g. withdrawals on
  Etherlink) during reorgs, when messages are not in the new chain. (MR :gl:`!17961`)

Version 22.0
============

Node
----

- Added RPC ``GET
  /chains/<chain>/delegators_contribution/<cycle>/<baker_pkh>``, which
  provides a breakdown of all the contributions to the delegation
  portion of the baking power of the baker for the given cycle. (MR
  :gl:`!17406`)

DAL node
--------

- **Deprecation** The CLI experimental flag ``--sqlite3-backend`` and its
  corresponding configuration file field have been deprecated since
  SQLite is now the default storage backend for storing skip list
  cells for DAL slots.

- Aliases have been added to the command line. ``--operator-profiles`` is now
  equivalent to ``--operator`` and ``-E`` is equivalent to ``--endpoint``.
  (MR :gl:`!17496`)

Version 22.0~rc3
================

- Use the correct constant to determine executable or lost outbox messages for the
  ``/local/outbox/pending`` and ``/local/outbox/pending/executable`` RPC endpoints. (MR
  :gl:`!17279`)

Version 22.0~rc2
================

DAL node
--------

- **Feature** The node will detect stalled connections more quickly (on
  Linux-based distributions). This behavior can be controlled via the
  environment variable ``OCTEZ_P2P_TCP_USER_TIMEOUT``. Its default
  value is ``15000``, meaning that it will now take ``15s`` to detect
  a stalled connection (compared to up to ``15`` minutes by default on
  Linux). Users can opt out by setting the value to ``0``. (MR
  :gl:`!16907`)

- Added a new RPC ``GET /published_levels/<level>/known_traps`` that returns the
  trap shards that the DAL node knows. (MR :gl:`!16870`)

Version 22.0~rc1
================

General
-------

- Changed the compiler version to 5.2.1 and added a manual job to compile with
  ocaml 4.14.2. (MR :gl:`!15404`)

- Logging output on TTYs now adapt to the terminal width. (MR :gl:`!12348`)

- Logging output can now advertise the level associated to each events, by
  enabling the ``advertise-levels`` option in the file-descriptor sink URI. (MR :gl:`!16190`)

- Removed binaries for ParisC. (MR :gl:`!16427`)

Node
----

- Changed the default ``history-mode`` from ``Full`` to ``Rolling``. (MR :gl:`!15942`)

- Introduced a specific exit code for the ``octez-node upgrade storage
  --status`` command. It now returns the exit code 1 when an upgrade
  is available. 0 is returned when the storage is up to date. (MR :gl:`!15152`)

- New RPCs ``/chain/{chain_id}/protocols`` (and
  ``/chain/{chain_id}/protocols/{protocol_hash}``) to retrieve protocol
  activation levels of the chain. (MR :gl:`!15447`)

Client
------

- Registered ``operation.bls_mode_unsigned`` encoding. (MR :gl:`!16655`)

- Allow tz4 (BLS) addresses to be registered as delegate and or as consensus
  keys. (MR :gl:`!15302`)

 - **Breaking change** Removed read-write commands specific to ParisC. (MR :gl:`!16431`)

Baker
-----

- The baker emits a warning when it is started with ``--dal-node``, but the DAL
  node has no registered attester, that is, it was not started with
  ``--attester-profiles <manager_key>``. (MR :gl:`!16333`)

- **Breaking change** For ``proto_alpha``, providing the endpoint of a running
  DAL node is required for the baker to be launched, unless opted out with the
  newly introduced ``--without-dal`` option. (MR :gl:`!16049`)

- **Breaking change** The baker daemon ``--dal-node-timeout-percentage``
  argument has been removed. (MR :gl:`!15554`)

Smart Rollup node
-----------------

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

Agnostic Baker
--------------

- The agnostic baker binary becomes ``octez-baker``. (MR :gl:`!17491`, :gl:`!17747`)

- Released agnostic baker binary as experimental. (MR :gl:`!16318`)

- Use of a generic watchdog. (MR :gl:`!15508`)

- Changed the binary name to ``octez-experimental-agnostic-baker``. (MR :gl:`!16434`)

- Added a mechanism for the agnostic baker to switch on new protocol. (MR :gl:`!15305`)

- Introduced a dummy agnostic baker. (MR :gl:`!15029`)

Overview: The Agnostic Baker is a protocol-independent binary that dynamically determines
and executes the appropriate baking binary based on the active protocol. It continuously
monitors the blockchain state and automatically transitions to the correct binary whenever
a new protocol is detected, such as during migrations or at startup.


Protocol Compiler And Environment
---------------------------------

- Added a new version of the protocol environment (V14). (MR :gl:`!15345`)

- Added a new version of the protocol environment (V15). (MR :gl:`!16599`)

Docker Images
-------------

- Fixed the Docker ``octez-snapshot-import`` command to properly pass
  arguments to the snapshot import process. (MR :gl:`!11259`)

Data Availability Committee (DAC)
---------------------------------

- **Breaking_change** DAC node and client have been removed to
  simplify the codebase. (MR :gl:`!14862`)

Data Availability Layer (DAL)
-----------------------------

DAL node
~~~~~~~~

- **Breaking_change** The configuration value ``metrics-addr`` is now an option.
  It should not break unless the value differs from the default value
  (``0.0.0.0:11733``). The new default value is ``None``, so no metrics are
  exported by default.

- **Breaking change** For the RPCs ``/p2p/gossipsub/topics/peers``,
  ``/p2p/gossipsub/pkhs/peers``, and ``/p2p/gossipsub/slot_indexes/peers``, the
  flag ``subscribed`` is removed and a new flag ``all`` is introduced. The
  default behavior is now to list peers only for topics the current peer is
  subscribed to, while the ``all`` flag can be used to recover the previous
  behavior. (MR :gl:`!14518`)

- **Bugfix** When shutting down the DAL node using SIGINT, it does a
  best effort to shutdown properly its running P2P connections

- **Feature** The DAL node stores now a peers.json file in its
  directory when it is shutdown with SIGINT. This file is read if it
  exists when starting the DAL node to restore previous known
  connections quickly.

- **Change** The DAL node store version has been upgraded from 1 to 2.
  The DAL node store will automatically upgrade without requiring any
  user action. For users running the DAL node with the
  ``--operator-profiles`` flag enabled, the node now uses SQLite
  specifically for managing skip list cells (MR :gl:`!15780`),
  preventing inode exhaustion. All other stores remain unchanged.

- **Feature** The DAL node downloads trusted setup files when launched in observer
   or operator mode. (MR :gl:`!16102`)

- The DAL node supports a ``config update`` command to update an
  existing configuration. It takes the same arguments as for the other
  commands. (MR :gl:`!15759`)

- Fixed file descriptor leak in resto affecting connections to the L1 node.
  (MR :gl:`!15322`)

- Added a new RPC ``/last_processed_level`` to retrieve the last (finalized) L1
  level processed by a DAL node. (MR :gl:`!16420`)

- A warning is emitted when registering a public key hash (as an attester
  profile) that does not correspond to that of a delegate. (MR :gl:`!16336`)

- Set the message validation function at node startup, fixing
  https://gitlab.com/tezos/tezos/-/issues/7629. (MR :gl:`!15830`)

- A warning has been introduced in case it is observed that the DAL node lags
  behind the L1 node. (MR :gl:`!15756`)

- Added a new RPC ``GET /protocol_parameters/`` that retrieve the protocol
  parameters that the DAL node uses for a given level, which by default is the
  last finalized level the node is aware of. (MR :gl:`!16704`)

Miscellaneous
-------------

- Renamed ``Bls`` file from the crypto library in ``Bls_aug.ml``. (MR :gl:`!16683`).

Version 21.4
============

Node
----

- Fixed the storage maintenance default value in the config file (MR :gl:`!16744`)

- The node will detect stalled connections more quickly (on
  Linux-based distributions). This behavior can be controlled via the
  environment variable ``OCTEZ_P2P_TCP_USER_TIMEOUT``. Its default
  value is ``15000``, meaning that it will now take ``15s`` to detect
  a stalled connection (compared to up to ``15`` minutes by default on
  Linux). Users can opt out by setting the value to ``0``. (MR
  :gl:`!16907`)

DAL node
--------

- **Bugfix** Fixed the timing of the reconnection to peers attempts. (MR :gl:`!16466`)

- **Feature** A new RPC ``/p2p/gossipsub/reconnection_delays`` which
  provides for each unreachable point, the time remaining until the
  next reconnection attempt. (MR :gl:`!16767`)

- **Feature** Added a new RPC ``GET /p2p/gossipsub/mesh/`` that returns the GossipSub mesh
  (i.e. full data connections per topic) of a peer. (MR :gl:`!16754`)

- **Feature** Added a new RPC ``GET /p2p/gossipsub/fanout/`` that returns the GossipSub
  fanout of a peer. (MR :gl:`!16764`)

- **Bugfix** Fixed the timing of the reconnection to peers attempts. (MR :gl:`!16466`)

- **Bugfix** From v21.2, the ``SO_KEEP_ALIVE`` socket option was used
  for incoming connections only. It is now used with both incoming
  connections and outgoing connections. (MR :gl:`!16820`)

Baker
-----

- Fixed a long time running baker memory leak. (MR :gl:`!16719`)

Version 21.3
============

Baker
-----

- **Deprecation:** For Paris and Quebec protocols, launching a
  baker daemon without specifying a DAL node endpoint is deprecated.
  To opt out of this requirement, use the newly introduced
  ``--without-dal`` option (MR :gl:`!16213`).
  Using one of the CLI arguments ``--dal-node <uri>`` or ``--without-dal`` will be mandatory
  The CLI argument ``--dal-node <uri>`` or ``--without-dal`` will be mandatory
  in the next version of Octez.

Smart Rollup node
-----------------

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

Version 21.2
============

Miscellaneous
-------------

- Fixed an issue on Ghostnet originated from lowering
  ``consensus_rights_delay`` from 3 to 2 with the recent activation of the Quebec protocol. This issue does not affect mainnet, where
  ``consensus_rights_delay`` was already set to 2 by the activation of Paris and will remain
  unchanged with the activation of Quebec. (MR :gl:`!16219`)

Version 21.1
============

DAL node
--------

- Fix a peering issue when the P2P identity changed recently. (MR :gl:`!15977`)

- Do not attempt to connect to a peer we are already connected with. (MR :gl:`!15984`)

- Introduce a timeout preventing too many reconnections to unreachable
  points. (MR :gl:`!16005`)

- Emit various warnings when the registered attester does not seem to attest
  correctly, or when the DAL node seems to be lagging. (:gl:`!15306`,
  :gl:`!15607`, :gl:`!15756`)

- Set the message validation function at node startup, fixing :gl:`#7629`. (MR
  :gl:`!15830`)

- Retry DNS resolution of bootstrap points every 5 minutes. (MR :gl:`!15858`)

- Keep established connections alive. This applies to the Layer 1 node as
  well. (MR :gl:`!15914`)

Baker
-----

- **Breaking change** Removed the baker daemon's
  ``--dal-node-timeout-percentage`` argument. The DAL node now fetches the
  slots' attestation status from the DAL node one level in advance. (MR
  :gl:`!15554`)

- An event at Notice level is now emitted when the delegate is not in the DAL committee,
  that is, it has no assigned shards at the current level. (:gl:`!15846`)

Version 21.0
============

General
-------

- Integrated binaries for Quebec (MR :gl:`!15611`).

Smart Rollup node
-----------------

- Storage now uses SQLite as a backend instead of the custom indexed-file based
  store. This change makes the rollup node more robust but entails a migration
  of the store data. (MRs :gl:`!15053`, :gl:`!15026`, :gl:`!15059`,
  :gl:`!15073`, :gl:`!15218`, :gl:`!15257`)

- Allow to import snaphosts for older stores by migrating the data on import.
  (MR :gl:`!15422`)

- Fixed a bug which would make injection of messages in the batcher with the RPC
  ``/local/batcher/injection`` fail if called too early. (MR :gl:`!15459`)

DAL node
~~~~~~~~

- **Breaking_change** The configuration value ``metrics-addr`` is now an option.
  It should not break unless the value differs from the default value
  (``0.0.0.0:11733``). The new default value is ``None``, so no metrics are
  exported by default.

- **Breaking change** For the RPCs ``/p2p/gossipsub/topics/peers``,
  ``/p2p/gossipsub/pkhs/peers``, and ``/p2p/gossipsub/slot_indexes/peers``, the
  flag ``subscribed`` is removed and a new flag ``all`` is introduced. The
  default behavior is now to list peers only for topics the current peer is
  subscribed to, while the ``all`` flag can be used to recover the previous
  behavior. (MR :gl:`!14518`)

- **Breaking** Changed binary encoding of /config/network/dal. This change is
  not retro-compatible.  As a result, the v21 DAL node is not compatible with
  earlier Octez nodes.

Version 21.0~rc3
================

General
-------

- Integrated binaries for Qena (MR :gl:`!15123`).

DAL node
--------

- Fixed a memory leak in the DAL node.

- Deactivate the metrics server by default

- Fix ``--public-addr`` when the specified port was different
  from the default one. (MR :gl:`!11732`)

Version 21.0~rc1 and 21.0~rc2
=============================

General
-------

- Add and **experimental** switch to enable the use of the Brassaia context
  backend using ``TEZOS_CONTEXT_BACKEND=Brassaia`` environment variable. (MR :gl:`!13054`)

- Removed binaries for Oxford. (MR :gl:`!13795`)

- Removed binaries for ParisB. (MR :gl:`!14026`)

Node
----

- Added a ``source`` argument to ``GET
  /chains/<chain>/mempool/pending_operations`` which allows operations
  to be filtered by source. (MR :gl:`!11278`)

- Added an ``operation_hash`` argument to ``GET
  /chains/<chain>/mempool/pending_operations`` which allows operations
  to be filtered by hash. (MR :gl:`!13977`)

- Added a ``source`` argument to ``GET
  /chains/<chain>/mempool/monitor_operations`` which allows operations
  to be filtered by source. (MR :gl:`!14284`)

- Added an RPC
  ``/chains/<chain>/blocks/<block>/context/smart_rollups/smart_rollup/<sr1...>/consumed_outputs/<outbox_level>``
  that returns the consumed output's indexes for the given outbox
  level. (MR :gl:`!12776`)

- Restart the block application/peer validation once when encountering a context
  error to mitigate the error crashing by replaying the application/validation
  before raising an uncaught failure (MR :gl:`!13398` and :gl:`!13437`)

- Importing a snapshot uses the configuration if it exists. It's
  useful when an archive snapshot contains some
  ``unsafe_pvm_patches``. The same ``unsafe_pvm_patches`` must be set
  in the export and import or the import will fail. (MR :gl:`!13397`)

- When available, add some colors to some event logs: (MR :gl:`!13610`)

   - node_is_ready is now Green
   - synchronization status changes are now Magenta
   - store merge start/end events are now Cyan
   - update to protocol table is now Blue

- Removed Oxford mempool plugin. (MR :gl:`!13798`)

- Removed support for deprecated version ``0`` for RPCs ``GET
  ../mempool/monitor_operations``, ``POST ../helpers/preapply/operations``,
  ``GET ../blocks/<block>``, ``GET ../blocks/<blocks>/metadata``. and ``GET
  ../blocks/<blocks>/operations``. (MR :gl:`!13449`)

- Removed support for deprecated version ``1`` for RPC ``GET
  ../mempool/pending_operations``. (MR :gl:`!13449`)

- Removed support for deprecated version ``0`` from RPCs ``POST
  ../helpers/parse/operations``, ``POST ../helpers/scripts/run_operation`` and
  ``POST ../helpers/scripts/simulate_operation``. (MR :gl:`!13451`)

- **Breaking change** Decommissioned deprecated
  ``/chains/<chain_id>/checkpoint`` RPC endpoint. Use
  ``/chains/<chain_id>/levels/{checkpoint, caboose, savepoint}``, or
  ``/config/history_mode`` instead to obtain the current checkpoint
  for this chain. (MR :gl:`!13880`)

- **Breaking change** Decommissioned deprecated
  ``/monitor/commit_hash`` RPC endpoint. Use ``/version`` instead. (MR
  :gl:`!13879`)

- Removed ParisB mempool plugin. (MR :gl:`!14031`)

- Introduced the external RPC process to reduce the load of the node
  when answering heavy RPCs. This can be enabled using
  ``--external-rpc-addr`` (MR :gl:`!9490`)

- Introduced ``--storage-maintenance-delay`` to allow delaying the
  storage maintenance. It is set to ``auto`` by default, to
  automatically trigger the maintenance whenever it is the most
  suitable. (MR :gl:`!14503`)

- **Breaking change** Bumped the node’s storage version to
  ``3.2``. This new version changes the store’s representation,
  allowing the storage to scale to the increasing number of blocks per
  cycles, thus paving the way to reducing even more the block
  time. Upgrading to this new version must be done manually (using the
  ``octez-node upgrade storage`` command) and is irreversible. (MR
  :gl:`!14211`)

- **Breaking change** Bumped the snapshot version from ``7`` to ``8``,
  in order to support the changes introduced by the ``3.2`` storage
  version. Snapshots of version ``7`` exported with previous versions
  of Octez (``v20``) can still be imported. Snapshots of version ``8``
  are not retro-compatible with previous octez versions (MR
  :gl:`!14398`).

- Environment variable ``TEZOS_USE_YES_CRYPTO_I_KNOW_WHAT_I_AM_DOING`` can be
  set to ``yes`` or ``y`` to use yes-crypto in testing. With yes-cryptography
  activated, signatures are faked and always considered valid. This should be
  used for testing purposes only and/or with extreme care. This can put your
  software at risk of being considered faulty/malicious if it fake signs
  and exploited by attackers if it fake-checks signatures.

- To avoid misusage of ``TEZOS_USE_YES_CRYPTO_I_KNOW_WHAT_I_AM_DOING``
  environment variable, if it is set to 'yes' or 'y', nodes must explicitly be
  launched with the ``--allow-yes-crypto`` argument to run.

- **Breaking change** removed the ``octez-proxy-server`` binary. The
  feature is subsumed by the external RPC server.

Client
------

- The ``--extra-big-maps`` and ``--other-contracts`` command-line
  parameters, which are used to specify contextual information in some
  Michelson-related commands, now allow file names as argument. (MR
  :gl:`!13311`)

- **Breaking change** Removed read-write commands specific to Oxford. (MR :gl:`!13799`)

- **Breaking changes** client's encoding with legacy attestation name are no
  longer supported starting from protocol following ParisC. (MR :gl:`!13454`)

- **Breaking change** Removed read-write commands specific to ParisB. (MR :gl:`!14033`)

- **Breaking change** Removed all bls key related command in favor of
  generics one. All keys that were generated with ``bls gen keys`` can
  be used with usual command of the octez-client (``list``, ``known``,
  ``sign``, ...).  (MR :gl:`!14417`)

Baker
-----

- When available, added some colors to some event logs: (MR :gl:`!13611`)

   - block_injected is now Blue
   - revealing_nonce is now Cyan

- Branch used in consensus operation is now the grandparent block instead of the
  parent block. This is done to avoid having consensus operation branched on
  blocks that are not part of the canonical chain anymore.(MR :gl:`!13619`)

- Removed ``preendorse for`` and ``endorse for`` deprecated commands from baker.
  (MR :gl:`!14096`)

- By default, the Baker only accepts to communicate with nodes of the same or
  more recent version. To allow the Baker to communicate with nodes of older
  version or dev version, use the --node-version-check-bypass or
  --node-version-allowed option. (MRs :gl:`!14044`, :gl:`!14189`)

- The baker accepts a new argument ``--dal-node-timeout-percentage
  <percentage>``, which specifies the percentage of the time until the end of
  round determining the timeout to wait for the DAL node to provide shards'
  attestation status. The default value is 10%. For instance, the default value
  means that if there are 5 seconds left till the end of the round, then the
  baker will wait for ``0.5`` seconds for the DAL attestations' status. (MR
  :gl:`!14480`)

- **Breaking_change** The baker now accepts a new argument,
  ``--force_apply_from_round <round>``, which replaces ``--force-apply``.
  Previously, the baker applied blocks from round 0 if ``--force_apply`` was
  used, and from round 1 otherwise. The default is now set to 3 and can be
  adjusted using ``--force_apply_from_round <round>``. (MR :gl:`!14875`)

Accuser
-------

- When available, add some colors to some event logs: (MR :gl:`!13611`)

   - double_attestation_detected and double_preattestation_detected is now
     Magenta
   - double_attestation_denounced and double_preattestation_denounced is now
     Blue

Protocol Compiler and Environment
---------------------------------

- Added a new version of the protocol environment (V13). (MR :gl:`!12966`)

- Remove with_legacy_attestation_name encodings from the protocol environment.
  (MR :gl:`!14046`)

Smart Rollup node
-----------------

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

Data Availability Layer (DAL)
-----------------------------

DAL node
~~~~~~~~

- Reduce the number of inodes used by a bootstrap node. This fixes an issue
  where the number of inodes used was too high with respect to the disk size. (MR :gl:`!12900`)

- The DAL node's store has been updated, and it is not compatible with
  V20. However, a V20 store is upgraded at startup. (MR :gl:`!13820`)

- The format of the configuration file (and in particular that of profiles) has
  been updated. However, the node is able to read V20 configuration files. (MR
  :gl:`!12968`, MR :gl:`!13787`)

- The profile names have changed, in particular '(slot) producers' are now called
  'operators'. Accordingly, the node has a new argument ``--operator`` that should
  be used instead of ``--producer-profiles``, which is deprecated and will be
  removed at the next release, but still supported. (MR :gl:`!14261`, MR
  :gl:`!14277`)

- The following RPCs have been removed:

  - ``POST /commitments`` (MR :gl:`!12949`), use ``POST /slots`` instead,
  - ``GET /commitments/<c>/proof`` (MR :gl:`!13080`), also use ``POST /slots`` instead,
  - ``PATCH /commitments`` (MR :gl:`!12886`),
  - ``PUT /commitments/<c>/shards`` (MR :gl:`!12949`),
  - ``GET /levels/<int32>/headers`` (MR :gl:`!13044`),

- The paths or method of the following RPCs have been updated:

  - ``GET /commitments/<c>/slot`` is now ``GET /levels/<l>/slots/<i>/content``  (MR :gl:`!13075`),
  - ``GET /levels/<l>/slot_indices/<i>/commitment`` is now ``GET /levels/<l>/slots/<i>/commitment``  (MR :gl:`!13046`),
  - ``POST /pages/<p>/proof`` is now ``GET /levels/<l>/slots/<i>/pages/<p>/proof``  (MR :gl:`!13083`),
  - ``GET /shard/<c>/<s>`` is now ``GET /levels/<l>/slots/<i>/shards/<s>/content`` (MR :gl:`!13095`),
  - ``POST /slot`` is now ``POST /slots`` (MR :gl:`!12949`),
  - ``GET /slot/pages/<c>`` is now ``GET /levels/<l>/slots/<i>/pages`` (MR :gl:`!12880`),
  - ``GET /commitments/<c>/headers`` is now ``GET /levels/<l>/slots/<i>/status`` (MR :gl:`!13055`),
  - ``GET /p2p/peers/list`` is now ``GET /p2p/peers`` (MR :gl:`!14521`).

- Two new RPCs have been added:

  - ``GET /p2p/gossipsub/slot_indexes/peers``
  - ``GET /p2p/gossipsub/pkhs/peers``

  These two new RPCs are similar to ``GET /p2p/gossipsub/topics/`` but instead of
  grouping peers by topic they group them by slot indices or attester's public key
  hashes (``pkhs``) appearing in the relevant topics. (MR :gl:`!14504`)

- In the output of ``GET /p2p/peers/info``, the field ``"point"`` has been renamed
  to ``"peer"``. (MR :gl:`!14521`)

- A new RPC ``GET /health`` has been added to check the status on the node (MR :gl:`!14670`).

- An optional ``slot_index`` numerical query argument has been added to
  RPC ``POST /slots``. When provided, the DAL node checks that its
  profile allows to publish data on the given slot index (MR :gl:`!14825`).

Miscellaneous
-------------

- Now depends on OCaml 4.14.2 (was 4.14.1 before). (MR :gl:`!14536`)

- Current Debian packages are now available via APT repository for Ubuntu
  (Noble and Jammy) and Debian Bookworm, both for AMD64 and ARM64.

- New set of Debian packages are now available for testing. These new set of
  packages are built following Debian best practices for packaging, use debconf
  for configuration and systemd to handle the runtime lifecycle of the daemons.
  These new packages are going to introduce few breaking changes starting from
  the next release. Please check our documentation for more details. (MR
  :gl:`!13273`)

Version 20.3
============

Node
----

- Introduced ``--storage-maintenance-delay`` to allow delaying the
  storage maintenance. It is set to ``auto`` by default, to
  automatically trigger the maintenance whenever it is the most
  suitable. (MR :gl:`!14503`)

- **Breaking change** Bumped the node’s storage version to
  ``3.2``. This new version changes the store’s representation,
  allowing the storage to scale to the increasing number of blocks per
  cycles, thus paving the way to reducing even more the block
  time. Upgrading to this new version must be done manually (using the
  ``octez-node upgrade storage`` command) and is irreversible. (MR
  :gl:`!14211`)

- **Breaking change** Bumped the snapshot version from ``7`` to ``8``,
  in order to support the changes introduced by the ``3.2`` storage
  version. Snapshots of version ``7`` exported with previous versions
  of Octez can still be imported. Snapshots of version ``8``
  are not retro-compatible with previous octez versions (MR
  :gl:`!14398`).

DAL node
--------

- Fixed a memory leak in the DAL node.

- Fix ``--public-addr`` when the specified port was different
  from the default one. (MR :gl:`!11732`)

Version 20.2
============

Baker
-----

- Fixes a corner-case implementation bug. In an unlikely but plausible scenario, the baker binary did not behave as expected by the Tenderbake algorithm, and the Tezos protocol. This bugfix reinforces Tenderbake invariants when injecting consensus operations (MR :gl:`!14134`)

Version 20.1
============

General
-------

- Added binaries for ParisC. (MR :gl:`!13747`)

Smart Rollup node
-----------------

- New command ``repair commitments`` which allows the rollup node to recompute
  correct commitments for a protocol upgrade which did not. (MR :gl:`!13615`)

Version 20.0
============

Node
----

- Reduced the maximum allowed timestamp drift to 1 seconds. It is recommended to
  use NTP to sync the clock of the node. (MR :gl:`!13198`)

- Removed ``ghostnet.kaml.fr`` from ghostnet bootstrap peers. (MR :gl:`!13435`)

Smart Rollup node
-----------------

- Added support for custom, and user defined, PVM patches for rollup genesis
  (to be used on private rollups). (MRs :gl:`!12907`, :gl:`!12957`, :gl:`!12983`)

Version 20.0~rc1
================

General
-------

- Removed binaries for Nairobi. (MR :gl:`!12043`)

Node
----

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
  if a delegate is forbidden after being denounced for misbehaving. This RPC
  will become available when protocol P is activated. (MR :gl:`!12341`)

- Introduced a new ``/health/ready`` RPC endpoint that aims to return
  whether or not the node is fully initialized and ready to answer to
  RPC requests. (MR :gl:`!6820`)

- Removed the deprecated ``local-listen-addrs`` configuration file
  field. Use ``listen-addrs`` instead. (MR :gl:`!12489`)

 - Augmented the ``--max-active-rpc-connections <NUM>`` argument to contain
   an ``unlimited`` option to remove the threshold of RPC connections.
   (MR :gl:`!12324`)

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

- **Breaking change** Removed read-write commands specific to Nairobi. (MR :gl:`!12058`)

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

- Made the baker sign attestations as soon as preattestations were
  forged without waiting for the consensus pre-quorum. However, the
  baker will still wait for the pre-quorum to inject them as specified
  by the Tenderbake consensus algorithm. (MR :gl:`!12353`)

- Fixed situations where the baker would stall when a signing request
  hanged. (MR :gl:`!12353`)

- Introduced two new nonces files (``<chain_id>_stateful_nonces`` and
  ``<chain_id>_orphaned_nonces``). Each nonce is registered with a state
  for optimising the nonce lookup, reducing the number of rpc calls
  required to calculate nonce revelations. (MR :gl:`!12517`)

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

Miscellaneous
-------------

- **Breaking change** Switch encoding of ``nread_total`` field of
  ``P2p_events.read_fd`` in Octez-p2p library to ``Data_encoding.int64`` to fix an
  overflow.

- Versions now include information about the product. (MR :gl:`!12366`)

- **Breaking change** Multiple occurrence of same argument now
  fails when using ``lib-clic``. (MR :gl:`!12780`)

Version 19.2
============

Node
----

- Fixed a cemented block store encoding causing an overflow for cycles above 1Gib.

Smart Rollup node
-----------------

- Fixed an issue with the way the rollup node computes dissections in the refutation games. (MR :gl:`!12534`)

Version 19.1
============

Node
----

- Added ``--max-active-rpc-connections <NUM>`` that limits the number
  of active RPC connections per server to the provided argument. The
  default limit is set to 100.

- Enforced the proposed default ACL list.

Smart Rollup node
-----------------

- Fixed a critical bug that could lead to data loss when chain
  reorganizations happen while a GC is running. (MR :gl:`!11358`)

- Fixed issue with constants fetching during protocol migration. (MR :gl:`!11804`)

Version 19.0
============

Miscellaneous
-------------

- References to ``teztnets.xyz`` have been changed to ``teztnets.com``.

Version 19.0~rc1
================

Node
----

- **Breaking change** Removed the deprecated ``endorsing_rights`` RPC,
  use ``attestation_rights`` instead. (MR :gl:`!9849`)

- Added metrics about messages sent, broadcasted, or received by the shell's DDB.

- **Breaking change** Removed the deprecated
  ``disable-mempool-precheck`` configuration flag and
  ``disable_precheck`` field of ``prevalidator`` in the shell limits
  of the configuration file. They already had no effect on the node
  anymore. (MR :gl:`!10030`)

- Log at ``Info``` level the reasons behind disconnections in the p2p section.

- Removed a spurious "missing validation plugin" warning message that
  was emitted every time a block was applied using an old protocol
  whose its plugin was removed.

- **Breaking change** Removed the deprecated ``/monitor/valid_blocks``
  RPC. Instead, use the ``/monitor/applied_blocks`` RPC that has the
  same behaviour.

Client
------

- Fixed indentation of the stacks outputted by the ``normalize stack``
  command. (MR :gl:`!9944`)

- Added options to temporarily extend the context with other contracts
  and extra big maps in Michelson commands. (MR :gl:`!9946`)

- Added a ``run_instruction`` RPC in the plugin and a ``run michelson code``
  client command allowing to run a single Michelson instruction or a
  sequence of Michelson instructions on a given stack. (MR :gl:`!9935`)

- The legacy unary macros for the ``DIP`` and ``DUP`` Michelson
  instructions have been deprecated. Using them now displays a warning
  message on stderr.

Baker
-----

- Made the baker attest as soon as the pre-attestation quorum is
  reached instead of waiting for the chain's head to be fully
  applied (MR :gl:`!10554`)

Docker Images
-------------

- The rollup node is protocol agnostic and released as part of the Docker
  image. (MR :gl:`!10086`)


Smart Rollup node
-----------------

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

Smart Rollup client
-------------------

- **Breaking change** Smart Rollup client have been deprecated and
  no longer exist, most commands have equivalents RPCs and ``octez-codec`` (MR :gl:`!11046`).

- The following table outlines the deprecated commands of the Smart Rollup client and
  their corresponding replacements with new RPCs:

  .. code-block:: rst

    ==========================================  ====================================================
    Command                                     RPC
    ==========================================  ====================================================
    get smart rollup address                    [GET global/smart_rollup_address]
    ------------------------------------------  ----------------------------------------------------
    get state value for <key> [-B --block       [GET global/block/<block>/state]
    <block>]
    ------------------------------------------  ----------------------------------------------------
    get proof for message <index> of outbox     [GET /global/block/<block-id>/helpers/proofs/outbox/
    at level <level> transferring               <outbox_level>/messages] with message index in query
    <transactions>
    ------------------------------------------  ----------------------------------------------------
    get proof for message <index> of outbox     [GET /global/block/<block-id>/helpers/proofs/outbox/
    at level <level>                            <outbox_level>/messages] with message index in query
    ==========================================  ====================================================


Smart Rollup WASM Debugger
--------------------------

- Added flag ``--no-kernel-debug`` to deactivate kernel debug messages. (MR
  :gl:`!9813`)

- Support special directives using ``write_debug`` host function in the
  profiler, prefixed with ``__wasm_debugger__::``. Support
  ``start_section(<data>)`` and ``end_section(<data>)`` to count ticks in

- Partially support the installer configuration of the Smart Rollup's SDK, i.e.
  support only the instruction ``Set``. The configuration can be passed to
  the debugger via the option ``--installer-config`` and will initialize the
  storage with this configuration. (MR :gl:`!9641`)

- The argument ``--kernel`` accepts hexadecimal files (suffixed by ``.hex``), it
  is consired as an hexadecimal ``.wasm`` file. (MR :gl:`!11094`)

Miscellaneous
-------------

- Beta scripts to build Debian and RedHat packages have been added to the tree.

- Recommended Rust version bumped to 1.71.1 from 1.64.0.

- Extended the Micheline lexer to allow primitives starting with the
  underscore symbol (``_``). (MR :gl:`!10782`)

- Beta Debian and Redhat packages are now linked in gitlab releases.

- Renamed package registries for releases from ``tezos-x.y`` to ``octez-x.y``.

Version 18.1
============

Node
----

- **Breaking change** Bumped the snapshot version from ``6`` to ``7``,
  in order to address an issue which resulted in the export of corrupted tar rolling and full
  snapshots. Octez v18.1 nodes can still import previous version ``6`` (and earlier) snapshots.
  but snapshots in version 7 are not retro-compatible with previous
  octez versions (MR :gl:`!10785`).

Version 18.0
============

Node
----

- **Breaking change** Bumped Octez store version from ``5`` to
  ``6`` to explicit the incompatibility with previous store (and hence Octez) versions. As a result snapshots exported from a v6 store are not compatible with earlier Octez versions. Also, improved the consistency of ``snapshot`` import errors
  messages (MR :gl:`!10138`)

Smart Rollup node
-----------------

- Fixed an issue where the rollup node could forget to update its Layer 2 head for a
  block. (MR :gl:`!9868`)

- Remove the batcher simulation. This simulation was generic and could
  not catch problematic transaction. Batcher configuration now has a
  one less field ``simulate``. (MR :gl:`!10842`)

Version 18.0-rc1
================

Node
----

- Changed the bounding specification of valid operations in the mempool:

  + Before, the number of valid **manager operations** in the mempool
    was at most ``max_prechecked_manager_operations`` (default 5_000),
    with no other constraints. (Operations to keep were selected
    according to a "weight" that consists in the ratio of fee over
    "resources"; the latter is the maximum between the following
    ratios: operation gas over maximal allowed gas, and operation size
    over maximal allowed size. The baker uses the same notion of
    "weight" to select operations.)

  + Now, the number of valid **operations of any kind** is at most
    ``max_operations`` (default 10_000), and also the **sum of the
    sizes in bytes** of all valid operations is at most
    ``max_total_bytes`` (default 10_000_000). See
    [src/lib_shell/prevalidator_bounding.mli] for the reasoning behind
    the default values. (Operations are selected according to the
    protocol's ``compare_operations`` function, which currently orders
    operations according to their validation pass (consensus is
    highest and manager is lowest); note that two manager operations
    are ordered using their fee over gas ratio.)

  The values of ``max_operations`` and ``max_total_bytes`` can be
  retrieved with ``GET /chains/<chain>/mempool/filter`` and configured
  with ``POST /chains/<chain>/mempool/filter`` (just as
  ``max_prechecked_manager_operations`` used to be). As a result, the
  JSON format of the outputs of these two RPCs and the input of the
  second one have slightly changed; see their updated descriptions.
  (MR :gl:`!6787`)

- Errors ``prefilter.fees_too_low_for_mempool`` and
  ``plugin.removed_fees_too_low_for_mempool`` have been replaced with
  ``node.mempool.rejected_by_full_mempool`` and
  ``node.mempool.removed_from_full_mempool`` with different
  descriptions and messages. The ``rejected_by_full_mempool`` error
  still indicates the minimal fee that the operation would need to be
  accepted by the full mempool, provided that such a fee exists. If
  not, the error now states that the operation cannot be included no
  matter its fee (e.g. if it is a non-manager operation). (MRs
  :gl:`!6787`, :gl:`!8640`)

- Updated the message of the mempool's
  ``prevalidation.operation_conflict`` error. It now provides the
  minimal fee that the operation would need to replace the
  pre-existing conflicting operation, when such a fee exists. (This
  fee indication used to be available before v16, where it had
  been removed for technical reasons.) (MR :gl:`!9016`)

- RPC ``/helpers/forge/operations`` can now take JSON formatted operations with
  ``attestation``, ``preattestation``, ``double_attestation_evidence`` and
  ``double_preattestation_evidence`` kinds. Note that the existing kinds
  ``endorsement``, ``preendorsement``, ``double_endorsement_evidence``, and
  ``double_preendorsement_evidence`` are still accepted. (MR :gl:`!8746`)

- Simplified the peer to peer messages at head switch. The node now
  systematically broadcasts only its new head (instead of sometime
  broadcasting a sparse history of the chain).

- Added version ``1`` to RPC ``POST ../helpers/parse/operations``. It can be
  used by calling the RPC with the parameter ``?version=1`` (default version is
  still ``0``). Version ``1`` allows the RPC to output ``attestation``,
  ``preattestation``, ``double_attestation_evidence`` and
  ``double_preattestation_evidence`` kinds in the JSON result. (MR :gl:`!8840`)

- Added version ``2`` to RPC ``GET ../mempool/pending_operations``. It can be
  used by calling the RPC with the parameter ``?version=2`` (default version is
  still ``1``). Version ``2`` allows the RPC to output ``attestation``,
  ``preattestation``, ``double_attestation_evidence`` and
  ``double_preattestation_evidence`` kinds in the JSON result. This version
  also renames the ``applied`` field of the result to ``validated``
  (MRs :gl:`!8960`, :gl:`!9143`)

- RPCs ``/helpers/scripts/run_operation`` and
  ``/helpers/scripts/simulate_operation`` can now take JSON formatted operations
  with ``double_attestation_evidence`` and ``double_preattestation_evidence``
  kinds. Even though consensus operations are not supported by the RPCs,
  ``attestation`` and ``preattestation`` are accepted in the input JSON. (MR
  :gl:`!8768`)

- Removed ``lwt-log`` from the dependencies. The default logger has been updated
  to use the ``file-descriptor-stdout`` sink instead of the previous ``lwt-log``
  sink. This change has resulted in the removal of certain features from the log
  implementation that were specific to "lwt-log". Some features, such as log
  rules, syslog, and the output format, have been replaced with alternative
  implementations. Additionally, the previous implementation of "syslog" had
  some issues, including duplicated log headers or cropped messages, depending
  on the file output. These issues have been addressed, and the new
  implementation should now work correctly.

- Removed ``template`` field from ``log`` configuration with the removal of
  ``lwt-log`` library. Since it was believed to have low usage, no alternative
  implementation has been provided.

- The configuration flag ``disable-mempool-precheck`` is now
  deprecated, as well as the ``disable_precheck`` field of
  ``prevalidator`` in the shell limits of the configuration file. They
  already didn't do anything since v16. (MR :gl:`!8963`)

- Added version ``1`` to RPCs ``POST ../helpers/scripts/run_operation`` and
  ``POST ../helpers/scripts/simulate_operation``. It can be used by calling the
  RPC with the parameter ``?version=1`` (default version is still ``0``).
  Version ``1`` allows the RPC to output ``attestation``, ``preattestation``,
  ``double_attestation_evidence`` and ``double_preattestation_evidence`` kinds
  in the JSON result. (MR :gl:`!8949`)

- The error message when the local injection of an operation fails now
  begins with ``Error while validating injected operation`` instead of
  ``Error while applying operation``. (MR :gl:`!8857`)

- Updated the description of the ``ban_operation`` RPC to better
  reflect its behavior, which is unchanged. (More precisely, removed
  the "reverting its effect if it was applied" part since operations
  are never applied.) (MR :gl:`!8857`)

- Added version ``1`` to RPC ``GET ../mempool/monitor_operations``. It can be
  used by calling the RPC with the parameter ``?version=1`` (default version is
  still ``0``). Version ``1`` allows the RPC to output ``attestation``,
  ``preattestation``, ``double_attestation_evidence`` and
  ``double_preattestation_evidence`` kinds in the JSON result. (MR :gl:`!8980`)

- Improved the performances of JSON RPC calls by optimizing the
  serialization to JSON. (MR :gl:`!9072`)

- Fixed the ``validation_pass`` argument usage of ``monitor_operations`` RPC.
  Only operation that were in the mempool before the RPC call were filtered by
  validation passes. (MR :gl:`!9012`)

- **Breaking change** Removed the ``octez_mempool_pending_applied``
  metric, and renamed the ``octez_mempool_pending_prechecked`` one to
  ``octez_mempool_pending_validated``. (MR :gl:`!9137`)

- Added version ``1`` to RPC ``POST ../helpers/preapply/operations``. It can be
  used by calling the RPC with the parameter ``?version=1`` (default version is
  still ``0``). Version ``1`` allows the RPC to output ``attestation``,
  ``preattestation``, ``double_attestation_evidence`` and
  ``double_preattestation_evidence`` kinds in the JSON result. (MR :gl:`!8891`)

- Changed default stdout logs by adding simple coloration. The log header
  header is now bold and warning and errors are highlighted. The
  ``--log-coloring`` command line argument can be used to enable or
  disable logs coloration on default stdout logs; it is enabled by
  default. (MR :gl:`!8685`)

- Improved the performance of block validation: the block validation time has
  been reduced by half on average, resulting in a reduced propagation time
  through the network. (MR :gl:`!9100`)

- Added ``validated`` argument for ``GET ../mempool/monitor_operations`` and
  ``GET ../mempool/pending_operations``. ``applied`` argument of these RPCs is
  deprecated. (MR :gl:`!9143`)

- Added version ``1`` to RPCs ``GET ../blocks/<block>``, and ``GET
  ../blocks/<blocks>/operations``. It can be used by calling the RPC with the
  parameter ``?version=1`` (default version is still ``0``). Version ``1``
  allows the RPC to output ``attestation``, ``preattestation``,
  ``double_attestation_evidence`` and ``double_preattestation_evidence`` kinds
  in the JSON result. (MR :gl:`!9008`)

- When an operation in the mempool gets replaced with a better
  conflicting operation (e.g. an operation from the same manager with
  higher fees), the replaced operation is now reclassified as
  ``branch_delayed`` instead of ``outdated``. The associated error
  ``prevalidation.operation_replacement`` is otherwise unchanged. This
  makes it consistent with the reverse situation: when the new
  operation is worse than the old conflicting one, the new operation
  is classified as ``branch_delayed`` with the
  ``prevalidation.operation_conflict`` error. (MR :gl:`!9314`)

- In RPC ``/protocol_data``, ``"per_block_votes"`` replaces ``"liquidity_baking_toggle_vote"``;
  ``"per_block_votes"`` has two properties ``"liquidity_baking_vote"`` and ``"adaptive_issuance_vote"``.
  A vote is one of ``"on"``, ``"off"``, ``"pass"``.

- Added version ``1`` to RPC ``GET ../blocks/<blocks>/metadata``. It can be used
  by calling the RPC with the parameter ``?version=1`` (default version is still
  ``0``). Version ``1`` of this RPC and ``GET ../blocks/<block>`` allow the RPC
  to output ``attesting rewards`` and ``lost attesting rewards`` kinds in the
  JSON result. (MR :gl:`!9253`)

- Fixed a behavior where each time a new data was received from a
  peer, a new p2p request would be triggered instead of waiting for
  the delayed retry. (MR :gl:`!9470`)

- Renamed RPC server events: Added section ``rpc_server`` and changed
  names from ``legacy_logging_event-rpc_http_event-<level>`` into
  ``rpc_http_event_<level>``.

- Reduced the workload of the mempool by preventing unnecessary worker
  requests to be made and fixed a data-race that would request a
  resource that was already received. (MR :gl:`!9520`)

- Event ``block.validation.protocol_filter_not_found`` renamed to
  ``block.validation.validation_plugin_not_found`` with updated
  message ``no validation plugin found for protocol
  <protocol_hash>``. (MR :gl:`!9583`)

- Added RPC to get smart rollup's balance of ticket with specified ticketer, content type, and content:
  ``POST chains/<chain>/blocks/<block>/context/smart_rollups/smart_rollup/<smart_rollup_address>/ticket_balance``
  (MR :gl:`!9535`)

- **Breaking change** Removed ``mumbainet`` network alias. (MR :gl:`!9694`)

- Removed Mumbai mempool plugin. (MR :gl:`!9696`)

Client
------

- Added client commands to generate, open and verify a time-lock.

- The ``typecheck script`` command can now be used to typecheck several scripts.

- From protocol ``Oxford`` operation receipts output ``attestation`` instead of
  ``endorsement``. For example ``double preendorsement evidence`` becomes
  ``double preattesation evidence``, ``lost endorsing rewards`` becomes ``lost
  attesting rewards``. (MR :gl:`!9232`)

- Add ``attest for`` and ``preattest for`` commands. ``endorse for`` and
  ``preendorse for`` are now deprecated. (MR :gl:`!9494`)

- **Breaking change** Removed read-write commands specific to Mumbai (MR :gl:`!9695`)

- Added new client commands related to the new staking mechanisms:
  ``stake``, ``unstake``, ``finalize unstake``, ``set delegate parameters``,
  ``get full balance`` and ``get staked balance``. (MR :gl:`!9642`)

- Fixed a concurrency issue in the logging infrastructure
  which can cause the node to become temporarily unresponsive. (MR :gl:`!9527`)

Baker
-----

- Changed the baker liquidity baking vote file
  ``per_block_votes.json`` lookup so that it also considers its client
  data directory when searching an existing file. The previous
  semantics, which looks for this file in the current working
  directory, takes precedence.

- Bakers are now asked (but not required) to set their votes for the adoption of the
  adaptive issuance feature. They may use the CLI option ``--adaptive-issuance-vote``
  or the per-block votes file (which is re-read at each block, and overrides the CLI option).
  Absence of vote is equivalent to voting "pass".

- **Breaking change** Renamed ``liquidity_baking_toggle_vote`` into
  ``read_liquidity_baking_toggle_vote`` (MR :gl:`!9464`)
  and ``reading_per_block`` into ``reading_per_block_votes`` (MR :gl:`!8661`),
  for baker events.

- **Breaking change** Renamed ``endorsement`` into ``attestation`` for baker errors and events.
  (MR :gl:`!9195`)

- Cached costly RPC calls made when checking if nonces need to be
  revealed. (MR :gl:`!9601`)

Accuser
-------

- **Breaking change** Renamed ``endorsement`` into ``attestation`` for accuser errors and events.
  (MR :gl:`!9196`)

Proxy Server
------------

- Redirected not found replies (HTTP 404 answers) to the underlying
  octez-node itself. Public visibility of the node is not required
  anymore.

Protocol Compiler And Environment
---------------------------------

- Added a new version of the protocol environment (V10)

  - Exposed a limited API to manipulate an Irmin binary tree within the
    protocol.

  - Exposed encoding with legacy attestation name. (MR :gl:`!8620`)

Docker Images
-------------

-  Bumped up base image to ``alpine:3.17``. In particular, this changes Rust
   version to 1.64.0.

Smart Rollup node
-----------------

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

Smart Rollup WASM Debugger
--------------------------

- Changed the syntax for the ``octez-smart-rollup-wasm-debugger`` to prefix the
  the kernel file by ``--kernel``. (MR :gl:`!9318`)

- ``profile`` commands now profiles the time spent in each steps of a PVM
  execution. It can be disabled with the option ``--without-time`` (MR
  :gl:`!9335`).

- Added option ``--no-reboot`` to the ``profile`` command to profile a single
  ``kernel_run``.

- Improved profiling output for consecutive kernel runs.

- Allow serialized messages in inputs: ``{ "serialized": "01..." }``, instead
  of only external and internal transfers. This allows to inject arbitrary
  messages in the rollup. (MR :gl:`!9613`)

Data Availability Committee (DAC)
---------------------------------

- Released experimental Data Availability Committee executables which include ``octez-dac-node``
  and ``octez-dac-client``. Users can thus experiment
  with operating and using DAC in their Smart Rollup workflow to achieve higher data
  throughput. It is not recommended to use DAC on Mainnet but instead on testnets
  and lower environments.

Miscellaneous
-------------

- Updated and re-enabled the time-lock Michelson commands.

- New Recommended Rust version 1.64.0 instead of 1.60.0.

- Sapling parameters files are now installed by ``make build-deps`` via Opam

- Removed binaries of Mumbai (MR :gl:`!9693`)

Version 17.3
============

- Operations posting invalid WASM proofs are now discarded earlier by the
  Nairobi mempool plugin. (MR :gl:`!9768`)

Version 17.2
============

Node
----

- Removed the warning ``no prevalidator filter found for protocol
  ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im`` that was
  emitted at node start-up, because it is normal for ``Genesis`` not
  to have a prevalidator filter. The warning will still be issued if
  no prevalidator filter is found later on for a different
  protocol. (MR :gl:`!9261`)

- Renamed ``no prevalidator filter found for protocol {protocol_hash}``
  to ``no protocol filter found for protocol {protocol_hash}``.

- Added a syntactic operation filter in the protocol's plugins.

Version 17.1
============

Node
----

- Improved performances of RPC responses on request for older blocks by
  caching the archived metadata accesses. (MR :gl:`!8976`)

Miscellaneous
-------------

- Prevent cohttp-lwt.5.1.0 incompatibility with Resto,
  which causes leakage of file descriptors for streamed RPCs. (MR :gl:`!9059`)

Version 17.0
============

Node
----

- Fixed a bug where the node could freeze when an old block was
  requested during a store merge. (MR :gl:`!8952`)

Version 17.0-rc1
================

Node
----

- **Breaking Change**: Shortened a few lib_shell log messages at the default level by
   displaying only the completion time instead of the full status of the operation.

- Added an option ``daily-logs`` to file-descriptor sinks, enabling
  log rotation based on a daily frequency.

- Fixed a bug manifested while reconstructing the storage after a snapshot import
  that would result in wrong context hash mapping for some blocks.

- Added an option ``create-dirs`` to file-descriptor sinks to allow
  the node to create the log directory and its parents if they don't exist.

- Added a default node configuration that enables disk logs as a
  file-descriptor-sink in the data directory of the node with a 7 days rotation.

- **Breaking Change**: Removed section in stdout logs lines

- Removed the ``indexing-strategy`` option from the ``TEZOS_CONTEXT``
  environment variable to prevent the usage of the ``always``
  indexing strategy. For now, only the ``minimal`` indexing strategy
  is allowed.

- **Breaking Change** Removed the ``--network limanet``
  built-in network alias.

- Fixed a issue that may trigger unknown keys errors while reading the
  context on a read-only instance.

- Added query parameter ``protocol`` to RPC ``/monitor/heads/<chain_id>`` (which can be
  repeated) in order to monitor new heads of one or several given protocols only.

- **Breaking Change** Reworked some node logs. While bootstrapping,
  the node will log one message every 50 validated blocks to indicate
  the current head's level and how old it is, giving an indication on
  how long it will take to be synchronized. Also, gracefully indicates
  peer disconnection instead of spurious "worker crashed" messages.

- Fixed an issue where a node lagging behind would end up freezing and
  never be able to catch up.

Client
------

- **Breaking Changes**: an alias must be provided when originating a
  smart rollup. That alias can be used in other smart rollup commands
  instead of the address. This is similar to what is done for smart
  contracts.

  Smart rollup origination command::

    ./octez-client originate smart rollup <alias> from <source contract> of kind <smart rollup kind> of type <michelson type> with kernel <kernel>

  Other example command::

    ./octez-client recover bond of <implicit contrat> for smart rollup <alias or address> from <source contract>

- Similarly to smart contract, the client now has functions to manage the set of known smart rollups::

    ./octez-client remember smart rollup <alias> <smart rollup address>

    ./octez-client forget smart rollup <alias>

    ./octez-client forget all smart rollups

    ./octez-client show known smart rollup <alias>

    ./octez-client list smart rollups

- For the protocols that support it, added an ``operation_with_attestation`` and
  ``operation_with_attestation.unsigned`` registered encodings that support
  ``attestation`` kind instead of ``endorsement``. (MR :gl:`!8563`)

Baker
-----

- Consensus operations that do not use the minimal slot of the delegate are
  not counted for the (pre)quorums. (MR :gl:`!8175`)

- Consensus operations with identical consensus-related contents but different
  ``branch`` fields are counted only once in the (pre)quorums. (MR :gl:`!8175`)

- Improved efficiency to solve the anti-spam Proof-of-Work challenge.

- Made the baker capable of running in a RPC-only mode through a newly
  introduced command: ``octez-baker-<protocol> run remotely
  [delegates] [options]``. This mode does not require the octez node's
  storage to be directly accessible by the baker and will reduce
  memory consumption. However, this mode is less performant and may
  result in noticable slower baking times. (MR :gl:`!8607`)

- Added a default configuration for that enables disk logs as a
  file-descriptor-sink in the base directory with a 7 days rotation.

Accuser
-------

- Fixed a bug that made the accuser start without waiting for its
  corresponding protocol.

- The accuser now denounces double consensus operations that have the same
  level, round, and delegate, but different slots. (MR :gl:`!8084`)

Signer
------

- Reordered the display of ``octez-client list connected ledgers``
  command. It is now ``Ed25519``, ``Secp256k1``, ``Secp256r1`` and
  ``Bip32_ed25519``.

Smart Rollup node
-----------------

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

Smart Rollup client
-------------------

- Fixed a JSON decoding error for the command ``get proof for message ...`` (MR
  :gl:`!8000`).

Smart Rollup WASM Debugger
--------------------------

- Let the user select the initial version of the WASM PVM (MR :gl:`!8078`).

- Added commands to decode the contents of the memory and the durable storage
  (MRs :gl:`!7464`, :gl:`!7709`, :gl:`!8303`).

- Added the ``bench`` command (MR :gl:`!7551`).

- Added commands to inspect the structure of the durable storage (MRs
  :gl:`!7707`, :gl:`!8304`).

- Automatically ``load inputs`` when ``step inbox`` is called. (MR :gl:`!8444`)

- Added a command ``show function symbols`` to inspect the custom section
  ``name`` of unstripped kernels. (MR :gl:`!8522`)

- Added a command ``profile`` that runs a full ``kernel_run`` and produces a
  flamegraph of the execution (MR :gl:`!8510`).

Miscellaneous
-------------

- Removed binaries and mempool RPCs of Lima.

Version 16.1
============

Baker
-----

- Fixed a bug where the baker could count a (pre)endorsement twice
  while waiting for a (pre)quorum.

- Fixed an implementation bug where upon observing a pre-quorum on an
  earlier round, the baker would reach a state where it could not
  endorse anymore. This could lead to a slow consensus scenario
  affecting network liveness.

Version 16.0
============

Node
----

- Updated the built-in network alias for Mumbainet (``--network mumbainet``).
  The new alias matches the relaunch of Mumbainet with the protocol ``PtMumbai2``.


Version 16.0-rc3
================

General
-------

- Fixed an issue that prevented building on MacOS.

- Fixed an issue that caused the launch of Octez binaries to take several seconds (because of ``zcash`` initialization)

Node
----

- Fixed a issue that might trigger unknown keys errors while reading the
  context on a read-only instance.

- Added an RPC ``POST
  /chains/main/blocks/head/context/smart_rollups/all/origination_proof``
  with input ``{"kind":"<smart rollup kind>", "kernel"="<smart rollup
  kernel>"}`` to produce the origination proof needed to originate a
  smart rollup.

- Deprecated the RPC ``GET /monitor/valid_blocks`` and introduced
  ``GET /monitor/validated_blocks`` and ``GET /monitor/applied_blocks``
  respectively returning validated blocks which are not yet applied
  nor stored, and applied blocks which are fully applied and stored by
  the node. (MR :gl:`!7513`)

- Replaced some "precheck" occurrences with "validate" in event and
  error identifiers and messages. (MR :gl:`!7513`)

- Document the usage of "Yay" as a deprecated synonym for "Yea", to encourage
  using the correct value in the future. (MR :gl:`!7960`)

Baker
-----

- Changed the baker default semantics so that it performs a light
  validation of operations to classify them instead of fully applying
  them. Hence, the block production is now more
  time/cpu/disk-efficient. In this mode, application-dependent checks
  are disabled. Setting the ``--force-apply`` flag on the command line
  restores the previous behavior. (MR :gl:`!7490`)

- **Breaking Change**: Disabled the verification of signature of
  operations in the baker when baking a block. The baker must always
  be provided operations with a valid signature, otherwise produced
  blocks will be invalid and rejected by local nodes during their
  injection. Default setups are not affected but external mempools
  should make sure that their operations' signatures are correct.
  (MR :gl:`!7490`)

- Made the baker discard legacy or corrupted Tenderbake's saved
  states in order to avoid unexpected crashes when the baker gets
  updated, or when a new protocol's baker starts. (MR :gl:`!7640`)

- Restored previous behaviour from :gl:`!7490` for blocks at round
  greater than 0. Application-dependent checks are re-enabled for
  re-proposal and fresh blocks at round greater than 0.

- Reduced the preendorsement injection delay by making the baker
  preendorse as soon as the node considers a block as valid instead of
  waiting for the node to fully apply it. (MR :gl:`!7516`)

- Baker injects preendorsement twice: once the block is considered as
  valid by the node and once it is fully applied by the node. This is
  a safety measure in case the early preendorsement is dropped by the
  mempool. (MR :gl:`!7516`)


Version 16.0-rc2
================

Node
----

- Fixed a bug raising an error when a context split was called on a
  context that was created with Octez v13 (or earlier).

- **Breaking Change**: disabled snapshot export support for storage
  that was created with Octez v13 (or earlier).

  After upgrading to v16, if you have the following warning message, you won't be able to restore an up-to-date storage, without using either a recent third-party snapshot or bootstrapping from scratch::

    Warning: garbage collection is not fully enabled on this data directory: context cannot be garbage collected

  Please refer to the :doc:`Snapshots entry<../user/snapshots>` for further detail.

- Added the built-in network alias for Mumbainet (``--network mumbainet``).

Docker Images
-------------

- Fixed ``entrypoint.sh`` which did not had the executable permission flag.

- Updated Python version to 3.10.10.


Version 16.0~rc1
================

General
-------

- **Breaking change**: Symbolic links from old-names ``tezos-*`` to new-names ``octez-*``
  have been removed.
  Old names are not supported anymore.

Node
----

- Fixed a bug that caused snapshot import to ignore the data directory
  of the configuration file when the configuration file was specified
  from the command-line using ``--config-file``. Note that ``--data-dir``
  can still be used to override the data directory location from the
  configuration file, whether it is specified from the command-line or not.

- Fixed a bug that caused the ``snapshot import`` command to fail when
  used on data directories configured with an explicit number of
  additional cycles.

- Fixed an issue that could left a temporary directory if a snapshot
  export was cancelled. Additionally, a cleanup now ensures the
  absence of leftovers temporary directories when exporting a
  snapshot.

- Fixed an issue that could left a lock file if a snapshot import was
  cancelled.

- **Breaking change**: the default ``?version`` of the ``pending_operations``
  RPC is now 1 instead of 0. Version 1 is more consistent as
  ``branch_delayed``/``branch_refused``/``outdated``/``refused`` operations are
  encoded like ``applied`` operations: the ``"hash"`` field is included in the
  object instead of being separate in an array. The same change applies to
  ``unprocessed`` operations, except that those do not contain the ``error``
  field. More details can be found by calling the
  ``describe/chains/main/mempool/pending_operations`` RPC. You can get back the
  previous encoding with ``?version=0`` but note that version 0 is now
  deprecated and may be removed starting from the next major release of Octez.
  (MR :gl:`!6783`)

- The ``pending_operations`` RPC can now be run in ``binary`` format when using
  version ``1``. (MR :gl:`!6783`)

- Removed the ``node_`` prefix in identifiers of the
  ``config_validation`` and ``config_file`` events and errors.

- Introduced a ``--json`` command line argument to the ``snapshot
  info`` allowing to print snapshot information as JSON.

- Removed the ``octez-validator`` executable, which was already part
  of ``octez-node`` and that was already used internally (and that was
  not usable on its own).

- **Breaking change**: bumped the node's storage version to
  ``3.0``. This new version changes the store's representation
  required by the new protocol's semantics and the context's format to
  improve the disk usage footprint while running a context
  pruning. Upgrading to this new version is automatic and
  irreversible. (MR :gl:`!6835` and :gl:`!6959`)

- **Breaking change**: bumped the snapshot version to ``5``. This
  version changes internal snapshot file representation to include
  more information required by the new protocol's semantics and to
  improve both import and export speed. Snapshots of version ``4``
  exported with previous versions of Octez can still be
  imported. Snapshots of version ``5`` are not backward compatible.
  (MR :gl:`!6835` and :gl:`!6961`)

- Upon receiving a new non-manager operation that conflicts with a
  previously validated operation, the mempool may now replace the old
  operation with the new one, depending on both operations' content
  and hash. This behavior was already in place for manager operations,
  and has simply be extended to non-manager operations. It should help
  all mempools converge toward the same set of accepted operations,
  regardless of the order in which the operations were
  received. (MR :gl:`!6749`)

- Changed the id and message of the error when the mempool rejects a
  new operation because it already contains a preferred conflicting
  operation. Changed the id and message of the error associated with
  an operation that is removed from the mempool to make room for a
  better conflicting operation. (MR :gl:`!6749`)

- Fixed a minor bug that caused the mempool to accept a manager
  operation that conflicts with an already present ``drain_delegate``
  operation. (MR :gl:`!6749`)

- Removed the compatibility with storage snapshots of version ``2``
  and ``3``. These snapshot versions from Octez 12 cannot be imported
  anymore.

- Added optional query parameter ``validation_pass`` to RPCs ``GET
  /chains/main/mempool/pending_operations`` and ``GET
  /chains/<chain_id>/mempool/monitor_operation``. This new parameter causes the
  RPC to only return operations for the given validation pass (``0`` for
  consensus operations, ``1`` for voting operations, ``2`` for anonymous
  operations, ``3`` for manager operations). If ``validation_pass`` is
  unspecified, operations for all validation passes are returned, making this
  extension backward-compatible. (MR :gl:`!6724`)

- Fixed an issue where the node's RPC server would silently fail when
  either the path to the certificate or to the key passed in the
  node's ``--rpc-tls`` argument does not point to an existing
  file. The node's ``run`` now fails immediately in this case. (MR
  :gl:`!7323`)

- Improved the disk usage footprint when running a context pruning.

- **Breaking Changes:** Removed ``kathmandunet`` from the list of
  known networks (for ``--network`` command-line argument).

- Allowed symbolic links in the datadir (to split data over several places).

Client
------

- Added command to get contract's balance of ticket with specified ticketer, content type, and content. Can be used for both implicit and originated contracts.
  ``octez-client get ticket balance for <contract> with ticketer '<ticketer>' and type <type> and content <content>``. (MR :gl:`!6491`)

- Added command to get the complete list of tickets owned by a given contract by scanning the contract's storage. Can only be used for originated contracts.
  ``octez-client get all ticket balances for <contract>``. (MR :gl:`!6804`)

Baker
-----

- **Breaking change**: modified the baker's persistent state. Once the
  protocol "M" activates, the new baker will automatically overwrite
  the existing persistent state to the new format. This implies that
  previous bakers will fail to load this new state from disk unless
  the user directly removes the file
  ``<client-dir>/<chain_id>_baker_state``. On mainnet, this will have
  no effect as when the new protocol activates, previous bakers will
  be permanently idle. (MR :gl:`!6835`)

- Fixed an issue where the baker would keep files opened longer than
  necessary causing unexpected out of space errors making the baker
  crash.

Signer
------

- **Breaking change**: Versioning of signature module for protocol
  specific support and future extensibility. Signatures length became
  variable which changed their binary encoding. This breaks the
  compatibility with octez-signer <= 15.1 in local and socket modes.

Proxy Server
------------

- The proxy server can now serve endpoints about blocks of all known economic
  protocols instead of only one chosen at boot time.

Codec
-----

- Added the ``dump encoding <id>`` command to dump the description of a single
  registered encoding.


Rollups
-------

- Added Smart Rollups executables.
  This includes ``octez-smart-rollup-node-PtMumbai``, ``octez-smart-rollup-client-PtMumbai``.

- Released ``octez-smart-rollup-wasm-debugger`` as part of the Octez distribution (MR :gl:`!7295`).
  See the smart rollups documentation for its functionalities and how to use it to test and debug kernels.

Miscellaneous
-------------

- Removed binaries and mempool RPCs of Kathmandu.

Version 15.1
============

Node
----

- Fixed a bug that caused the bootstrap pipeline to apply a block without
  prechecking it first. This issue only occurs for recent protocols (i.e., Lima
  and later) where the validation of a block is dissociated from its
  application. (MR :gl:`!7014`)

Version 15.0
============

General
-------

- Fixed the warning that was added in 15.0~rc1 about using deprecated
  ``tezos-`` names. This warning gave the wrong new name for executables
  that contained the protocol number.

Node
----

- Fixed the restoration of the protocol table when restoring from an
  inconsistent data directory.

- Improved the response time of RPCs computing the baking and endorsing
  rights of delegates.

- Added the built-in network alias for Limanet (``--network limanet``).

- Fixed a bug that caused the ``snapshot import`` command to fail when
  used on data directories configured with an explicit number
  additional cycles.

- Improved cleanup of leftover files when starting a node.

Client
------

- Fixed a regression in 15.0~rc1 that caused ``make build-deps`` to not
  install the ``ledgerwallet-tezos`` opam package by default, which in turn
  caused the client to be built without Ledger commands. Docker images
  and static executables were not affected.

Baker
-----

- Fixed a file permission issue when running in Docker.

Version 15.0~rc1
================

General
-------

- **Breaking change**: all executables have been renamed.
  The ``tezos-`` prefix has been replaced by ``octez-`` and protocol numbers
  have been removed. For instance, ``tezos-node`` is now named ``octez-node``
  and ``tezos-baker-014-PtKathma`` is now named ``octez-baker-PtKathma``.
  If you compile using ``make``, symbolic links from the old names to the
  new names are created, so you can still use the old names.
  But those old names are deprecated and may stop being supported
  starting from version 16.0.

- **Breaking change**: in the Docker entrypoint, all commands have been renamed.
  The ``tezos-`` prefix has been replaced by ``octez-``.
  For instance, ``tezos-node`` is now named ``octez-node`` and ``tezos-baker``
  is now named ``octez-baker``. The old command names are still available but
  are deprecated and may stop being supported starting from version 16.0.

- Added :doc:`Lima <protocols/015_lima>`, a protocol proposal for
  Mainnet featuring, among others, Pipelining, Consensus Key,
  improvements to Tickets, and Ghostnet fixes.

Node
----

- **Breaking change**: Decreased, from 5 to 1, the default number of
  additional cycles to keep in both ``Full`` and ``Rolling`` history
  modes. As a consequence, the storage footprint will be lowered and
  only the last 6 cycles will be available (10 previously).

- **Breaking change**: The node context storage format was
  upgraded. To this end, a new storage version was introduced: 2.0
  (previously 1.0). Backward compatibility is preserved: upgrading
  from 1.0 to 2.0 is done automatically by the node the first time you
  run it. This upgrade is instantaneous. However, be careful that
  there is no forward compatibility: previous versions of Octez will
  refuse to run on a data directory which was running with this
  storage version.

- **Breaking change**: Node events using a legacy logging system
  are migrated to the current one. Impacted events are in the following
  sections: ``validator.chain``, ``validator.peer``, ``prevalidator``
  and ``validator.block``. Section ``node.chain_validator`` is merged
  into ``validator.chain`` for consistency reasons. Those events see
  their JSON representation shortened, with no duplicated
  information. e.g.  ``validator.peer`` events were named
  ``validator.peer.v0`` at top-level and had an ``event`` field with a
  ``name`` field containing the actual event name, for example
  ``validating_new_branch``. Now, the event is called
  ``validating_new_branch.v0`` at top-level and contains a ``section``
  field with ``validator`` and ``peer``.

- Added context pruning for the context part of the storage
  backend. It is activated by default for all nodes running with a
  full or rolling history mode.

- Add a ``/chains/<chain>/blocks/<block>/merkle_tree_v2`` RPC. This is an
  evolution of the ``../merkle_tree`` RPC, using a simpler implementation of the
  Merkle tree/proof features that works with Irmin trees and proofs underneath
  instead of proof code internal to Octez, and is planned to eventually replace
  the old one in a future release.

- Add a field ``dal`` in the node's configuration file. This field is
  for a feature which is being developed and should not be
  modified. It should be used only for testing.

- Fixed a bug in the p2p layer that prevented a fast regulation of the
  number of connections (when having too few or too many connections)

- Improved the octez store merging mechanism performed on each new
  cycle. The node's memory consumption should not differ from a normal
  usage, while, in the past, it could take up to several gigabytes of
  memory to perform a store merge. It also takes less time to perform
  a merge and shouldn't impact normal node operations as much as it
  previously did; especially on light architectures.

- Added support for ``level..level`` range parameters in the replay command.

-  Added support for ``--strict`` mode in the replay command: it causes the
   command to be less permissive.

- The ``config`` and ``identity`` node commands no longer try to
  update the data directory version (``version.json``).

- Fixed a bug in the store that was generating an incorrect protocol
  table during a branch switch containing a user activated protocol
  upgrade.

- Added Oxhead Alpha endpoints in the list of bootstrap peers for
  Mainnet.

- Removed the ``--network hangzhounet`` and ``--network jakartanet``
  built-in network aliases.

Client
------

- The light client (``tezos-client --mode light``) now uses the
  ``../block/<block_id>/merkle_tree_v2`` RPC introduced in this version, removing
  a lot of delicate verification code and relying on Irmin instead. The client
  for this version will thus not work with older node versions that do not have
  this RPC.

- Simulation returns correct errors on batches of operations where some are
  backtracked, failed and/or skipped.

- External operations pool specified by the ``--operations-pool`` option are
  guaranteed to be included in the order they are received from the operations
  source.

- Added commands to get the used and paid storage spaces of contracts:
  ``tezos-client get used storage space for <contract>`` and
  ``tezos-client get paid storage space for <contract>``.

- Added RPCs to get the used and paid storage spaces of contracts:
  ``GET /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>/storage/used_space``
  and ``GET /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>/storage/paid_space``.

- Added commands related to the "consensus key" feature:

	Update the consensus key of a baker:

```shell
tezos-client set consensus key for <mgr> to <key>
```

  It is also possible to register as a delegate and immediately set the consensus key:

```shell
tezos-client register key <mgr> as delegate with consensus key <key>
```

  (The current registration command still works.)


  Drain a baker's account:

```shell
tezos-client drain delegate <mgr> to <key>
```

  or, if the destination account is different from the consensus key

```shell
tezos-client drain delegate <mgr> to <dest_key> with <consensus_key>
```


Baker
-----

- External operations pool specified by the ``--operations-pool`` option are
  guaranteed to be included in the order they are received from the operations
  source.

- The logs now display both the delegate and its consensus key.

Signer
------

- Improved performance by 50% of Ledger's signing requests by caching
  redundant requests.

Proxy Server
------------

- Replaced not found replies (HTTP 404 answers) by redirects (HTTP 301
  answers) suggesting to query directly the node on all unserved
  requests.

Docker Images
-------------

-  Bump up base image to ``alpine:3.16``. In particular, it changes Rust
   and Python versions to 1.60.0 and 3.10.9 respectively.

Miscellaneous
-------------

-  Recommend rust version 1.60.0 instead of 1.52.1.

-  Removed delegates for protocols Ithaca and Jakarta.

Version 14.1
============

- Fixed a number of issues with JSON encodings.

- Removed Giganode from the list of bootstrap peers for Mainnet.

- Add third user-activated upgrade to the ``--network ghostnet`` built-in
  network alias (at level 1191936 for Kathmandu).

Version 14.0
============

Node
----

- Added the built-in network alias for Kathmandunet (``--network kathmandunet``).

Client
------

- Disabled origination of contracts with timelock instructions.

Version 14.0~rc1
================

Node
----

- Added Kathmandu, a protocol proposal for Mainnet featuring, among others,
  pipelining of manager operations, improved randomness generation, event
  logging and support for permanent testnets.

- Fixed a bug that lead to forgetting the trusted status of peers when connection
  is lost.

- Added store metrics to expose the amount of data written while
  storing the last block and the completion time of the last merge.

- Added a block validator metric to expose the number of operations per
  pass for each new block validated.

- Added protocol metrics: ``head_cycle``, ``head_consumed_gas`` and ``head_round``.

- Added a store metric to expose the number of blocks considered as invalid.

- Fixed the ``octez-node config reset`` command which did not actually reset
  the configuration file to its default values.

- Added metrics to observe the bootstrapped and synchronisation status.

- Added metrics to track peer validator requests.

- Added an optional query parameter ``metadata`` to the
  ``GET /chains/<chain>/blocks/<block>/`` and
  ``GET /chains/<chain>/blocks/<block>/operations/`` RPCs. Passing this
  parameter with value ``always`` overrides the metadata size limit
  configuration, and forces the re-computation of operation metadata
  whose size was beyond the limit, and therefore not stored. The
  re-computed metadata are not stored on disk after this call, but
  rather just returned by the RPC call. Passing this parameters with
  value ``never`` prevents the request to return metadata, to allow
  lighter requests. If the query string is not used, the configured
  metadata size limit policy is used.

- Deprecated the ``force_metadata`` query parameter for the
  ``GET /chains/<chain>/blocks/<block>/`` and
  ``GET /chains/<chain>/blocks/<block>/operations/`` RPCs. To get a similar
  behaviour, use the ``metadata`` query string with the value
  ``always``.

- Deprecated the CLI argument ``--enable-testchain`` and the corresponding
  configuration-file option ``p2p.enable_testchain``.

- Added metrics to track the pending requests of chain validator, block
  validator and prevalidator workers.

- **Breaking change**: The node context storage format was
  upgraded. To this end, a new storage version was introduced: 1.0
  (previously 0.8). Backward compatibility is preserved: upgrading
  from 0.6, 0.7 (Octez 12.x) or 0.8 (Octez 13.0) is done through the
  ``octez-node upgrade storage`` command. This upgrade is
  instantaneous. However, be careful that there is no forward
  compatibility: previous versions of Octez will refuse to run on an
  upgraded data directory.

- **Breaking change**: the built-in network alias for Ithacanet
  (``--network ithacanet``) has been removed.

- Added the built-in network alias for Ghostnet (``--network ghostnet``).

- Updated the encoding of worker events JSON messages.

- Fixed a bug that caused the ``replay`` command to write into the context store.

Client
------

- Client now allows to simulate failing operations with ``--simulation
  --force``, and report errors without specifying limits.

- Added ``--ignore-case`` option to the ``octez-client gen vanity keys`` command
  to allow case-insensitive search for the given pattern.

Proxy Server
------------

- Changed the proxy server's handling of requests it does not know how to serve:
  it now forwards the client to the full node at the given ``--endpoint``, by
  responding with a ``301 Moved Permanently`` redirect.

Protocol Compiler And Environment
---------------------------------

- Added protocol environment V6.

Docker Images
-------------

- **Breaking change**: script ``tezos-docker-manager.sh``, also known as
  ``alphanet.sh`` or ``mainnet.sh``, has been removed. It was deprecated
  since version 13.0. It is recommended to write your own docker-compose file instead.
  ``scripts/docker/docker-compose-generic.yml`` is an example of such file.

- ``octez-codec`` is now included in Docker images.

Rollups
-------

- Included the Transaction Rollups (TORU) and Smart Contract Rollups
  (SCORU) executables in the Docker images of Octez.  These executables are
  **experimental**.  They are provided solely for testing,
  and should not be used in production.  Besides, they should not be
  considered as being part of Octez, and as a consequence will not be
  provided with the same degree of maintenance.  However, developers
  interested in implementing their own rollup nodes and clients are
  more than welcome to leverage them.

Version 13.0
============

Node
----

- Fixed a bug that caused metrics to return wrong values for the
  number of accepted points.

- Added the ``jakartanet`` built-in network alias.
  You can now configure your node with ``--network jakartanet`` to run the
  Jakartanet test network.

- Fixed a bug in the environment that could prevent checking BLS signatures.
  This bug could affect transactional optimistic rollups (TORUs, introduced in Jakarta).

Miscellaneous
-------------

- Fixed a bug that caused static executables to report the wrong
  version number with ``--version``.

Version 13.0~rc1
================

Node
----

- Added Jakarta, a protocol proposal for Mainnet featuring, among others,
  Transaction Optimistic Rollups, Tickets Hardening and Liquidity Baking Toggle.

- **Breaking change**:
  Restored the encoding of events corresponding to "completed
  requests" (block validation, head switch, ...) to their format prior to Octez v11.
  They only contain absolute timestamps.

- Add optional query parameters ``applied``, ``refused``, ``outdated``,
  ``branch_refused``, and ``branch_delayed`` to RPC
  ``GET /chains/main/mempool/pending_operations``.
  These new parameters filter the operations returned based on their
  classifications. If no option is given, all
  the parameters are assumed to be ``true``, making this extension
  backward-compatible (i.e. all operations are returned).

- Added optional parameter ``--media-type`` and its corresponding field
  in the configuration file. It defines which format of data serialisation
  must be used for RPC requests to the node. The value can be ``json``,
  ``binary`` or ``any``. By default, the value is set to ``any``.

- Added an option ``--metrics-addr <ADDR>:<PORT>`` to ``octez-node`` to
  expose some metrics using the Prometheus format.

- Added command ``octez-node storage head-commmit`` which prints the commit hash
  of the current context head.

- Added a history mode check when importing a snapshot to ensure the consistency between the
  history mode of the snapshot and the one stored in the targeted data
  directory configuration file.

- Fixed a wrong behavior that could cause the savepoint to be dragged
  too early.

- Fixed a memory leak where some operations were not cleaned up. This problem
  occurred occasionally, when during the fetching the operation of some block,
  the node changed his head.

- **Breaking change**:
  The node context storage format was upgraded. To this end, a new storage
  version was introduced: 0.0.8 (previously 0.0.7). Backward compatibility is
  preserved: upgrading from 0.0.7 to
  0.0.8 is done automatically by the node the first time you run it. This
  upgrade is instantaneous. However, be careful that there is no forward
  compatibility: previous versions of Octez
  will refuse to run on a data directory which was used with Octez 13.0.

- **Breaking change**:
  Validation errors are flatter. Instead of ``economic_protocol_error``
  carrying a field ``trace`` with the relevant economic-protocol errors, the
  relevant economic-protocol errors are included in the main trace itself.

- **Breaking change**:
  Exported snapshots now have version number 4 (previously 3).
  Snapshots of version 2 and 3 exported with previous versions of
  Octez can still be imported. Snapshots of version 4 cannot be
  imported with Octez prior to version 13.0.

- Added an optional query parameter ``force_metadata`` to the
  ``GET /chains/<chain>/blocks/<block>/`` and
  ``GET /chains/<chain>/blocks/<block>/operations/`` RPCs. Passing this
  parameter overrides the metadata size limit configuration, and forces
  the re-computation of operation metadata whose size was beyond the
  limit, and therefore not stored. The re-computed metadata are not
  stored on disk after this call, but rather just returned by the RPC call.

- Added ``--progress-display-mode`` option to the ``octez-node`` commands
  that display progress animation. This option allows to redirect progress
  animation to non-TTY file descriptors.

Client
------

- The client no longer computes the description of RPCs by default.
  This makes the client run faster at the cost of possibly getting
  less informative error messages. You can switch back to the previous
  behavior using new command-line option ``--better-errors``.

- A new ``--self-address`` option was added to the ``run script``
  command. It makes the given address be considered the address of
  the contract being run. The address must actually exist in the
  context. Unless ``--balance`` is specified, the script also
  inherits the given contract's balance.

- Storage and input parameters given to the ``run script`` command
  can now be read from a file just like the script itself.
  The ``file:`` prefix can be added for disambiguation, like for a script.

- Add option ``--force`` to the command ``submit ballots``. This is
  mostly for testing purposes: it disables all checks and allows to
  cast invalid ballots (unexpected voting period, missing voting
  rights, ...)

.. _changes_13_0_rc1_baker:

Baker
-----

The following breaking changes affect the Octez v13.0~rc1 baker daemon
for the Jakarta 2 protocol ``octez-baker-013-PtJakart``, but **not** the
corresponding one for the the Ithaca 2 protocol,
``octez-baker-012-Psithaca``.

- **Breaking change**:
  The ``--liquidity-baking-escape-vote`` command-line option has been renamed
  to ``--liquidity-baking-toggle-vote``.

- **Breaking change**:
  The ``--liquidity-baking-toggle-vote`` command-line option is now
  mandatory. The ``--votefile`` option can still be used to change
  vote without restarting the baker daemon, if both options are
  provided ``--votefile`` takes precedence and
  ``--liquidity-baking-toggle-vote`` is only used to define the
  default behavior of the daemon when an error occurs while reading
  the vote file. Note that ``--liquidity-baking-toggle-vote`` must be placed
  **after** ``run`` on the command-line.

- **Breaking change**:
  The format of the vote file provided by the ``--votefile`` option
  has changed too; the ``liquidity_baking_escape_vote`` key is now named
  ``liquidity_baking_toggle_vote`` and its value must now be one of
  the following strings: ``"on"`` to vote to continue Liquidity
  Baking, ``"off"`` to vote to stop it, or ``"pass"`` to abstain.

- Fixed a memory leak in ``baker`` binary (Ithaca2, Jakarta and Alpha)

- Fixed a memory leak in ``accuser`` binary (Ithaca2, Jakarta and Alpha)

- Fixed the RPC ``/chains/<chain>/mempool/monitor_operations`` which
  would not notify outdated operations when the query
  ``outdated=true`` was provided.

Signer
------

- Added global option ``--password-filename`` which acts as the client
  one.

- **Breaking change**:
  Option ``--password-file``, which did nothing, has been removed.

- Added support for Ledger Nano S Plus devices.

Proxy server
------------

- A new ``--data-dir`` option was added. It expects the path of the
  data-dir of the node from which to obtain data. This option greatly
  reduces the number of RPCs between the proxy server and the node, thus
  reducing the IO consumption of the node.

Codec
-----

- Added command ``slice`` which splits a binary, hex-encoded blob into its
  different constituents. This command is useful to understand what a binary message means.

Docker Images
-------------

- **Breaking change**:
  Script ``tezos_docker_manager.sh`` (also known as ``mainnet.sh``) is now deprecated.
  It may be removed from Octez starting from version 14.0.
  It is recommended to write your own Docker Compose files instead.
  To this end, you can take inspiration from ``scripts/docker/docker-compose-generic.yml``.

- ``tezos_docker_manager.sh`` no longer starts the endorser.
  As a reminder, starting from Ithaca, which is the active protocol on Mainnet,
  there is no endorser: its role is played by the baker.

- ``tezos_docker_manager.sh`` no longer supports Hangzhounet.

Miscellaneous
-------------

- Removed protocol ``genesis-carthagenet``.
  No live test network uses this protocol anymore.

- Removed delegates for protocol Hangzhou, since it was replaced by Ithaca
  as the active protocol on Mainnet.

Version 12.4
============

- Fixed a memory leak in the baker and the accuser.
  This is a backport of the fix introduced in version 13.0~rc1.

Version 12.3
============

- Fixed a bug that prevented the store from
  decoding metadata from previous versions of Octez.
  This bug caused the store to systematically have to restore
  its consistency on startup.

- **Breaking change**:
  Exported snapshots now have version number 3 (previously 2).
  Snapshots exported by nodes running previous versions of Octez can still be
  imported by a v12.3 node, but snapshots exported by a v12.3 node cannot
  be imported by nodes running previous versions.

  Please note that snapshots exported with versions 12.1 and 12.2
  of Octez cannot be imported with previous versions of Octez either,
  but their version number was not increased, leading to less clear
  error messages when trying to import them from previous versions.
  It is thus recommended to avoid exporting snapshots with versions
  12.1 or 12.2 of Octez.

- Increased the maximum size of requests to sign a block header with a
  Ledger in order to take into account Tenderbake block headers which
  are reproposals of a block at an higher round.
  Combined with an incoming update of the Ledger baking app,
  this fixes a case where the Ledger failed to sign blocks.

Version 12.2
============

- Added ``--metadata-size-limit`` option to the node to configure the
  operation metadata size limit. This defaults to 10MB but can be
  overridden by providing another value (representing a number of bytes)
  or the value ``unlimited``.

Version 12.1
============

Node
----

- Added optional argument ``cycle`` to RPC ``selected_snapshot``.
  See more information in the changelog of the protocol:
  :doc:`../protocols/012_ithaca`

- **Breaking change**: The node no longer stores large metadata.
  RPC requesting this kind of metadata will return ``"too large"``.
  To this end, a new storage version was introduced: 0.0.7 (previously
  0.0.6). Upgrading from 0.0.6 to 0.0.7 is done automatically by the
  node the first time you run it. This upgrade is
  instantaneous. However, be careful that previous versions of Octez
  will refuse to run on a data directory which was used with Octez
  12.1 or later.

- A new ``--force`` option was added to the ``transfer`` command. It
  makes the client inject the transaction in a node even if the
  simulation of the transaction fails.

- Fixed a corner case where the mempool would propagate invalid operations.

Baker
-----

- Fixed an incorrect behavior that could make the baker crash under
  certain circumstances.

Version 12.0
============

Node
----

- The octez-node configuration file parameter
  ``shell.prevalidator.limits.max_refused_operations`` is now
  deprecated and may be removed starting from version 13.0.

- Fixed missing removal of replaced operation in the plugin when another better
  one takes its place (when the mempool is full).

- The output of ``octez-client get ledger high watermark for <ledger>``
  now also displays the high-water mark for the round, if available.
  Rounds are introduced in Tenderbake.

- Optimized global CPU usage. This can save up to a third of CPU usage.

- RPC ``/helpers/scripts/simulate_operations`` now takes protocol
  activation into account: the cache is considered empty three levels
  before activation.

- Added an optional field to the RPC
  ``/helpers/scripts/simulate_operations`` named
  ``blocks_before_activation`` (int32, measured in number of blocks)
  which allows to override the number of blocks before activation,
  which can be useful in case of user-activated upgrade.

Miscellaneous
-------------

- Updated the build system to use a patched version of the OCaml compiler
  to fix a bug that could cause compilation to fail on newer versions of gcc and glibc.

- Optimized memory consumption of the block validator.
  This improves memory usage of the node, the external validator process,
  and the baker. Memory usage should be lower and more predictable.

Baker / Endorser / Accuser
--------------------------

- Improved the log messages of the Ithaca baker for the default
  (``Notice``) and ``Info`` logging levels.

- Fixed a corner case where the baker could include redundant endorsements
  when a delegate was double baking.

Version 12.0~rc2
================

- Replaced protocol Ithaca (``PsiThaCa``) with protocol Ithaca2 (``Psithaca2``).

Node
----

- (backport from 11.1) Fixed an incorrect behaviour of the store which
  could cause the node to freeze for a few seconds.

- The ``ithacanet`` network alias now denotes the configuration for
  the Ithacanet test network that uses Ithaca2 (``Psithaca2``)
  instead of the initial Ithacanet test network that used Ithaca (``PsiThaCa``).

- The RPC GET ``/chains/main/mempool/pending_operations`` does not
  output unparsable operations anymore. Previously, they were in the
  ``Refused`` field with a parsing error.

- The output format for RPC ``/chains/<chain_id>/mempool/filter`` changed.
  The field ``backlog`` was removed. This change is similar to other RPC changes
  introduced in 12.0~rc1.

- Added two optional fields, ``now`` and ``level`` as input to the
  ``run_view``, ``run_code``, and ``trace_code`` RPCs (under
  ``/chains/<chain_id>/blocks/<block>/helpers/scripts/``). These
  fields can be used to override the values normally returned by the
  ``NOW`` and ``LEVEL`` instructions.

- Pending operations in the mempool are now sorted, and propagated with the following
  priority in decreasing order (operations with the highest priority are
  propagated first):

  - consensus operations;

  - anonymous and voting (governance) operations;

  - manager operations where the priority is given by the ratio of the operation
    fees over gas limit or operation size.

- Fixed an issue where storage failed to restore its consistency after
  corrupted metadata files.

- Added an optional field, ``max_prechecked_manager_operations`` to
  ``/chains/<chain_id>/mempool/filter`` in order to control how many manager
  operations are kept in the prechecked classification.

Client
------

- Renamed the ``--mempool`` option into ``--operations-pool``.
  The format of the file passed as parameter has changed from the one of RPC
  ``pending_operations`` (that is, a key-value dictionary whose values are lists
  of operations) to a single list of operations to be considered for inclusion.

- ``--operations-pool`` option supports URL parameters to fetch remote mempools
  through HTTP.  Environment variable ``TEZOS_CLIENT_REMOTE_OPERATIONS_POOL_HTTP_HEADERS``
  may be set to specify custom HTTP headers. Only the Host header is supported
  as of now (see description in `rfc2616, section 14.23
  <https://datatracker.ietf.org/doc/html/rfc2616#section-14.23>`_)

- Added new option ``--ignore-node-mempool`` to the ``bake for`` command
  to avoid querying the node's mempool when baking a block.

Baker / Endorser / Accuser
--------------------------

- Ported the ``--operations-pool`` option of the ``bake for`` command of the client
  to the baker daemon.

- Fixed the Ithaca baker to allow it to fallback to an RPC (instead of
  relying on direct access to the local context) when baking the
  migration block to its successor. This necessary mechanism was
  present in all bakers except for the Ithaca baker of Octez 12.0~rc1.

Version 12.0~rc1
================

Node
----

- UNIX errors are now displayed using human-friendly English instead of error codes.

- Manager operations do no longer need to be executed before being
  propagated over the network. This feature will be available from
  protocol ``I``, provided the latter is activated. The aim is to
  increase the throughput of transactions gossiped over the network,
  while reducing the load on the Octez node's prevalidator
  (aka the mempool).

- The following RPCs output format changed:

  1. ``/workers/block_validator``
  2. ``/workers/chain_validators``
  3. ``/workers/chain_validators/<chain_id>``
  4. ``/workers/chain_validator/<chain_id>/peer_validators``
  5. ``/workers/chain_validator/<chain_id>/peer_validators/<peer_id>``
  6. ``/workers/prevalidators``

  The field ``backlog`` is removed. Those logs can be obtained via the
  node itself. Logging can be redirected to a file via the option
  ``--log-file``. External tools such as ``logrotate`` can be used to
  remove entries that are too old.

- The node configuration format is changed. The
  following paths are removed:

  1. ``shell.chain_validator.limits.worker_backlog_size``
  2. ``shell.chain_validator.limits.worker_backlog_level``
  3. ``shell.peer_validator.limits.worker_backlog_size``
  4. ``shell.peer_validator.limits.worker_backlog_level``
  5. ``shell.prevalidator.limits.worker_backlog_size``
  6. ``shell.prevalidator.limits.worker_backlog_level``
  7. ``shell.block_validator.limits.worker_backlog_size``
  8. ``shell.block_validator.limits.worker_backlog_level``

  If those fields are present in your configuration file, they can
  simply be removed.

- Added version ``1`` to RPC ``GET /chains/main/mempool/pending_operations``.
  It can be used by calling the RPC with the parameter ``?version=1``
  (default version is still ``0``).

- Added an RPC ``/config/logging`` to reconfigure the logging framework
  without having to restart the node. See also the new documentation pages
  related to logging.

- Better handling of mempool cache in the ``distributed_db`` which
  should make the ``distributed_db`` RAM consumption strongly
  correlated to the one of the mempool.

- Fixed RPC GET ``/chains/<chain_id>/mempool/filter``, that did not
  show fields of the filter configuration that were equal to their
  default value: e.g. if the configuration was the default one, it
  just returned ``{}``. Now displays all the fields by default. The
  old behavior may be brought back by setting the new optional
  parameter ``include_default`` to ``false``.

- Changed the behavior of RPC POST ``/chains/<chain_id>/mempool/filter``
  when provided an input json that does not describe a valid filter
  configuration. It used to revert the filter back to the default
  configuration in that case, but now it leaves it unchanged. (Note:
  if the input json is valid but does not provide all the fields of
  the filter configuration, then any missing field is set back to its
  default value, rather than left unchanged. This is the same
  behavior as the previous version of the RPC.) As this behavior may
  be confusing, the RPC now returns the new filter configuration of
  the mempool.

- When encoded in binary, errors now have a single size field. This only
  affects the binary representation of errors or values that include errors
  inside. It may break the compatibility for tools that request binary-only
  answers from the node and parse the errors by hand.

- Added a new mempool's classification for the recently introduced
  outdated error category of protocols in environment v4.

- Add a new CLI & config option ``advertised-net-port``.

- Added an optional ``show_types`` field in the input of the
  ``/chains/<chain_id>/blocks/<block>/helpers/scripts/typecheck_code``
  RPC. When this field is set to ``false``, type checking details are
  omitted. This can be used to improve the performances of this RPC.

- Fix the comparison operator of history modes to avoid considering
  the default history modes as not equal to an history mode manually
  set to the same default value.

- The prevalidator (which handles operations which have been received but not
  yet included in a block) was made more restrictive: it now accepts a single
  manager operation from a given manager for a given block. This limitation
  was already present implicitly if you were using the ``octez-client`` commands.
  Batches of operations can be used to get around this restriction, see the
  ``multiple transfers`` command to learn more. In addition, operations
  rejected because of this limitation are solely delayed to a future block.

- Removed support for store versions 0.0.4 (used by Octez 9.7) or below.
  It is no longer possible to run ``octez-node upgrade storage`` to upgrade
  from those older versions. It is also no longer possible to import
  snapshots that were exported using this version.

- Fixed an inconsistency of the cache: the shell now reloads the cache
  from scratch if the application fails because of a hash
  inconsistency.

- Removed the ``granadanet`` built-in network alias.

- Added the ``ithacanet`` built-in network alias.

- Added an optional field, ``replace_by_fee_factor`` to
  ``/chains/<chain_id>/mempool/filter`` in order to control when the mempool
  accepts a manager operation replacement.

Client
------

- Expanded the number of product ids searched with the HID API when
  looking for a ledger device.

- Added an optional parameter ``--media-type`` to control the
  ``Accept`` header for RPC requests to the node. This header
  indicates to the node which format of data serialisation is
  supported. Possible values are ``json``, ``binary`` and ``any``.

- Added two options, ``--now`` and ``--level`` to the ``run script``
  and ``run view`` commands simulating execution of Michelson
  code. These options can be used to override the values normally
  returned by the ``NOW`` and ``LEVEL`` instructions.

- Added new option ``--replace`` to ``transfer`` and ``multiple transfers`` commands.
  This option allows a manager to inject a transfer or a smart contract call
  operation (with more fees) to replace an existing one in the node's mempool.
  This option should only be used to inject in nodes whose prevalidators use
  the new validation scheme of manager operations (called ``operations
  precheck``) instead of fully applying the operation in a prevalidation block.
  Note that there are no guarantees on which operation will possibly be
  included in a block. For instance, the second operation may arrive too late to
  the baker, in which case, the latter might includes the first operation and
  the second one becomes invalid.

Baker / Endorser / Accuser
--------------------------

- Added an optional parameter ``--media-type`` to control the
  ``Accept`` header for RPC requests to the node. This header
  indicates to the node which format of data serialisation is
  supported. Possible values are ``json``, ``binary`` and ``any``.

-  Removed baker, endorser and accuser for Granada.

Miscellaneous
-------------

-  Made the ``file-descriptor-{path,stdout,stderr}://`` event-logging
   sink more configurable (e.g. filtering per level and per section). The
   environment variable ``TEZOS_NODE_HOSTNAME`` used for the output of events
   was renamed to the more appropriate ``TEZOS_EVENT_HOSTNAME``.

-  Added specific documentation pages about logging for users and
   developers.

-  Some RPC entry points are stricter about their inputs. Specifically, some
   RPCs where only positive integers would make sense will now error when
   provided negative values (instead of, e.g., returning empty results).

-  Added diffing functionality to the Micheline library. It allows to compare
   Micheline expressions whose primitives are ``strings``. The difference is
   returned as another Micheline expression annotated appropriately in places
   where compared values differ.

Version 11.1
============

-  Octez can now be compiled using opam 2.1 instead of requiring opam 2.0.

-  ADX instructions have been disabled in Docker images and static binaries.
   This makes it possible to use them on older CPUs.

-  Fixed an incorrect behaviour of the store which could cause the node
   to freeze for a few seconds.

-  Reduced the memory consumption of the snapshot import.

Version 11.0
============

No changes compared to 11.0~rc2.

Version 11.0~rc2
================

-  Included fixes from version 10.3.

Node
----

-  Added protocol Hangzhou2 (``PtHangz2``), which is a modified version
   of Hangzhou (``PtHangzH``) with a number of critical bug fixes.

-  Added a user-activated protocol override from Hangzhou
   (``PtHangzH``) to Hangzhou2 (``PtHangz2``) on Mainnet. This
   means that nodes using version 11.0~rc2 will activate Hangzhou2
   instead of Hangzhou if Hangzhou was to be activated by the on-chain
   governance process.

-  As the Hangzhounet test network was restarted to use ``PtHangz2``
   instead of ``PtHangzH``, the ``hangzhounet`` network alias now
   contains the configuration to connect to this restarted
   Hangzhounet.

-  Bumped the network version to 2.

-  Added early block advertisement based on a precheck mechanism to
   improve the propagation time in the network. This mechanism is only
   available for nodes with a network version of 2.

-  The default allocation policy for the OCaml runtime is now ``2``
   (also called ``best-fit``). The previous value was ``0``. This new
   policy gives the best compromise in terms of performances and memory
   consumption. This policy can be changed using the ``OCAMLRUNPARAM``
   environment variable. For example, to set back this value to ``0``,
   one can do ``OCAMLRUNPARAM="a=0"``. More information on this
   environment variable can be found `here <https://ocaml.org/manual/runtime.html>`__.

-  Improved the performance of the ``raw/bytes`` RPC call.
   In particular, this prevents stack overflows that could happen
   because of the flattened context if Hangzhou2 is activated.

-  Improved the performance of the context flattening migration that
   will happen if Hangzhou2 is activated. In particular, this reduces
   how much memory is needed by this operation.

-  Fixed issue #1930: during decoding, the validity of Micheline
   annotations is enforced.

-  Improved the snapshot export mechanism by reducing both the export
   time and the memory footprint.

-  Added new RPCs to inspect the storage status:

   -  GET ``/chains/main/levels/checkpoint``: checkpoint block hash and
      level.
   -  GET ``/chains/main/levels/savepoint``: savepoint block hash and
      level.
   -  GET ``/chains/main/levels/caboose``: caboose block hash and
      level.
   -  GET ``/config/history_mode``: history mode of the node.

-  Deprecated the ``/chains/main/checkpoint`` RPC. It may be deleted
   starting from v12.0.

-  The field ``backlog`` of the following RPCs is deprecated and may be
   deleted starting from v12.0:

   - ``/workers/block_validator``

   - ``/workers/chain_validators``

   - ``/workers/chain_validators/<chain_id>``

   - ``/workers/chain_validator/<chain_id>/peer_validators``

   - ``/workers/chain_validator/<chain_id>/peer_validators/<peer_id>``

   - ``/workers/prevalidators``

-  The following paths of the node configuration format are deprecated
   and may be deleted starting from v12.0:

   - ``shell.chain_validator.limits.worker_backlog_size``

   - ``shell.chain_validator.limits.worker_backlog_level``

   - ``shell.peer_validator.limits.worker_backlog_size``

   - ``shell.peer_validator.limits.worker_backlog_level``

   - ``shell.prevalidator.limits.worker_backlog_size``

   - ``shell.prevalidator.limits.worker_backlog_level``

   - ``shell.block_validator.limits.worker_backlog_size``

   - ``shell.block_validator.limits.worker_backlog_level``

-  The ``octez-admin-client show current checkpoint`` command now only
   outputs the current checkpoint. It no longer outputs the savepoint,
   caboose and history mode.

-  When calling the
   ``/chains/<chain_id>/blocks/<block>/helpers/preapply`` RPC, the
   preapplication is now done by the external validator process
   instead of the main node process. This allows the external
   validator to cache the result. If later the block is applied, this
   cache is then used to optimize the application of the block.

-  Fixed an inconsistency of the cache internal counter between the
   baker and the node when the cache has been emptied.

Version 11.0~rc1
================

Node
----

-  **Breaking change**:
   updated the output of the ``/stats/gc`` RPC entry point: it now also
   reports the number of full major collections made by the OCaml
   garbage collector.

-  **Breaking change**:
   updated the encoding of chain validator events.
   The output of RPC ``GET /workers/chain_validators/<chain_id>``
   was modified as a result.

-  Updated RPC ``GET /workers/prevalidators``: field ``backlog`` now
   always returns an empty list. The events in this backlog can now be
   obtained either via stdout, or by configuring a new sink for events
   via the environment variable ``TEZOS_EVENTS_CONFIG`` (to be set
   before launching the node).

-  Updated RPC ``GET /chains/<chain_id>/mempool/monitor_operation``:
   output was extended to include operation hashes (field name is
   ``hash``) and errors (field name is ``error``) when the operation
   is classified as ``Branch_delayed``, ``Branch_refused`` or ``Refused``.

-  Improved how the distributed database (DDB) handles the mempool cache.
   This should make the DDB RAM consumption strongly correlated
   to the one of the mempool.

-  Fixed wrong error message in case of P2P network address binding collision.

-  Added new RPCs to ban/unban operations locally.

   -  POST ``/chains/<chain_id>/mempool/ban_operation``: ban a given
      operation hash. The operation is removed from the mempool, and
      its effect is reverted if it was applied. It is also added to
      the prevalidator's set of banned operations, to prevent it from
      being fetched/processed/injected in the future.

   -  POST ``/chains/<chain_id>/mempool/unban_operation``: unban a given
      operation hash, removing it from the prevalidator's set of banned
      operations. Nothing happens if the operation was not banned.

   -  POST ``/chains/<chain_id>/mempool/unban_all_operations``: unban
      all operations, i.e. clear the set of banned operations.

-  Added the possibility to use the ``~``, ``-`` and ``+`` operators
   when querying blocks by their level using the
   ``/chains/.../blocks/`` RPC. For instance,
   ``/chains/main/blocks/41+1`` requests the block at level 42. Before
   this change, these notations were only available with aliases (such
   as ``head-1``).

-  Added the possibility to use the ``+`` operator when specifying the
   block to export, using the ``--block`` argument of the snapshot
   export command. Before, only ``~`` and ``-`` were allowed.

-  Fixed a bug where the mempool forgot about ``refused`` operations
   on flush, leading to these operations being potentially reevaluated
   in the future (e.g. if they are advertised again by a peer).

-  Removed the built-in network aliases for Edonet and Florencenet,
   since Edo and Florence have been replaced by Granada.

-  Added a built-in network alias for Hangzhounet.

Client
------

-  Disabled indentation checking by default in the ``octez-client
   convert script`` and ``octez-client hash script`` commands. In
   particular, ``octez-client convert script <script> from Michelson
   to Michelson`` can now be used as a Michelson script formatter. To
   force the indentation check, the new ``--enforce-indentation``
   command line switch can be used.

-  Added admin commands ``ban operation <operation_hash>``,
   ``unban operation <operation_hash>``, and ``unban all operations``
   that call the corresponding RPCs.

-  Made mode light ``--endpoint`` / ``--sources`` consistency check
   happen earlier, so that it is guaranteed to catch mismatches.

-  Added commands ``list proxy protocols`` and ``list light protocols``,
   to get the list of protocols supported by ``--mode proxy`` and ``--mode light``

-  Fix gas simulation for operation batches for Granada, Hangzhou and Alpha

-  Added timestamp display of the snapshot's block target when running
   the ``octez-node snapshot info`` command.

Baker / Endorser / Accuser
--------------------------

-  Removed baker, endorser and accuser for Edo and Florence, since they
   have been replaced by Granada.

Protocol Compiler And Environment
---------------------------------

-  Added a new version of the protocol environment (V3).

   -  Updated some dependency libraries that have had releases since V2.

   -  Improved safety by removing access to some potentially dangerous functions
      (functions that make assumptions about their input, functions that rely on
      implicit comparison, etc.).

   -  Added new features: ``Timelock`` and ``FallbackArray``.

   -  Added new feature: RPC outputs can now be chunked.
      RPCs that use this feature in the protocol can now respond without blocking
      during the encoding of the output.

Docker Images
-------------

-  The entrypoint script now starts the node with ``--allow-all-rpc``.
   This means that ACLs are inactive in the Docker image on the default RPC port.
   Note that the Docker image does not expose this port by default.
   If you use ``tezos-docker-manager.sh``, it will expose this port only to
   other Octez containers.
   In summary, you can now call all RPCs if you use Docker images, without
   compromising security as long as you do not explicitly expose the RPC port.

Version 10.3
============

Node
----

-  Fixed wrong behaviour when updating the additional cycles of the
   node's history mode.

-  Removed redundant event while setting a new head.

-  Fixed wrong behaviour when merging the store after a rolling
   snapshot import.

-  Fixed an issue when reconstructing a storage with missing block or
   operations metadata hashes.

-  Fixed an issue in the store were the table in charge of maintaining
   the associations between a protocol and its activation block was not
   well updated.

-  Prevented some store files from being written only partially,
   which could result in store corruptions.

Docker Images
-------------

-  The ``--force-history-mode-switch`` option is now available for
   ``octez-node`` entrypoint. It allows the user to switch the history
   mode of the node's storage.

Version 10.2
============

- Fixed a critical issue in the chain storage layer.

Version 10.1
============

-  Really added the CLI option ``--allow-all-rpc`` to enable full
   access to all RPC endpoints on a given listening address.

-  Fixed recycling of operations in the mempool when the node changes
   its head. Broadcasting of endorsements received earlier than the
   end of the validation of the endorsed block is restored.

Version 10.0
============

-  Improved some error messages related to P2P initialization.

Version 10.0~rc3
================

Node
----

-  Included fixes from versions 9.6 and 9.7.

-  Fixed an issue in the store that prevented some blocks from being queried,
   resulting in "block not found" errors.

-  Store version is now 0.0.6.
   If you were previously using Octez 10.0~rc1 or 10.0~rc2, you were using
   store version 0.0.5. If you were previously using Octez 9.x, you were
   using store version 0.0.4. In both cases, use command
   ``octez-node upgrade storage`` to upgrade to 0.0.6.

-  Added an upgrade procedure to upgrade from ``v0.0.5`` to ``v0.0.6``. The
   procedure is implemented through the ``octez-node upgrade storage``
   command.

-  Added an ``integrity-check-index`` subcommand to ``octez-node
   storage``, which can be used to check for corruptions (missing
   entries) in the index of the store. This command also accepts an
   optional flag ``--auto-repair`` to fix those specific corruptions
   by adding back missing entries.

-  Fixed an RPC inconsistency where, after a migration occurred, the
   metadata from blocks returned by RPCs would return inconsistent
   data (blocks prior to a migration from a protocol A to B would
   return that their current protocol is A and next protocol is B
   instead of A and A).

Baker
-----

-  Improved error reporting for ill-formed liquidity-baking escape vote files.

Version 10.0~rc2
================

Node
----

-  Added a check to prevent protocol migrations that decrease the protocol
   environment version.

-  Old stores of nodes running Granadanet can now be upgraded to the new store format
   introduced in 10.0~rc1. Before, this was only possible for Mainnet, Edonet and
   Florencenet.

-  Empty stores can now be migrated to the new store format too.

-  Fixed a case where the context could become corrupted.

-  Fixed a memory leak in the cache of the mempool. This issue could
   also cause operations to not be propagated correctly in some cases.

Docker Images
-------------

-  Running the node with the ``--version`` flag now correctly returns the commit date.

Version 10.0~rc1
================

Node
----

-  **Breaking change**:
   Introduced Access Control Lists for RPC servers, which allow to restrict
   access to selected RPC endpoints for different listening addresses. The
   default Access Control List is quite restrictive. RPC endpoints that are
   considered unsafe will now be blocked by default for all requests coming from
   default Access Control List is quite restrictive. Requests from remote hosts
   to unsafe RPC endpoints are now blocked by default.
   Among other things, this breaks bakers and endorsers running
   remotely. For processes operating on the same host as the node, nothing
   changes. If necessary, the old behaviour can be restored by editing the
   node's configuration file, but it is discouraged due to security concerns
   of open unsafe endpoints on public networks. See Node Configuration section
   of the Tezos documentation for details.

-  Replaced the chain storage layer with a more efficient backend in
   terms of both performance and storage size.

-  Added an upgrade procedure to upgrade from the previous store to the
   new one. The procedure is implemented through the
   ``octez-node upgrade storage`` command. This command is
   non-destructive: the previous store is preserved at
   ``<data_dir>/lmdb_store_to_be_removed`` and needs to be manually
   removed when the user made sure the upgrade process went well.

-  Reworked the storage snapshots:

   -  Introduced a new snapshot format (v2)
   -  Improved the snapshot export/import process in both terms of
      duration and memory usage
   -  Added ``--export-format`` option:

      -  ``--export-format tar`` (default) creates a snapshot as a
         portable tar archive
      -  ``--export-format raw`` creates a snapshot as a raw directory
         suitable for IPFS sharing

   -  The argument ``[output_file]`` in
      ``octez-node export snapshot [output_file]`` becomes optional and
      defaults to a file whose name follows this pattern
      ``<NETWORK>-<BLOCK_HASH>-<BLOCK_LEVEL>.<SNAPSHOT_HISTORY_MODE>``
   -  Improved the metadata of snapshots which can be displayed using
      ``octez-node snapshot info``
   -  The ``octez-node snapshot import`` command is retro-compatible
      with the previous snapshot format (v1) but legacy snapshots cannot
      be exported anymore

-  Interrupted context reconstruction can now be resumed.

-  Promoted the ``experimental-rolling`` history mode to ``rolling``.
   The node’s option ``--history-mode experimental-rolling`` is now
   deprecated and is equivalent to ``--history-mode rolling``.

-  Reworked the nodes rolling and full history modes. Previously, these
   two modes were maintaining a window of ``<preserved cycles>`` cycles
   of metadata (``5`` on mainnet). These modes may now be configured to
   keep a larger window of metadata. E.g.
   ``octez-node run --history-mode full+2`` will maintain 2 extra cycles
   of metadata, in addition to the network’s preserved cycles. This may
   become useful for users that want to keep more data from the past:
   for instance, to compute rewards payouts. The default number of extra
   preserved cycles is 5 (``5 + 5`` on mainnet).

-  Updated the semantics of the history mode configuration parameter/option
   of the node in full and rolling modes. If the number of additional cycles
   is not explicitly specified, the default value is used. The default
   number of additional cycles to keep is set to 5.

-  Updated the RPC ``chains/main/checkpoint`` by renaming the
   ``save_point`` field into ``savepoint`` to be consistent to the
   ``v0.0.5`` store naming.

-  Improved the shutdown procedure for external validator process.

-  Added command ``replay`` which takes a list of block levels, hashes
   or aliases, revalidate those blocks in the context of their
   predecessor, and check that the result is the same as what is
   currently stored. This is mostly useful for debugging and
   benchmarking purposes.

-  Reduced the maximum allowed timestamp drift to 5 seconds.

-  The file descriptor sink, which can be used to output node events to
   a file using JSON format, now outputs events with an additional field
   ``"hostname"``. This field can be used to identify the node when
   aggregating events from multiple nodes. Its default value is the
   hostname of the device the node is running on, and it can be
   customized with environment variable ``TEZOS_NODE_HOSTNAME``.

-  Fixed a bug that caused the lack of connection in private network
   with ``--connections`` set to 1.

-  Fixed a potential interleaving of distinct events written to a file
   descriptor sink simultaneously.

-  You can now control the verbosity of the logs of the context
   storage backend using the ``TEZOS_CONTEXT`` environment
   variable. Set it to ``v`` to display log messages with level "info"
   or to ``vv`` to also display log messages with level "debug".

-  The ``TEZOS_STORAGE`` variable now has no effect. Use
   ``TEZOS_CONTEXT`` instead (see previous item).

-  Added an RPC to run `TZIP-4
   views <https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-4/tzip-4.md#view-entrypoints>`__
   offchain, accessible via ``../<block_id>/helpers/scripts/run_view``.

- Added a CLI option ``--allow-all-rpc`` to enable full access to all RPC
  endpoints on a given listening address.

Client
------

-  Changed to 5 the recommended number of blocks after which an
   operation can be considered final. Under normal network conditions
   and an attacker with less than 33% of stake, an operation can be
   considered final with quasi-certainty if there are at least 5 blocks
   built on top of it. See Emmy\* TZIP for more detailed explanations.

-  Added ``--mode light`` which makes the client execute some RPCs
   locally (to lower the load of nodes and to avoid having to trust
   the nodes). This mode is akin to light clients and SPV clients:
   it uses Merkle proofs to make the light mode super safe.

-  Added commands to display the hash of Michelson script from files
   (``octez-client hash script``) and from addresses (``octez-client
   get contract script hash``).

-  Added support for a new generic version of the multisig contract.

-  Added a new flag, ``--simulation``, which simulates operations instead of preapplying them.

-  ``hash data`` command now supports the optional ``--for-script [TSV|CSV]``.

-  Renamed ``--block`` option of ``sign message`` command to ``--branch``.

-  Commands using an encrypted key now fail after the user fails to give the correct
   password three times.

-  Added support for FA1.2 standard, allowing to interact with fungible
   assets contracts using the ``from fa1.2 contract ...`` commands, and
   support for running the view entrypoints offchain.


-  Added a ``--legacy`` flag to the ``convert script`` command. This flag permits to use the
   legacy typechecking mode when the input of the command is typechecked.

Baker / Endorser / Accuser
--------------------------

-  Optimized the performance of the baker to reduce the number of RPC
   calls to the node while waiting for endorsements.

Proxy server
------------

-  Added a new binary: ``octez-proxy-server``, a read-only frontend to a node.
   It is designed to lower the load of nodes, by being capable
   of serving :doc:`protocol RPCs <alpha/rpc>`.
   An instance of a proxy server is protocol-specific: it automatically picks
   up the protocol from the backing node when it starts. Proxy servers
   can be started and destroyed at will, making them easy to deploy.

Version 9.7
===========

-  The mempool plugin now avoids some costly operations on outdated
   consensus operations such as endorsements for old blocks.

-  The mempool now filters out old consensus operations to avoid
   reevaluating them again after flushing when the node receives a new
   head.

Version 9.6
===========

-  Increased the delay after which the endorser gives up on endorsing to
   1200 seconds (previously 110 seconds), to prevent an issue where
   blocks that arrived too late were not endorsed at all, causing the
   next block to also be produced late.

Version 9.5
===========

-  Fixed a bug that could result in a corrupted storage and in assert
   failure errors.

Version 9.4
===========

- Fixed an issue in the mempool that caused too many operations
  referring to unknown blocks to be kept, resulting in the node
  running out of memory.

Version 9.3
===========

-  Reintroduced the following RPCs in the Granada RPC plugin. These
   RPCs were already present in the Edo and Florence protocol plugin
   and are deprecated, they will be removed in the successor protocol
   of Granada.

   - ``../<block_id>/helpers/scripts/run_code/normalized``
     (deprecated alias of ``../<block_id>/helpers/scripts/run_code``)
   - ``../<block_id>/helpers/scripts/trace_code/normalized``
     (deprecated alias of ``../<block_id>/helpers/scripts/trace_code``)

-  Increased the LMDB store mapsize limit to avoid ``MDB_MAP_FULL`` failures.

-  Fixed a case where the node was unable to fetch an operation because
   a remote peer did not answer.

-  Fixed various issues with the TLS layer that could in particular
   cause some valid certificates to be refused from remote nodes.

Version 9.2
===========

Node
----

-  Added Granada, a protocol proposal for Mainnet featuring, among others,
   the Emmy* consensus algorithm, Liquidity Baking, and reduced gas consumption.

-  Added the configuration for Granadanet, a test network for Granada,
   as a built-in network alias (``--network granadanet``).

-  Updated the mempool to keep more than 50 non-included operations
   when receiving a new block. In particular, this should result in
   fewer endorsements being missed.

Docker Images
-------------

-  File ``scripts/mainnet.sh`` is now deprecated and may be removed starting from
   version 10.0. If you have a script that downloads this file (with
   ``wget https://gitlab.com/tezos/tezos/raw/latest-release/scripts/mainnet.sh``
   for instance), your script should now download ``scripts/tezos-docker-manager.sh``
   instead and rename it into ``mainnet.sh`` (with
   ``wget -O mainnet.sh https://gitlab.com/tezos/tezos/raw/latest-release/scripts/tezos-docker-manager.sh``
   for instance).

-  File ``scripts/carthagenet.sh`` may also be removed starting from version 10.0.

Version 9.1
===========

Node
----

-  Fixed a performance issue that caused the node to freeze for several minutes
   and memory usage to rise to unexpected levels.

-  Reintroduced the following RPCs in the Florence RPC plugin. These
   RPCs were already present in the Edo protocol plugin and were removed
   by mistake when moving the functionality they offer to the Florence
   protocol:

   - ``../<block_id>/context/contracts/<contract_id>/storage/normalized``
   - ``../<block_id>/context/contracts/<contract_id>/script/normalized``
   - ``../<block_id>/context/big_maps/<big_map_id>/<script_expr>/normalized``
   - ``../<block_id>/helpers/scripts/run_code/normalized``
     (deprecated alias of ``../<block_id>/helpers/scripts/run_code``)
   - ``../<block_id>/helpers/scripts/trace_code/normalized``
     (deprecated alias of ``../<block_id>/helpers/scripts/trace_code``)

Version 9.0
===========

Node
----

-  Fixed a bug where the mempool could crash with an assertion failure.

Version 9.0~rc2
===============


Node
----

-  Fixed a performance regression of the storage backend. This in
   particular impacted RPCs that query the context. This regression was
   introduced in 9.0~rc1.

-  Removed protocol ``PsFLorBA``, the variant of Florence with baking
   accounts, which was rejected in favor of ``PsFLoren``.

-  The cap on the number of expected connections that was introduced in
   9.0~rc1 can now be bypassed with ``--disable-config-validation``.

Baker
-----

-  Added the fixes to the baker that were released in 8.3 but that were
   not present in 9.0~rc1 (which was published before 8.3).


Client
------

-  Improved operation injection to better deal with cases where
   parameters (fees, gas limit, …) are partially given by the user.

Version 9.0~rc1
===============


Node
----

-  Added Florence, the current protocol proposal on Mainnet. This is the
   version of Florence without baking accounts (``PsFLoren``).

-  Added a new version of the protocol environment (v2). It is used by
   Florence.

-  Added built-in network configurations for Edo2net (which runs Edo2,
   the current Mainnet protocol) and Florencenet (which runs Florence).
   Their corresponding aliases for ``--network`` are ``edo2net`` and
   ``florencenet``.

-  Capped the number of expected connections to ``100`` on the
   command-line interface.

-  Fixed a bug that caused the execution of the prevalidator when the
   node was not bootstrapped.

-  Enforced loading of non-embedded protocols before starting the node
   to allow the prevalidator to start correctly.

-  Optimized I/O and CPU usage by removing an unnecessary access to the
   context during block validation.

-  Fixed a bug where any event would allocate more memory than needed
   when it was not to be printed.

-  Added a new RPC for Alpha: ``helpers/scripts/normalize_type``.

-  Replace Edonet by Edo2net in built-in network configuration. The
   alias to give to ``--network`` is now ``edo2net``.

-  Removed the built-in configuration for Delphinet. You can no longer
   configure your node with ``--network delphinet``.

-  The ``--network`` option now also accepts the name of a file
   containing the configuration for a custom network, or a URL from
   which such a file can be downloaded.

-  Fixed JSON encoding of timestamps before epoch (1970).
   Pretty-printing and encoding of dates before epoch in human-readable
   form (as part of a JSON value) that failed in the past will now
   succeed. Binary form (used when nodes exchange data) was unaffected
   by the bug. This may impact some RPC representations of timestamps.

-  Some RPCs now send their response in chunked transfer encoding.
   Additionally, the implementation allows for more concurrency
   internally: it allows RPC requests to be treated even if a request is
   currently being treated. This leads to some improved response times
   on some RPC requests.

-  Added a way to optionally specify an expected peer identity for all
   command line options accepting a point as argument (such as
   ``--peer``). This identity can be given using the usual b58 format.
   The RPC ``patch /network/points/<point> {"peer_id": <peer_id>}`` set
   the expected identity and ``get /network/points/<point>`` tells
   whether an expected ``peer_id`` has been set.

-  Added a checking of the well-formedness of addresses in the config
   files when the node starts. If this check fails, the node stops with
   an explanation.

-  Fixed the targeted number of connections which did not respect the
   constraints expressed with –connections settings.

-  RPC: the semantics of ban and unban has changed:

   -  instead of just affecting the banned/unbanned point, they affect
      all associated cryptographic identities;
   -  additionally, ban now removes the cryptographic identity / point
      from the whitelist, which was not previously the case.

-  RPC: the following RPCs are now deprecated:

   -  GET: ``/network/peers/<peer_id>/ban``
   -  GET: ``/network/peers/<peer_id>/unban``
   -  GET: ``/network/peers/<peer_id>/trust``
   -  GET: ``/network/peers/<peer_id>/untrust``
   -  GET: ``/network/points/<point>/ban``
   -  GET: ``/network/points/<point>/unban``
   -  GET: ``/network/points/<point>/trust``
   -  GET: ``/network/points/<point>/untrust``

-  RPC: the following RPCs are added and replace those above:

   -  PATCH: ``/network/peers/<peer_id>`` payload
      ``{ acl: [ban,trust,open] }``
   -  PATCH: ``/network/point/<point>`` payload
      ``{ acl: [ban,trust,open], peer_id: <peer_id> }`` where

      -  ``{acl : ban}``: blacklist the given address/peer and remove it
         from the whitelist if present
      -  ``{acl: trust}``: trust a given address/peer permanently and
         remove it from the blacklist if present.
      -  ``{acl: open}``: removes an address/peer from the blacklist and
         whitelist.

-  Added RPC ``DELETE /network/greylist`` to clear the greylist tables.
   RPC ``GET /network/greylist/clear`` is now deprecated.


Client
------

-  Fixed the return code of errors in the client calls to be non-zero.

-  Added a new multisig command to change keys and threshold:
   ``set threshold of multisig contract ...``.

-  Added a command to perform protocol migrations in persistent mockup
   mode: ``migrate mockup to <protocol_hash>``.

-  Added the ``--version`` flag.

-  Fixed commands ``--mode mockup config show`` and
   ``--mode mockup config init`` which returned the default values
   rather than the actual ones.

-  Replaced command ``check that <bytes> was signed by <pkh>`` by
   ``check that bytes <bytes> were signed by <pkh>`` to differentiate
   from new command ``check that message <string> was signed by <pkh>``.

-  Added wallet support for PVSS keys.

-  Added support for all protocol constants in Mockup mode.

-  Mockup mode now uses Alpha instead of an arbitrary protocol when none
   is specified. It also warns that it takes this default behavior.


Baker / Endorser / Accuser
--------------------------

-  Added the ``--version`` flag.

-  Fixed the operation ordering in the baker so that the most profitable
   operations are applied first.


Protocol Compiler And Environment
---------------------------------

-  Added the ``--version`` flag.


Codec
-----

-  Added the ``--version`` flag.

-  Added support for some base encodings including arbitrary precision
   integers, n-bit sized integers, and floating point numbers.


Miscellaneous
-------------

-  Sapling: fixed dummy address generator (the last 5 bits are now
   correctly set to 0 instead of the first 5 bits).

-  Fixed a bug that caused some file descriptors to be leaked to
   external processes.

Version 8.3
===========


Baker / Endorser / Accuser
--------------------------

-  Fixed a bug where the baker would not consider all of the operations
   when a costly one was encountered.

-  Fixed a bug where the most profitable operations would not be applied
   first.

Version 8.2
===========


Node
----

-  Override ``PtEdoTez`` activation by ``PtEdo2Zk`` in mainnet network.

-  Make size limits on p2p messages explicit in low-level encodings.

-  Add new RPCs for Edo:
   ``helpers/scripts/normalize_{data,script,type}`` and a
   ``XXX/normalized`` variant to each protocol RPC ``XXX`` outputting
   Michelson expressions.


Baker / Endorser / Accuser
--------------------------

-  Replace ``PtEdoTez`` by ``PtEdo2Zk``.


Miscellaneous
-------------

-  Update external opam dependencies. In particular, switch to
   ``hacl-star.0.3.0-1`` which performs better.

Version 8.1
===========


Node
----

-  Fix a performance regression affecting serialization of tz3
   signatures by reverting the P256 implementation to ``uecc``.

-  Fixup allowing nodes in ``--history-mode full`` to answer to all new
   messages to the distributed database protocol.


Client
------

-  As a consequence of moving back to ``uecc``, revert for now the
   ability to sign with tz3 addresses.


Miscellaneous
-------------

-  Allow building from sources with older version of git (used to
   require 2.18)

-  Downgrade ``mirage-crypto`` dependency to avoid failure on startup
   with ``illegal instruction`` on some hardware.

Version 8.0
===========


Node
----

-  Added two new bootstrap peers for Mainnet and one for Edonet.

-  Fixes a bug where any event would allocate more memory than needed
   when it were not to be printed.

-  Improved how the node stores buffered messages from peers to consume
   less memory.

-  Enforce loading of non-embedded protocols before starting the node
   allowing the prevalidator to start correctly.

-  Optimized the I/O and CPU usage by removing an unnecessary access to
   the context during block validation.


Docker Images
-------------

-  Bump up base image to ``alpine:12``. In particular, it changes rust
   and python versions to 1.44.0 and 3.8.5 respectively.


Miscellaneous
-------------

-  Recommend rust version 1.44.0 instead of 1.39.0.

Version 8.0~rc2
===============


Node
----

-  Snapshots exported by a node using version 8 cannot be imported by a
   node running version 7. This is because the new snapshots contain
   additional information required by protocol Edo. On the other hand,
   snapshots exported by a node using version 7 can be imported by a
   node running version 8.

-  Added a new version (version 1) of the protocol environment. The
   environment is the set of functions and types that the economic
   protocol can use. Protocols up to Delphi used environment version 0.
   The Edo protocol uses environment version 1.

-  Added the Edo protocol: the node, client and codec now comes linked
   with Edo, and the Edo daemons (baker, endorser and accuser) are
   available.

-  Added a built-in configuration for Edonet, a test network that runs
   Edo. You can configure your node to use this test network with
   ``--network edonet``.

-  Removed the built-in configuration for Carthagenet, which ends its
   life on December 12th 2020. You can no longer configure your node
   with ``--network carthagenet``.

-  The bootstrap pipeline no longer tries to concurrently download steps
   from other peers. The result is actually a more efficient bootstrap,
   because those concurrent downloads resulted in multiple attempts to
   download the same block headers. It also resulted in more memory
   usage than necessary.

-  Added six messages to the distributed database protocol and bumped
   its version from 0 to 1. These new messages allow to request for: a
   peer’s checkpoint, the branch of a given protocol and a block’s
   predecessor for a given offset. These messages are not yet used but
   will be useful for future optimizations.

-  You can now specify the data directory using environment variable
   ``TEZOS_NODE_DIR``. If you both set this environment variable and
   specify ``--data-dir``, the latter will be used.

-  Added new RPC ``/config`` to query the configuration of a node.

-  Changed signal handling and exit codes for most binaries. The codes’
   significance are detailed in :doc:`the user documentation <user/exits>`.

-  Command ``octez-node --version`` now exits with exit code 0 instead
   of 1.

-  Fixed the synchronisation threshold which was wrongly capped with an
   upper bound of 2 instead of a lower bound of 2 when ``--connections``
   was explicitely specified while the synchronisation threshold itself
   was not specified.


Client
------

-  Added client command ``import keys from mnemonic``, which allows to
   import a key from a mnemonic following the BIP39 standard.

-  When the client asks for a password, it no longer tries to hide its
   input if the client was not run from a terminal, which allows for use
   in a script.

-  You can now specify the base directory using environment variable
   ``TEZOS_CLIENT_DIR``. If you both set this environment variable and
   specify ``--base-dir``, the latter will be used.

-  Fixed command ``set delegate for <SRC> to <DLGT>`` to accept public
   key hashes for the ``<DLGT>`` field.

-  Fixed the ``rpc`` command that did not use the full path of the URL
   provided to ``--endpoint``. Before this,
   ``--endpoint http://localhost:8732/node/rpc`` would have been
   equivalent to ``--endpoint http://localhost:8732``.

-  Fixed an issue where the client would try to sign with a key for
   which the private counterpart was unknown even though a remote signer
   was connected.


Baker / Endorser / Accuser
--------------------------

-  Fixed a crash (assertion error) that could happen at exit, in
   particular if a baker were connected.


Docker Images
-------------

-  Docker images are now available for arm64. Image tags stay the same
   but now refer to “multi-arch” manifests.

Version 8.0~rc1
===============


Node
----

-  Fixed some cases where the node would not stop when interrupted with
   Ctrl+C.

-  The node’s mempool relies on a new synchronisation heuristic. The
   node’s behaviour, especially at startup, may differ slightly; log
   messages in particular are likely to be different. More information
   is available in the whitedoc.

-  The new synchronisation heuristic emits an event when the
   synchronisation status changes. This can be used to detect when the
   chain is stuck for example. More information is available in the
   whitedoc.

-  Node option ``--bootstrap-threshold`` is now deprecated and may be
   removed starting from version 9.0. Use
   ``--synchronisation-threshold`` instead.

-  Fixed an issue which prevented using ports higher than 32767 in the
   client configuration file.

-  The ``octez-node run`` command now automatically generates an
   identity file as if you had run ``octez-node identity generate`` if
   its data directory contains no identity file.

-  Improved various log messages and errors.

-  When bootstrapping, do not greylist peers in rolling mode whose
   oldest known block is newer than our head.

-  Made the timestamp in log messages more precise (added milliseconds).

-  Fixed encoding of P2P header message length for larger lengths.

-  Added ``-d`` as a short-hand for the ``--data-dir`` option of the
   node.

-  Added a built-in activator key for the built-in sandbox network. This
   allows to spawn a sandbox without the need for a custom genesis
   protocol.

-  Greylist the identity and address of peers that send malformed
   messages.

-  Fixed some cases where the context was not closed properly when
   terminating a node or if the baker failed to bake a block.

-  Removed the “get operation hashes” and “operation hashes” messages of
   the distributed database protocol. Those messages were never used.

-  Reduced the amount of log messages being kept in memory (that can be
   queried using RPCs) before they are discarded to reduce the total
   memory footprint.

-  Fixed a case where the ``/workers/prevalidator`` RPC could fail if
   there were too many workers.

-  Fixed how protocol errors are displayed. Before, there were printed
   using the cryptic ``consequence of bad union`` message.

-  Pruned blocks can now be queried using RPC
   ``/chains/<chain>/blocks/<block>``. The ``metadata`` field will be
   empty in the response, leaving only the header.

-  Fixed handling of pre-epoch timestamps, in particular in RPCs.

-  Time is now output with millisecond precision when calling RPCs.

-  Fixed the ``/chains/<chain>/blocks`` RPC which sometimes did not
   return all blocks.

-  Improved the performance of the progress indicator when importing
   snapshots.

-  Improved performance of ``octez-node snapshot export``.

-  Fixed the node which sent too many “get current branch” messages to
   its peers on testchain activation.

- **Breaking change** Bumped the node’s storage version to
  ``3.2``. This new version changes the store’s representation,
  allowing the storage to scale to the increasing number of blocks per
  cycles, thus paving the way to reducing even more the block
  time. Upgrading to this new version must be done manually (using the
  ``octez-node upgrade storage`` command) and is irreversible. (MR
  :gl:`!14211`)

- **Breaking change** Bumped the snapshot version from ``7`` to ``8``,
  in order to support the changes introduced by the ``3.2`` storage
  version. Snapshots of version ``7`` exported with previous versions
  of Octez (``v20``) can still be imported. Snapshots of version ``8``
  are not retro-compatible with previous octez versions (MR
  :gl:`!14398`).

Client
------

-  The ``octez-client config show`` command now takes into account the
   command line arguments.

-  Fixed an issue which caused ``octez-client rpc get /errors`` as well
   as ``octez-codec dump encodings`` to fail because of duplicate
   encodings. As a result, some protocol encodings whose name was not
   prefixed by the protocol name are now prefixed by it. If you have
   tools which rely on encoding names you may have to update them.

-  Added client command
   ``multiple transfers from <src> using <transfers.json>`` to perform
   multiple operations from the same address in a single command.

-  Added option ``--endpoint`` to client and bakers. It replaces options
   ``--addr``, ``--port`` and ``--tls`` which are now deprecated.

-  Added command ``rpc patch`` to the client, to perform RPCs using the
   PATCH HTTP method.

-  Make the client emit a more human-readable error if it failed to
   understand an error from the node.

-  Added client commands
   ``octez-client convert script <script> from <input> to <output>`` and
   ``octez-client convert data <data> from <input> to <output>`` to
   convert to and from michelson, JSON, binary and OCaml with
   type-checking.

-  The client now retries commands a few times if the node is not yet
   ready.

-  Added client command ``compute chain id from block hash <hash>`` and
   ``compute chain id from seed <seed>`` to compute the chain id
   corresponding to, respectively, a block hash or a seed.

-  Added the verbose-signing switch to a number of multisig commands.

-  The ``prepare multisig`` commands now display the Blake 2B hash.

-  Some client commands which use the default zero key
   ``tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU`` in dry runs now display this
   key using an informative string
   ``the baker who will include this operation`` instead of the key
   itself.

-  Fixed an error which occurred in the client when several keys had the
   same alias.

-  Added support for some ``rpc {get,post,...}`` commands in the
   client’s mockup mode.

-  Added ``--mode mockup`` flag to ``config init`` for the client’s
   mockup mode, that writes the mockup’s current configuration to files.

-  Added ``--mode mockup`` flag to ``config show`` for the client’s
   mockup mode, that prints the mockup’s current configuration to
   standard output.

-  Added arguments ``--bootstrap-accounts`` and ``--protocol-constants``
   to the client’s ``create mockup`` command. ``--bootstrap-accounts``
   allows changing the client’s bootstrap accounts and
   ``--protocol-constants`` allows overriding some of the protocol’s
   constants. Use commands ``config {show,init} mockup`` (on an existing
   mockup) to see the expected format of these arguments.

-  The client no longer creates the base directory by default in mockup
   mode.

-  Fixed the argument ``--password-filename`` option which was ignored
   if it was present in the configuration file.


Baker / Endorser / Accuser
--------------------------

-  The baker now automatically tries to bake again in case it failed. It
   retries at most 5 times.

-  The baker now outputs an explicit error when it loses connection with
   the node.

-  Added command-line option ``--keep-alive`` for the baker. It causes
   the baker to attempt to reconnect automatically if it loses
   connection with the node.


Protocol Compiler And Environment
---------------------------------

-  Prepare the addition of SHA-3 and Keccak-256 cryptographic
   primitives.

-  Prepare the introduction of the new protocol environment for protocol
   008.

-  The protocol compiler now rejects protocols for which the OCaml
   compiler emits warnings.


Codec
-----

-  Fixed ``octez-codec dump encodings`` which failed due to two
   encodings having the same name.

Version 7.5
===========


Client
------

-  Fixed gas cost estimation for Delphi for contract origination and
   revelation.


Codec
-----

-  Fixed the name of the ``big_map_diff`` encoding from
   ``<protocol_name>`` to ``<protocol_name>.contract.big_map_diff``.

Version 7.4
===========

-  Added the Delphi protocol.

-  Added the Delphinet built-in network configuration. The alias to give
   to ``--network`` is ``delphinet``.

-  Updated the list of bootstrap peers for Carthagenet.

Version 7.3
===========

-  Fixed a case where the number of open file descriptors was not
   correctly limited. This could result in the node crashing due to
   being out of file descriptors.

-  Set a limit to the length of some incoming messages which previously
   did not have one.

-  Fixed some value encodings which were missing cases.

Version 7.2
===========

-  Fixed an error that could cause baking to fail when validating some
   smart contracts.

-  Fixed an issue in ``tezos-docker-manager.sh`` which prevented to use
   some options, such as ``--rpc-port``.

Version 7.1
===========

Source Compilation
------------------

-  The ``Makefile`` now ignores directories with no
   ``lib_protocol/TEZOS_PROTOCOL`` files when listing protocols to
   compile. This fixes an error where ``make`` complained that it had no
   rule to build ``TEZOS_PROTOCOL`` for directories that Git does not
   completely remove when switching branches.

-  One can now use opam 2.0.0 again. In version 7.0, an error saying
   that it did not know about option ``--silent`` was emitted.

-  The repository no longer contains file names which are longer than
   140 characters. Longer file names prevented users from checking out
   version 7.0 on encrypted file systems in particular.

-  Fixed an issue causing ``make build-deps`` to sometimes fail after an
   update of the digestif external library.


Client
------

-  Optimized the LAMBDA which is built when injecting manager
   operations.

-  Fixed a bug which caused the wrong entrypoint (``set_delegate``
   instead of ``remove_delegate``) from being used in some cases when
   setting delegates.

-  Command ``activate account ... with`` can now be given a JSON value
   directly as an argument instead of only a filename.

-  Syntax for command ``call from <SRC> to <DST>`` has been fixed to
   match the one for ``proto_alpha``. It should now be called as
   ``call <DST> from <SRC>``.

Version 7.0
===========

Multinetwork
------------

-  Node and client now come with all current and past protocols that are
   still in use on Mainnet or some active test networks.

-  Added option ``--network`` to ``octez-node config init`` to select
   which network to connect to from a list of built-in networks (e.g.
   ``carthagenet``). If you do not run ``config init`` or run it without
   the ``--network`` option, the node will use the default network
   (Mainnet).

-  Added option ``--network`` to ``octez-node run`` and
   ``octez-node snapshot import`` which causes the node to check that it
   is configured to use the given network.

-  Added ``network`` configuration field to select which network to
   connect to, similar to ``--network``. This field also lets you
   specify an entirely custom, non-built-in network and is especially
   useful to run private networks. For instance, LabNet
   (https://forum.tezosagora.org/t/introducing-labnet-a-rapid-iteration-testnet-for-tezos/1522)
   uses such a custom configuration.

-  The ``network`` configuration field also allows to specify
   user-activated upgrades and user-activated protocol overrides. In the
   past, those upgrades and overrides required you to upgrade the node;
   now, you can just edit the configuration file instead. You can also
   disable built-in upgrades by specifying the configuration explicitly.

-  The ``network`` configuration field also allows to specify the
   parameters of the genesis protocol, such as the activation key of
   ``proto_genesis``. This allows to use the same genesis protocol for
   several test networks with different activation keys.

-  The network name is printed in the logs on startup.

For more information, see :doc:`user/multinetwork`.


Node
----

-  Added RPC ``/version`` which returns the version of the node, the
   version of the P2P protocol, the version of the distributed DB, the
   commit hash and the commit date. Other RPCs which returned version
   numbers (``/network/version``, ``/network/versions`` and
   ``/monitor/commit_hash``) are deprecated: use ``/version`` instead.

-  RPCs which returned ``treated`` and ``completed`` fields now return
   durations (relative to the value of the ``pushed`` field) instead of
   timestamps.

-  Improved various log messages and errors.

-  Fixed a memory leak causing greylisted addresses to be stored several
   times unnecessarily.

-  Fixed a small memory leak causing each new worker to store a logger
   section name forever.

-  When exporting snapshots, you can now specify the block not only by
   its hash but also by its level or using an alias such as:
   ``caboose``, ``checkpoint``, ``save_point`` or ``head``.

-  Fixed a bug which caused snapshots to fail if the checkpoint was a
   protocol transition block.

-  Added ``--status`` flag to ``upgrade storage``. This flag causes the
   node to tell you whether a storage upgrade is available.

-  Allow more files to exist in the data directory when starting a node
   from an empty storage: ``version.json``, ``identity.json``,
   ``config.json`` and ``peers.json``. Before, only ``identity.json``
   was allowed.

-  Fixed a bug which caused the check of the ``version.json`` file to be
   performed incorrectly.

-  The external validator process now dynamically loads the new protocol
   after a protocol upgrade.

-  Sandbox mode may now be used with the external validator process.
   Before, it required ``--singleprocess``.

-  The mempool RPC for preapplication now actually sorts operations when
   the flag is set.

-  Changed the format of the peer-to-peer protocol version number. Nodes
   which are running a version older than Mainnet December 2019 can no
   longer connect to nodes running this new version and should upgrade.

-  Added new peer-to-peer message type: Nack, that carries a list of
   alternative peers and can be returned by nodes with no room for your
   connection.

-  If maximum number of connections has been reached, before rejecting
   peers, authenticate them and memorize their point information.

-  Improved the behavior of the greylist of peers.

-  The node is now capable of recovering from some cases of storage
   corruption that could in particular occur if the disk became full or
   if the node was killed.

-  Fixed a bug which caused the peer-to-peer layer to send the wrong
   acknowledgement message in response to swap requests.

-  Nodes built for Docker images should now correctly contain the
   version number.

-  Removed non-read-only Babylon client commands as they are no longer
   useful.

-  If the node connects to a peer of another network (e.g. if a Mainnet
   node connects to a Carthagenet node), it now removes this peer from
   its list of known peers. This in particular means that it will no
   longer advertize this peer or try to connect to it again.

-  In private mode, do not try to discover the local network peers as
   they will not be trusted anyway.

-  Fixed a bug which caused the node to stop with a segmentation fault.


Client
------

-  Added protocol command ``expand macros in`` to expand macros in
   Michelson code.

-  Added command ``octez-admin-client protocol environment`` which
   displays the version of the environment used by a given protocol.

-  Greatly reduce the time the client takes to load.

-  Added option ``--mode mockup`` which can be used to run client
   commands, such as commands to typecheck Michelson code, without a
   running node.

-  Added commands ``create mockup for protocol`` and
   ``list mockup protocols`` to manage mockup environments used by
   ``--mode mockup``.

-  Multisig commands can now be used both with contract aliases and
   addresses instead of only with aliases.

-  Added a timeout to signature operations using a remote signer, which
   could otherwise block the baker, endorser or accuser.

Protocol
--------

-  Added safety checks against code injection when compiling downloaded
   or injected protocols. This was mostly a security concern for nodes
   with publicly available RPCs.

-  Added new demo protocol: ``proto_demo_counter``.

-  Prepared the shell to be able to handle multiple protocol environment
   versions.

Docker Script
-------------

-  Renamed script ``alphanet.sh`` into ``tezos-docker-manager.sh``. You
   should still use ``mainnet.sh`` and ``carthagenet.sh`` as they are
   now symbolic links to ``tezos-docker-manager.sh`` instead of
   ``alphanet.sh``.

-  Removed script ``zeronet.sh`` as Zeronet is using an older version of
   Babylon (PsBABY5H) for which the baker, endorser and accuser binaries
   are no longer available. If you need to connect to Zeronet, use the
   ``zeronet`` branch instead, which still has the ``zeronet.sh``
   script.


Miscellaneous
-------------

-  Remove outdated nginx.conf.
