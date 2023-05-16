:orphan:

Changelog
'''''''''

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
  ``name`` of unstripped kernels (MR :gl:`!8522`)

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
   of serving `protocol RPCs <https://tezos.gitlab.io/alpha/rpc.html>`__.
   An instance of a proxy server is protocol-specific: it automatically picks
   up the protocol from the backing node when it starts. Proxy servers
   can be started and destroyed at will, making them easy to deploy.

   Please refer to the `online documentation <https://tezos.gitlab.io/user/proxy-server.html>`__
   for further details.

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
   significance are detailed in `the user
   documentation <http://tezos.gitlab.io/user/various.html#tezos_binaries_signals_and_exit_codes>`__.

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

For more information, see: http://tezos.gitlab.io/user/multinetwork.html


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
