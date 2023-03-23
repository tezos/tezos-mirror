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
  irreversible. (MR :gl: `!6835` and :gl: `!6959`)

- **Breaking change**: bumped the snapshot version to ``5``. This
  version changes internal snapshot file representation to include
  more information required by the new protocol's semantics and to
  improve both import and export speed. Snapshots of version ``4``
  exported with previous versions of Octez can still be
  imported. Snapshots of version ``5`` are not backward compatible.
  (MR :gl: `!6835` and :gl: `!6961`)

- Upon receiving a new non-manager operation that conflicts with a
  previously validated operation, the mempool may now replace the old
  operation with the new one, depending on both operations' content
  and hash. This behavior was already in place for manager operations,
  and has simply be extended to non-manager operations. It should help
  all mempools converge toward the same set of accepted operations,
  regardless of the order in which the operations were
  received. (MR :gl: `!6749`)

- Changed the id and message of the error when the mempool rejects a
  new operation because it already contains a preferred conflicting
  operation. Changed the id and message of the error associated with
  an operation that is removed from the mempool to make room for a
  better conflicting operation. (MR :gl: `!6749`)

- Fixed a minor bug that caused the mempool to accept a manager
  operation that conflicts with an already present ``drain_delegate``
  operation. (MR :gl: `!6749`)

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

- Fixed a bug while reconstructing the storage after a snapshot import
  that would result in wrong context hash mapping for some blocks.

- Fixed a bug that caused a context corruption when using an old context.

- **Breaking Change**: disabled snapshot export support for storage
  that was created with Octez v13 (or earlier).

- Deprecated the RPC ``GET /monitor/valid_blocks`` and introduced
  ``GET /monitor/validated_blocks`` and ``GET /monitor/applied_blocks``
  which respectively returns validated blocks, which are not yet applied
  nor stored, and applied blocks which are fully applied and stored by
  the node. (MR :gl: `!7513`)

- Replaced some "precheck" occurrences with "validate" in event and
  error identifiers and messages. (MR :gl: `!7513`)

- Fixed a issue that may trigger unknown keys errors while reading the
  context on a read-only instance.

- Added an RPC ``POST
  /chains/main/blocks/head/context/smart_rollups/all/origination_proof``
  with input ``{"kind":"<smart rollup kind>", "kernel"="<smart rollup
  kernel>"}`` to produce the origination proof needed to originate a
  smart rollup.

- Updated the built-in network alias for Mumbainet (``--network mumbainet``).
  The new alias matches the relaunch of Mumbainet with the protocol `PtMumbai2`.

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
  be permanently idle. (MR :gl: `!6835`)

- Fixed an issue where the baker would keep files opened longer than
  necessary causing unexpected out of space errors making the baker
  crash.

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

- Fixed a bug where the baker could count an (pre)endorsement twice
  while waiting for a (pre)quorum.

- Fixed a bug where receiving an early prequorum would made the baker
  reach a state where it could not endorse anymore.

Accuser
-------

Signer
------

Proxy Server
------------

- The proxy server can now serve endpoints about blocks of all known economic
  protocols instead of only one chosen at boot time.

Protocol Compiler And Environment
---------------------------------

Codec
-----

- Added the ``dump encoding <id>`` command to dump the description of a single
  registered encoding.

Docker Images
-------------

- Change Python versions to 3.10.10.

Rollups
-------

- Release ``octez-smart-rollup-wasm-debugger`` as part of the Octez distribution (MR :gl:`!7295`). See the smart rollups documentation for its functionalities and how to use it to test and debug kernels.

Miscellaneous
-------------

- Versioning of signature module for protocol specific support and future
  extensibility.

- Removed binaries and mempool RPCs of Kathmandu.
