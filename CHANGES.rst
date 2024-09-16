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

- Add and **experimental** switch to enable the use of the Brassaia context
  backend using ``TEZOS_CONTEXT_BACKEND=Brassaia`` environment variable. (MR :gl:`!13054`)

- Removed binaries for Oxford. (MR :gl:`!13795`)

- Removed binaries for ParisB. (MR :gl:`!14026`)

Node
----

- Add an ``source`` argument to ``GET
  /chains/<chain>/mempool/pending_operations`` which allows operations
  to be filtered by source. (MR :gl:`!11278`)

- Add an ``operation_hash`` argument to ``GET
  /chains/<chain>/mempool/pending_operations`` which allows operations
  to be filtered by hash. (MR :gl:`!13977`)

- Add a ``source`` argument to ``GET
  /chains/<chain>/mempool/monitor_operations`` which allows operations
  to be filtered by source. (MR :gl:`!14284`)

- Add an RPC
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

- Remove support for deprecated version ``0`` for RPCs ``GET
  ../mempool/monitor_operations``, ``POST ../helpers/preapply/operations``,
  ``GET ../blocks/<block>``, ``GET ../blocks/<blocks>/metadata``. and ``GET
  ../blocks/<blocks>/operations``. (MR :gl:`!13449`)

- Remove support for deprecated version ``1`` for RPC ``GET
  ../mempool/pending_operations``. (MR :gl:`!13449`)

- Remove support for deprecated version ``0`` from RPCs ``POST
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

- **Breaking change** Removed all bls key related command in favor of
  generics one. All keys that were generated with ``bls gen keys`` can
  be used with usual command of the octez-client (``list``, ``known``,
  ``sign``, ...).  (MR :gl:`!14417`)

- **Breaking change** Removed read-write commands specific to Oxford. (MR :gl:`!13799`)

- **Breaking changes** client's encoding with legacy attestation name are no
  longer supported starting from protocol following ParisC. (MR :gl:`!13454`)

- **Breaking change** Removed read-write commands specific to ParisB. (MR :gl:`!14033`)

Baker
-----

- When available, add some colors to some event logs: (MR :gl:`!13611`)
   - block_injected is now Blue
   - revealing_nonce is now Cyan

- Branch used in consensus operation is now the grandparent block instead of the
  parent block. This is done to avoid having consensus operation branched on
  block that are not part of the canonical chain anymore.(MR :gl:`!13619`)

- Remove ``preendorse for`` and ``endorse for`` deprecated commands from baker.
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

Proxy Server
------------

Protocol Compiler And Environment
---------------------------------

- Added a new version of the protocol environment (V13). (MR :gl:`!12966`)

- Remove with_legacy_attestation_name encodings from the protocol environment.
  (MR :gl:`!14046`)

Codec
-----

Docker Images
-------------

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

- Introduce the 5th version of the WASM PVM, which defaults to a higher tick
  limits to delegate refutability to the kernels. (MR :gl:`!12999`)

- Trigger GC every 1000 blocks (instead of 100) by default to reduce CPU
  consumption. (MR :gl:`!13177`)

- Default history mode is now "full". (MR :gl:`!13178`)

- Allow to import archive snapshots in "full" rollup node. (MR :gl:`!13186`)

- Fix a bug in how commitments are computed after a protocol migration
  where the the commitment period changes. (MR :gl:`!13588`)

- Ensure penultimate commitment is published on snapshot export as a
  failsafe. (MR :gl:`!13544`)

- Include commitment publication information in snapshots. (MR :gl:`!13724`)

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

- Introduce the 6th version of the WASM PVM. (MR :gl:`!14493`)

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

Smart Rollup WASM Debugger
--------------------------

Data Availability Committee (DAC)
---------------------------------

Data Availability Layer (DAL)
-----------------------------

DAL node
~~~~~~~~

Reduce the number of inodes used by a bootstrap node. This fixes an issue
where the number of inodes used was too high with respect to the disk size. (MR :gl:`!12900`)

The DAL node's store has been updated, and it is not compatible with
V20. However, a V20 store is upgraded at startup. (MR :gl:`!13820`)

The format of the configuration file (and in particular that of profiles) has
been updated. However, the node is able to read V20 configuration files. (MR
:gl:`!12968`, MR :gl:`!13787`)

The profile names have changed, in particular '(slot) producers' are now called
'operators'. Accordingly, the node has a new argument ``--operator`` that should
be used instead of ``--producer-profiles``, which is deprecated and will be
removed at the next release, but still supported. (MR :gl:`!14261`, MR
:gl:`!14277`)

The following RPCs have been removed:

- ``POST /commitments`` (MR :gl:`!12949`), use ``POST /slots`` instead,
- ``GET /commitments/<c>/proof`` (MR :gl:`!13080`), also use ``POST /slots`` instead,
- ``PATCH /commitments`` (MR :gl:`!12886`),
- ``PUT /commitments/<c>/shards`` (MR :gl:`!12949`),
- ``GET /levels/<int32>/headers`` (MR :gl:`!13044`).

The paths or method of the following RPCs have been updated:

- ``GET /commitments/<c>/slot`` is now ``GET /levels/<l>/slots/<i>/content``  (MR :gl:`!13075`),
- ``GET /levels/<l>/slot_indices/<i>/commitment`` is now ``GET /levels/<l>/slots/<i>/commitment``  (MR :gl:`!13046`),
- ``POST /pages/<p>/proof`` is now ``GET /levels/<l>/slots/<i>/pages/<p>/proof``  (MR :gl:`!13083`),
- ``GET /shard/<c>/<s>`` is now ``GET /levels/<l>/slots/<i>/shards/<s>/content`` (MR :gl:`!13095`),
- ``POST /slot`` is now ``POST /slots`` (MR :gl:`!12949`),
- ``GET /slot/pages/<c>`` is now ``GET /levels/<l>/slots/<i>/pages`` (MR :gl:`!12880`),
- ``GET /commitments/<c>/headers`` is now ``GET /levels/<l>/slots/<i>/status`` (MR :gl:`!13055`),
- ``GET /p2p/peers/list`` is now ``GET /p2p/peers`` (MR :gl:`!14521`).

Two new RPCs have been added:

- ``GET /p2p/gossipsub/slot_indexes/peers``
- ``GET /p2p/gossipsub/pkhs/peers``

These two new RPCs are similar to ``GET /p2p/gossipsub/topics/`` but instead of
grouping peers by topic they group them by slot indices or attester's public key
hashes (``pkhs``) appearing in the relevant topics. (MR :gl:`!14504`)

In the output of ``GET /p2p/peers/info``, the field ``"point"`` has been renamed
to ``"peer"``. (MR :gl:`!14521`)

A new RPC ``GET /health`` has been added to check the status on the node (MR :gl:`!14670`).

An optional ``slot_index`` numerical query argument has been added to
RPC ``POST /slots``. When provided, the DAL node checks that its
profile allows to publish data on the given slot index (MR :gl:`!14825`).

Miscellaneous
-------------

- Depends on OCaml 4.14.2 (was 4.14.1 before). (MR :gl:`!14536`)

- Current Debian packages are now available via APT repository for Ubuntu
  (Noble and Jammy) and Debian Bookworm, both for AMD64 and ARM64.

- New set of Debian packages are now available for testing. These new set of
  packages are built following Debian best practices for packaging, use debconf
  for configuration and systemd to handle the runtime lifecycle of the daemons.
  These new packages are going to introduce few breaking changes starting from
  the next release. Please check our documentation for more details. (MR
  :gl:`!13273`)
