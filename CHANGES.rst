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

Node
----

- Changed the default ``history-mode`` from ``Full`` to ``Rolling``. (MR :gl:`!15942`)

- Introduced a specific exit code for the ``octez-node upgrade storage
  --status`` command. It now returns the exit code 1 when an upgrade
  is available. 0 is returned when the storage is up to date. (MR :gl:`!15152`)

- New RPCs ``/chain/{chain_id}/protocols`` (and
  ``/chain/{chain_id}/protocols/{protocol_hash}``) to retrieve protocol
  activation levels of the chain. (MR :gl:`!15447`)

- The node will detect stalled connections more quickly (on
  Linux-based distributions). This behavior can be controlled via the
  environment variable ``OCTEZ_P2P_TCP_USER_TIMEOUT``. Its default
  value is ``15000``, meaning that it will now take ``15s`` to detect
  a stalled connection (compared to up to ``15`` minutes by default on
  Linux). Users can opt out by setting the value to ``0``. (MR
  :gl:`!16907`)

Client
------

Baker
-----

Agnostic Baker
--------------

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

- In the bailout mode there was a bug where the wrong key was used
  when recovering the bond. The node uses the ``cementing`` key and not
  the ``operating`` key. (MR :gl:`!16016`).

- updated RPC ``DELETE /admin/injector/queues`` with new query to
  clear injector queues based on priority order. The RPC can takes two
  optional arguments:

  + ``order_below``: an integer that filters out all operations with
    order strictly inferior to it.

  + ``drop_no_order``: a boolean that if true remove all operations
    that has no order specified. ``false`` by default.

  When ``tag`` is specified only operation of that type will be
  considered, else all operations are considered.(MR :gl:`!15929`)

- Added RPC ``DELETE /admin/batcher/queue``, which can take two optional
  arguments:

  + ``order_below``: an integer that filters all messages with order
    inferior to it.

  + ``drop_no_order``: a boolean that if true remove all messages that
    has no order specified. ``false` by default. If no ``order_below``
    is specified it completely clear the queue.

  (MR :gl:`!15929`)

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

- Addition of ``elapsed_time`` to performance metrics,
  which exposes in seconds the time since the node started. (MR :gl:`!16551`)

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

- Fixed an issue which could introduce a discrepancy between the snapshot header
  and its content. (MR :gl:`!14777`)

- RPC ``/global/block/<block_id>/outbox/<outbox_level>/messages`` now fails if
  ``outbox_level`` is above the level of ``block_id``. (MR :gl:`!14911`)

- Improved error messages for RPC
  ``/global/block/<block_id>/helpers/proofs/outbox/<outbox_level>/messages?index=<message_index>``. (MR :gl:`!15507`)

- Paginate RPC for durable storage subkeys
  ``/global/block/<block_id>/durable/wasm_2_0_0/subkeys?key=<key>&offset=<offset>&length=<length>``,
  with new query parameters ``offset`` and ``length``. (MR :gl:`!15625`)

- Fixed file descriptor leak in resto for connections with the L1 node.
  (MR :gl:`!15322`)

- Fixed potential issue with store with SQLite < 3.35. (MR :gl:`!15631`)
- Improved error messages for RPC
  ``/global/block/<block_id>/helpers/proofs/outbox/<outbox_level>/messages?index=<message_index>``. (MR :gl:`!15507`)

- Fix potential issue with store with SQLite < 3.35. (MR :gl:`!15631`)

- New CLI switch ``--unsafe-disable-wasm-kernel-checks`` which allows to bypass
  invalid kernel checks in the WASM VM, for use by jstz. (MR :gl:`!15910`)

- Support ``remote`` signer scheme and check remote signer available on
  startup. (MR :gl:`!16651`)

- Add query parameter ``outbox_level`` for RPCs
  ``/local/outbox/pending/executable`` and
  ``/local/outbox/pending/unexecutable``. (MR :gl:`!16831`)

- Add a new RPC RPC ``/local/outbox/pending`` to fetch all known outbox messages
  with their status. (MR :gl:`!16831`)

Smart Rollup WASM Debugger
--------------------------

Data Availability Layer (DAL)
-----------------------------

DAL node
~~~~~~~~

- **Feature** The node will detect stalled connections more quickly (on
  Linux-based distributions). This behavior can be controlled via the
  environment variable ``OCTEZ_P2P_TCP_USER_TIMEOUT``. Its default
  value is ``15000``, meaning that it will now take ``15s`` to detect
  a stalled connection (compared to up to ``15`` minutes by default on
  Linux). Users can opt out by setting the value to ``0``. (MR
  :gl:`!16907`)

- **Feature** The DAL node stores now a peers.json file in its
  directory when it is shutdown with SIGINT. This file is read if it
  exists when starting the DAL node to restore previous known
  connections quickly.

- **Bugfix** When shutting down the DAL node using SIGINT, it does a
  best effort to shutdown properly its running P2P connections

- The DAL node supports a ``config update`` command to update an
  existing configuration. It takes the same arguments as for the other
  commands. (MR :gl:`!15759`)

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

- Fixed file descriptor leak in resto affecting connections to the L1 node.
  (MR :gl:`!15322`)

- **Feature** The DAL node downloads trusted setup files when launched in observer
   or operator mode. (MR :gl:`!16102`)

- Added a new RPC ``/last_processed_level`` to retrieve the last (finalized) L1
  level processed by a DAL node (MR :gl:`!16420`)
- A warning is emitted when registering a public key hash (as an attester
  profile) that does not correspond to that of a delegate. (MR :gl:`!16336`)

- Set the message validation function at node startup, fixing
  https://gitlab.com/tezos/tezos/-/issues/7629. (MR :gl:`!15830`)

- A warning has been introduced in case it is observed that the DAL node lags
  behind the L1 node. (MR :gl:`!15756`)

- **Change** The DAL node store version has been upgraded from 1 to 2.
  The DAL node store will automatically upgrade without requiring any
  user action. For users running the DAL node with the
  ``--operator-profile`` flag enabled, the node now uses SQLite
  specifically for managing skip list cells (MR :gl:`!15780`),
  preventing inode exhaustion. All other stores remain unchanged.

- Added a new RPC ``GET /protocol_parameters/`` that retrieve the protocol
  parameters that the DAL node uses for a given level, which by default is the
  last finalized level the node is aware of. (MR :gl:`!16704`)

- Added a new RPC ``GET /published_levels/<level>/known_traps`` that returns the
  trap shards that the DAL node knows. (MR :gl:`!16870`)

Protocol
~~~~~~~~

Miscellaneous
-------------
