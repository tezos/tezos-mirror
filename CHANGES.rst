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

Node
----

Client
------

Baker
-----

- **Deprecation:** For Paris and Quebec protocols, launching a
  baker daemon without specifying a DAL node endpoint is deprecated.
  To opt out of this requirement, use the newly introduced
  ``--without-dal`` option (MR :gl:`!16213`).
  The CLI argument ``--dal-node <uri>`` or ``--without-dal`` will be mandatory
  in the next version of Octez.

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

- Updated batcher with a new order structure. The RPC
  ``/local/batcher/injection`` now has a new query argument
  possibility ``"order": <int>``. The batcher will batch the
  received chunk with the following priority order: First chunks with
  ascending order then chunks by order of arrival. (MR :gl:`!15672`)

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

- RPC ``/global/block/<block_id>/outbox/<outbox_level>/messages`` now fails if
  ``outbox_level`` is above the level of ``block_id``. (MR :gl:`!14911`)

- Storage now uses SQLite as a backend instead of the custom indexed-file based
  store. This change makes the rollup node more robust but entails a migration
  of the store data. (MRs :gl:`!15053`, :gl:`!15026`, :gl:`!15059`,
  :gl:`!15073`, :gl:`!15218`, :gl:`!15257`)

- Allow to import snaphosts for older stores by migrating the data on import.
  (MR :gl:`!15422`)

- Fixed a bug which would make injection of messages in the batcher with the RPC
  ``/local/batcher/injection`` fail if called too early. (MR :gl:`!15459`)

- Paginate RPC for durable storage subkeys
  ``/global/block/<block_id>/durable/wasm_2_0_0/subkeys?key=<key>&offset=<offset>&length=<length>``,
  with new query parameters ``offset`` and ``length``. (MR :gl:`!15625`)

- New RPC to retrieve values under a key in the durable storage
  ``/global/block/<block_id>/durable/wasm_2_0_0/values?key=<key>&offset=<offset>&length=<length>``.
  (MR :gl:`!15627`)


- RPCs ``/global/block/<block_id>/committed_status`` and to retrieve commitment
  and cementation status for a given block (or an estimated timestamp
  otherwise). (MR :gl:`!15409`)

- Fix an issue in the background store migration which could make the rollup
  node send old heads in its stream at the end of the migration.  (MR
  :gl:`!15739`)

Smart Rollup WASM Debugger
--------------------------

Data Availability Committee (DAC)
---------------------------------

Data Availability Layer (DAL)
-----------------------------

DAL node
~~~~~~~~

- **Breaking_change** The configuration value ``metrics-addr`` is now an option.
  It should not break unless the value differs from the default value
  (``0.0.0.0:11733``). The new default value is ``None``, so no metrics are
  exported by default.

- **Feature** The DAL node stores now a peers.json file in its
  directory when it is shutdown with SIGINT. This file is read if it
  exists when starting the DAL node to restore previous known
  connections quickly.

- **Bugfix** When shutting down the DAL node using SIGINT, it does a
  best effort to shutdown properly its running P2P connections

- **Breaking change** Removed the baker daemon's ``--dal-node-timeout-percentage``
  argument. (MR :gl:`!15554`)

Baker
~~~~~

- Emit event at Notice level when the delegate is not in the DAL committee, that
  is, it has no assigned shards at the current level. (:gl:`!15846`)
- A warning has been introduced in case it is observed that the DAL node lags
  behind the L1 node. (MR :gl:`!15756`)
- Set the message validation function at node startup, fixing
  https://gitlab.com/tezos/tezos/-/issues/7629. (MR :gl:`!15830`)
- Added a new RPC ``GET /p2p/gossipsub/mesh/`` that returns the GossipSub mesh
  (i.e. full data connections per topic) of a peer. (MRs :gl:`!16754`,
  :gl:`!16775`)

Miscellaneous
-------------
