Development Changelog
'''''''''''''''''''''

**NB:** The changelog for releases can be found at: https://tezos.gitlab.io/CHANGES.html


This file lists the changes added to each version of tezos-node,
tezos-client, and the other Octez executables. The changes to the economic
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

Node
----

- Add a `/chains/<chain>/blocks/<block>/merkle_tree_v2` RPC. This is an
  evolution of the `../merkle_tree` RPC, using a simpler implementation of the
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
  usage where, in the past, it could take up to several gigabytes of
  memory to perform a store merge. It also takes less time to perform
  a merge and shouldn't impact normal node operations as much as it
  previously did; especially on light architectures.

- Added support for ``level..level`` range parameters in the replay command.

- Breaking change: Node events using a legacy logging system and are migrated to
  the actual one. Impacted events are in the following sections:
  ``validator.chain``, ``validator.peer``, ``prevalidator`` and
  ``validator.block``. Section ``node.chain_validator`` is merged into
  ``validator.chain`` for consistency reasons. Those events see their JSON
  reprensentation shorter, with no duplicated information. e.g.
  ``validator.peer`` events were named ``validator.peer.v0`` at top-level and
  had an ``event`` field with a ``name`` field containing the actual event name,
  for example ``validating_new_branch``. Now, the event is called
  ``validating_new_branch.v0`` at top-level and contains a ``section`` field
  with ``validator`` and ``peer``.

*  Added support for ``--strict`` mode in the replay command: it causes the
   command to be less permissive.

- Added garbage collection for the context part of the storage
  backend.  It is activated by default for all nodes running with a
  full or rolling history mode.

- **Breaking change**: The node context storage format was
  upgraded. To this end, a new storage version was introduced: 2.0
  (previously 1.0). Backward compatibility is preserved: upgrading
  from 1.0 to 2.0 is done automatically by the node the first time you
  run it. This upgrade is instantaneous. However, be careful that
  there is no forward compatibility: previous versions of Octez will
  refuse to run on a data directory which was running with this
  storage version.

- The ``config`` and ``identity`` node commands no longer try to
  update the data directory version (``version.json``).

- Fixed a bug in the store that was generating an incorrect protocol
  table during a branch switch containing a user activated protocol
  upgrade.

- Decreased, from 5 to 1, the default number of additional cycles to
  keep in both ``Full`` and ``Rolling`` history modes. As a
  consequence, the storage footprint will be lowered and only the last
  6 cycles will be available (10 previously).

- Removed Giganode from the list of bootstrap peers for Mainnet.

- Removed the ``--network hangzhounet`` and ``--network jakartanet``
  built-in network aliases.

- Add third user-activated upgrade to the ``--network ghostnet`` built-in
  network alias (at level 1191936 for Kathmandu).

Client
------

- The light client (`tezos-client --mode light`) now uses the
  `../block/<block_id>/merkle_tree_v2` RPC introduced in this version, removing
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

- Improved performance by 50% of Ledger's signing requests by caching
  redundant requests.

Accuser
-------

Signer
------

Proxy Server
------------

Protocol Compiler And Environment
---------------------------------

Codec
-----

Docker Images
-------------

-  Bump up base image to ``alpine:3.16``. In particular, it changes Rust
   and Python versions to 1.60.0 and 3.10.5 respectively.

Rollups
-------

Miscellaneous
-------------

-  Recommend rust version 1.60.0 instead of 1.52.1.

-  Removed delegates for protocols Ithaca and Jakarta.
