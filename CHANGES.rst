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

- Added Jakarta, a protocol proposal for Mainnet featuring, among others,
  Transaction Optimistic Rollups, Tickets Hardening and Liquidity Baking Toggle.

- **Breaking change**:
  Restored the encoding of events corresponding to "completed
  requests" (block validation, head switch, ...) to pre v11. They only
  contain absolute timestamps.

- Add optional query parameters ``applied``, ``refused``, ``outdated``,
  ``branch_refused``, and ``branch_delayed`` to RPC
  ``GET /chains/main/mempool/pending_operations``.
  These new parameters filter the operations returned based on their
  classifications. If no option is given, all
  the parameters are assumed to be ``true``, making this extension
  backward-compatible (i.e. all operations are returned).

- Added optional parameter ``--media-type`` and its corresponding field
  in the configuration file. It defines which format of data serialisation
  must be used for RPC requests to the node. The value can be  ``json``,
  ``binary`` or ``any``. By default, the value is set to ``any``.

- Added an option ``--metrics-addr <ADDR>:<PORT>`` to ``tezos-node`` to
  expose some metrics using the Prometheus format.

- Adds ``tezos-node storage head-commmit`` command to print the current
  context head commit hash to stdout.

- Added a history mode check when importing a snapshot to ensure the consistency between the
  history mode of the snapshot and the one stored in the targeted data
  directory configuration file.

- Fixed a wrong behavior that could cause the savepoint to be dragged
  too early.

- Fixed a memory leak where some operations were not cleaned up. This problem
  occurred occasionally, when during the fetching the operation of some block,
  the node changed his head.

- The node context storage format was upgraded. To this end, a new storage
  version was introduced: 0.0.8 (previously 0.0.7). Backward compatibility is
  preserved: upgrading from 0.0.7 to
  0.0.8 is done automatically by the node the first time you run it. This
  upgrade is instantaneous. However, be careful that there is no forward
  compatibility: previous versions of Octez
  will refuse to run on a data directory which was used with Octez 13.0.

- Validation errors are flatter. Instead of the ``economic_protocol_error``
  carrying a field ``trace`` with the relevant economic-protocol errors, the
  relevant economic-protocol errors are included in the main trace itself.

- Exported snapshots now have version number 4 (previously 3).
  Snapshots of version 2 and 3 exported with previous versions of
  Octez can still be imported. Snapshots of version 4 cannot be
  imported with Octez prior to version 13.0.

- Added optional query parameter ``force_metadata`` to the ``GET
  /chains/<chain>/blocks/<block>/`` and ``GET
  /chains/<chain>/blocks/<block>/operations/`` RPCs. Passing this
  parameter forces the re-computation of operation metadata that were
  considered as too large, even if they exceed the configured limit. The
  re-computed metadata are not stored on disk after this call, they are
  just returned.

- Added ``--progress-display-mode`` option to the ``tezos-node`` commands
  that display progress animation. This option allows to redirect progress
  animation to non-TTY file descriptors.

- Added the ``jakartanet`` built-in network alias.
  You can now configure your node with ``--network jakartanet`` to run the
  Jakartanet test network.

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

Baker / Endorser / Accuser
--------------------------

- The ``--liquidity-baking-escape-vote`` command-line has been renamed
  to ``--liquidity-baking-toggle-vote``.

- The ``--liquidity-baking-toggle-vote`` command-line option is made
  mandatory. The ``--votefile`` option can still be used to change
  vote without restarting the baker daemon, if both options are
  provided ``--votefile`` takes precedence and
  ``--liquidity-baking-toggle-vote`` is only used to define the
  default behavior of the daemon when an error occurs while reading
  the vote file.

- The format of the vote file provided by the ``--votefile`` option
  has changed too; the ``liquidity_baking_escape_vote`` key is renamed
  to ``liquidity_baking_toggle_vote`` and the value must now be one of
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
  one. Option ``--password-file`` which actually was a complete no-op
  has been removed.

- Added support for Ledger Nano S plus devices

Proxy server
------------

- A new ``--data-dir`` option was added. It expects the path of the
  data-dir of the node from which to obtain data. This option greatly
  reduces the number of RPCs between the proxy server and the node, thus
  reducing the IO consumption of the node.

Protocol Compiler And Environment
---------------------------------

Codec
-----

- Added command ``slice`` which splits a binary, hex-encoded blob into its
  different constituents. This command is useful to understand what a binary message means.

Docker Images
-------------

- Script ``tezos_docker_manager.sh`` (also known as ``mainnet.sh``) is now deprecated.
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
