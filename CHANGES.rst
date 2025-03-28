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

- Added RPC ``POST /bls/aggregate_public_keys`` to aggregate BLS
  public keys. (MR :gl:`!17461`)

- Added RPC ``POST /bls/aggregate_signatures`` to aggregate BLS
  signatures. (MR :gl:`!17461`)

- Added RPC ``POST /bls/check_proof`` to check a BLS proof. (MR
  :gl:`!17461`)

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

Signer
------

- Add a ``--allow-list-known-keys`` argument at signer launch to allow client to
  ask for the signer list of known public key hashes. The signer return ``List
  known keys request not allowed.`` otherwise. (MR :gl:`!17403`)

Baker
-----

Agnostic Baker
--------------

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

Proxy Server
------------

Protocol Compiler And Environment
---------------------------------

- Environment V15 uses signature V2. This change impacts the way BLS signatures
  are handled. In previous environments that used signature V1, the BLS
  signatures were expected to be produced with the ``Augmented`` cryptographic
  scheme. Starting from V15, they are expected to be produced with the ``Proof
  of possession`` cryptographic scheme. (MR :gl:`!17036`)

Codec
-----

Docker Images
-------------

Smart Rollup node
-----------------

- Add query parameter ``outbox_level`` for RPCs
  ``/local/outbox/pending/executable`` and
  ``/local/outbox/pending/unexecutable``. (MR :gl:`!16831`)

- Add a new RPC ``/local/outbox/pending`` to fetch all known outbox messages
  with their status. (MR :gl:`!16831`)

- Add a CLI argument ``--config-file`` to allow specifying a configuration file
  outside the data directory. (MR :gl:`!17225`)

- Allow to provide a remote URL for downloading snapshots in commands ``snapshot
  info`` and ``snapshot import``. (MRs :gl:`!17407`, :gl:`!17420`)

- Display logging levels in logs by default. (MR :gl:`!17479`)

Smart Rollup WASM Debugger
--------------------------

Data Availability Layer (DAL)
-----------------------------

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

- The DAL node supports a ``config update`` command to update an
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
- A warning is emitted when registering a public key hash (as an attester
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

Protocol
~~~~~~~~

Miscellaneous
-------------

- Revert Renamed ``Bls`` file from the crypto library in ``Bls_aug.ml``. (MR :gl:`!17051`).
- Grafazos: fix netdata metrics used for hardware monitoring, and add more flexibility
  over the mountpoint allowing to observe only / and /opt mountpoints if needed . Also,
  fix the network IOs panel presentation, avoiding a grafana panel transformation.
- Grafazos: add a filter on the selected ``node_instance`` variable over all metrics (was
  previously showing data from all sources on some panels even when a specific source had
  been selected in the grafana dashboard's variable)
