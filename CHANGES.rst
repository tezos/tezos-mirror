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
