.. _version-8:

Version 8.2
===========

Version 8.0 contains a new version (V1) of the protocol
environment, which is the set of functions that protocols can call. Up
to Delphi, all protocols used protocol environment V0. The new version
(V1) is used by Edo, which is a proposal for the next protocol after
Delphi. The release candidate also contains Edo itself as well as its
daemons (baker, endorser and accuser) so that you can test it easily.

We have also spawned a test network for Edo, named Edonet, that
replaces Ebetanet, which was a test network for a beta version of
Edo. The release candidate contains the necessary configuration to
join Edonet: just configure your node with
``tezos-node config init --network edonet``.

Version 8.1 fixes a performance regression related to operations
involving ``tz3`` addresses and several compilation problems in
some contexts.

Version 8.2 replaces `PtEdoTez` by `PtEdo2Zk` and provides RPCs to
"normalize" Michelson expressions returned by the Edo protocol along
with constraining the size of p2p messages at low level and updating
some external dependencies.

Update Instructions
-------------------

Starting from version 8.0, compiling Tezos requires the Rust compiler,
version 1.44.0, and the Cargo package manager to be installed.
See :ref:`instructions to set up Rust<setup_rust>`.

To update from sources::

  git fetch
  git checkout v8.2
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v8.2`` Docker images of Tezos.

Changelog — Version 8.2
-----------------------

Node
~~~~

- Override `PtEdoTez` activation by `PtEdo2Zk` in mainnet network.

- Make size limits on p2p messages explicit in low-level encodings.

- Add new RPCs for Edo: `helpers/scripts/normalize_{data,script,type}`
  and a `XXX/normalized` variant to each protocol RPC `XXX`
  outputting Michelson expressions.

Baker / Endorser / Accuser
~~~~~~~~~~~~~~~~~~~~~~~~~~

- Replace `PtEdoTez` by `PtEdo2Zk`.

Miscellaneous
~~~~~~~~~~~~~

- Update external opam dependencies. In particular, switch to
  `hacl-star.0.3.0-1` which performs better.

Changelog — Version 8.1
-----------------------

Node
~~~~

- Fix a performance regression affecting serialization of tz3
  signatures by reverting the P256 implementation to ``uecc``.

- Fixup allowing nodes in ``--history-mode full`` to answer to all new
  messages to the distributed database protocol.

Client
~~~~~~

- As a consequence of moving back to ``uecc``, revert for now the
  ability to sign with tz3 addresses.

Miscellaneous
~~~~~~~~~~~~~

- Allow building from sources with older version of git (used to
  require 2.18)

- Downgrade ``mirage-crypto`` dependency to avoid failure on startup
  with ``illegal instruction`` on some hardware.


Changelog — Version 8.0
-----------------------

Node
~~~~

- Added two new bootstrap peers for Mainnet and one for Edonet.

- Fixes a bug where any event would allocate more memory than needed
  when it were not to be printed.

- Improved how the node stores buffered messages from peers to consume less memory.

- Enforce loading of non-embedded protocols before starting the node
  allowing the prevalidator to start correctly.

- Optimized the I/O and CPU usage by removing an unnecessary access to
  the context during block validation.

Docker Images
~~~~~~~~~~~~~

- Bump up base image to ``alpine:12``. In particular, it changes rust and python
  versions to 1.44.0 and 3.8.5 respectively.

Changelog — Version 8.0~rc2
---------------------------

Node
~~~~

- Added a new version (version 1) of the protocol environment.
  The environment is the set of functions and types that the protocol can use.
  Protocols up to Delphi used environment version 0.
  The Edo protocol uses environment version 1.

- Added the Edo protocol: the node, client and codec now comes linked with Edo,
  and the Edo daemons (baker, endorser and accuser) are available.

- Added a built-in configuration for Edonet, a test network that runs Edo.
  You can configure your node to use this test network with ``--network edonet``.

- Removed the built-in configuration for Carthagenet, which ends its life on
  December 12th 2020. You can no longer configure your node with ``--network carthagenet``.

- Snapshots exported by a node using version 8 cannot be imported by a
  node running version 7. This is because the new snapshots contain
  additional information required by protocol Edo. On the other hand,
  snapshots exported by a node using version 7 can be imported by a
  node running version 8.

- The bootstrap pipeline no longer tries to concurrently download
  steps from other peers. The result is actually a more efficient
  bootstrap, because those concurrent downloads resulted in multiple
  attempts to download the same block headers. It
  also resulted in more memory usage than necessary.

- Added six messages to the distributed database protocol and bumped
  its version from 0 to 1. These new messages allow to request for: a
  peer's checkpoint, the branch of a given protocol and a block's
  predecessor for a given offset. These messages are not yet used but
  will be useful for future optimizations.

- You can now specify the data directory using environment variable ``TEZOS_NODE_DIR``.
  If you both set this environment variable and specify ``--data-dir``,
  the latter will be used.

- Added new RPC ``/config`` to query the configuration of a node.

- Changed signal handling and exit codes for most binaries. The codes'
  significance are detailed in [the user documentation](http://tezos.gitlab.io/user/various.html#tezos_binaries_signals_and_exit_codes).

- Command ``tezos-node --version`` now exits with exit code 0 instead of 1.

- Fixed the synchronisation threshold which was wrongly capped with an
  upper bound of 2 instead of a lower bound of 2 when ``--connections``
  was explicitely specified while the synchronisation threshold itself
  was not specified.

Client
~~~~~~

- Added client command ``import keys from mnemonic``, which allows to
  import a key from a mnemonic following the BIP39 standard.

- When the client asks for a password, it no longer tries to hide its
  input if the client was not run from a terminal, which allows for
  use in a script.

- You can now specify the base directory using environment variable ``TEZOS_CLIENT_DIR``.
  If you both set this environment variable and specify ``--base-dir``,
  the latter will be used.

- Fixed command ``set delegate for <SRC> to <DLGT>`` to accept public key hashes for
  the ``<DLGT>`` field.

- Fixed the ``rpc`` command that did not use the full path of the URL provided
  to ``--endpoint``. Before this, ``--endpoint http://localhost:8732/node/rpc``
  would have been equivalent to ``--endpoint http://localhost:8732``.

- Fixed an issue where the client would try to sign with a key for which
  the private counterpart was unknown even though a remote signer was connected.

Baker / Endorser / Accuser
~~~~~~~~~~~~~~~~~~~~~~~~~~

- Fixed a crash (assertion error) that could happen at exit,
  in particular if a baker were connected.

Docker Images
~~~~~~~~~~~~~

- Docker images are now available for arm64. Image tags stay the same
  but now refer to "multi-arch" manifests.

Changelog — Version 8.0~rc1
---------------------------

Node
~~~~

- Fixed some cases where the node would not stop when interrupted with Ctrl+C.

- The node's mempool relies on a new synchronisation heuristic. The
  node's behaviour, especially at startup, may differ slightly; log
  messages in particular are likely to be different. More information
  is available in the whitedoc.

- The new synchronisation heuristic emits an event when the
  synchronisation status changes. This can be used to detect when the
  chain is stuck for example. More information is available in the
  whitedoc.

- Node option ``--bootstrap-threshold`` is now deprecated and may be
  removed starting from version 9.0. Use ``--synchronisation-threshold``
  instead.

- Fixed an issue which prevented using ports higher than 32767 in
  the client configuration file.

- The ``tezos-node run`` command now automatically generates an identity file as if
  you had run ``tezos-node identity generate`` if its data directory contains
  no identity file.

- Improved various log messages and errors.

- When bootstrapping, do not greylist peers in rolling mode whose oldest known
  block is newer than our head.

- Made the timestamp in log messages more precise (added milliseconds).

- Fixed encoding of P2P header message length for larger lengths.

- Added ``-d`` as a short-hand for the ``--data-dir`` option of the node.

- Added a built-in activator key for the built-in sandbox network.
  This allows to spawn a sandbox without the need for a custom genesis protocol.

- Greylist the identity and address of peers that send malformed messages.

- Fixed some cases where the context was not closed properly when terminating a node
  or if the baker failed to bake a block.

- Removed the "get operation hashes" and "operation hashes" messages of the
  distributed database protocol. Those messages were never used.

- Reduced the amount of log messages being kept in memory (that can be queried
  using RPCs) before they are discarded to reduce the total memory footprint.

- Fixed a case where the ``/workers/prevalidator`` RPC could fail
  if there were too many workers.

- Fixed how protocol errors are displayed.
  Before, there were printed using the cryptic ``consequence of bad union`` message.

- Pruned blocks can now be queried using RPC ``/chains/<chain>/blocks/<block>``.
  The ``metadata`` field will be empty in the response, leaving only the header.

- Fixed handling of pre-epoch timestamps, in particular in RPCs.

- Time is now output with millisecond precision when calling RPCs.

- Fixed the ``/chains/<chain>/blocks`` RPC which sometimes did not return all blocks.

- Improved the performance of the progress indicator when importing snapshots.

- Improved performance of ``tezos-node snapshot export``.

- Fixed the node which sent too many "get current branch" messages to its peers
  on testchain activation.

Client
~~~~~~

- The ``tezos-client config show`` command now takes into account
  the command line arguments.

- Fixed an issue which caused ``tezos-client rpc get /errors``
  as well as ``tezos-codec dump encodings`` to fail because of duplicate encodings.
  As a result, some protocol encodings whose name was not prefixed by the protocol name
  are now prefixed by it. If you have tools which rely on encoding names you may have
  to update them.

- Added client command ``multiple transfers from <src> using <transfers.json>``
  to perform multiple operations from the same address in a single command.

- Added option ``--endpoint`` to client and bakers.
  It replaces options ``--addr``, ``--port`` and ``--tls`` which are now deprecated.

- Added command ``rpc patch`` to the client, to perform RPCs using the PATCH
  HTTP method.

- Make the client emit a more human-readable error if it failed to understand
  an error from the node.

- Added client commands ``tezos-client convert script <script> from <input> to <output>``
  and ``tezos-client convert data <data> from <input> to <output>``
  to convert to and from michelson, JSON, binary and OCaml with type-checking.

- The client now retries commands a few times if the node is not yet ready.

- Added client command ``compute chain id from block hash <hash>``
  and ``compute chain id from seed <seed>`` to compute the chain id corresponding
  to, respectively, a block hash or a seed.

- Added the verbose-signing switch to a number of multisig commands.

- The ``prepare multisig`` commands now display the Blake 2B hash.

- Some client commands which use the default zero key ``tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU``
  in dry runs now display this key using an informative string
  ``the baker who will include this operation`` instead of the key itself.

- Fixed an error which occurred in the client when several keys had the same alias.

- Added support for some ``rpc {get,post,...}`` commands in the client's mockup mode.

- Added ``--mode mockup`` flag to ``config init`` for the client's mockup mode,
  that writes the mockup's current configuration to files.

- Added ``--mode mockup`` flag to ``config show`` for the client's mockup mode,
  that prints the mockup's current configuration to standard output.

- Added arguments ``--bootstrap-accounts`` and ``--protocol-constants``
  to the client's ``create mockup`` command. ``--bootstrap-accounts`` allows
  changing the client's bootstrap accounts and ``--protocol-constants`` allows
  overriding some of the protocol's constants.
  Use commands ``config {show,init} mockup`` (on an existing mockup)
  to see the expected format of these arguments.

- The client no longer creates the base directory by default in mockup mode.

- Fixed the argument ``--password-filename`` option which was ignored if
  it was present in the configuration file.

Baker / Endorser / Accuser
~~~~~~~~~~~~~~~~~~~~~~~~~~

- The baker now automatically tries to bake again in case it failed.
  It retries at most 5 times.

- The baker now outputs an explicit error when it loses connection with the node.

- Added command-line option ``--keep-alive`` for the baker.
  It causes the baker to attempt to reconnect automatically if it loses connection
  with the node.

Protocol Compiler And Environment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Prepare the addition of SHA-3 and Keccak-256 cryptographic primitives.

- Prepare the introduction of the new protocol environment for protocol 008.

- The protocol compiler now rejects protocols for which the OCaml
  compiler emits warnings.

Codec
~~~~~

- Fixed ``tezos-codec dump encodings`` which failed due to two encodings having
  the same name.
