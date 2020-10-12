.. _version-8:

Version 8.0~rc1
===============

Version 8.0~rc1 is the first release candidate of version 8.0.
If it proves to be stable, it will become the next major release.

Update Instructions
-------------------

To update from sources::

  git fetch
  git checkout v8.0-rc1
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v8.0-rc1`` Docker images of Tezos.

Changelog — Version 8.0
-----------------------

Node
~~~~

- Fixed all known cases where the node would not stop when interrupted with Ctrl+C.

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
