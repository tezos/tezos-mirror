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

- Introduced a new process, forked by the node, that is responsible of
  managing the RPC server: the RPC-process. It is used by default by
  the node.

- Introduced a new ``--local-rpc-addr`` that starts the RPC server
  locally, not using the dedicated RPC-process.

Client
------

- Extended the support for the TZT format when using the ``run unit
  tests`` client command. (MR :gl:`!4474`)

- The ``timelock create`` command now takes the message to lock in hexadecimal format. (MR :gl:`!11597`)


Baker
-----

- Added optional ``--pre-emptive-forge-time t`` argument that, when
  set, will cause the baker to pre-emptively forge its block if
  the current level quorum has been reached, and it is the round 0
  proposer of the next level. The amount of time to wait before forging
  is ``round_time - t``. This optimization increases the chance for the
  proposed block to reach quorum by slightly reducing the operation
  inclusion window. Note that a ``t`` value that is too high could
  cause forging to begin too early and result in lower baking rewards.
  If not given, defaults to ``0.15 * block_time``. Set to ``0`` to
  ignore. (MR :gl:`!10712`)

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

- Added RPC ``/describe?recurse=true`` to retrieve schema of RPCs for the rollup
  node. (MR :gl:`!10118`)

- Added RPC ``/openapi?protocol={ProtoHash}`` to retrieve the OpenAPI
  specification for RPCs of the rollup node. (MR :gl:`!10118`)

- Introduced a new command ``generate openapi``, to generate the OpenAPI JSON
  specification and output it to stdout. (MR :gl:`!10118`)

- Register in ``octez-codec`` some of the protocol smart rollup
  related encodings. (MRs :gl:`!10174`, :gl:`!11200`)

- Fix a critical bug that could lead to data loss when chain
  reorganizations happen while a GC is running. (MR :gl:`!11358`)

- Snapshot inspection command. (MR :gl:`!11456`)

- Snapshot export options. (MRs :gl:`!10812`, :gl:`!11078`, :gl:`!11256`,
  :gl:`!11454`)

- Snapshot import. (MRs :gl:`!10803`)

Smart Rollup WASM Debugger
--------------------------

Data Availability Committee (DAC)
---------------------------------

Miscellaneous
-------------

- **Breaking change** Switch encoding of ``nread_total`` field of
  ``P2p_events.read_fd`` in Octez-p2p library to ``Data_encoding.int64`` to fix an
  overflow.
