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

Baker
-----

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

Data Availability Committee (DAC)
---------------------------------

Miscellaneous
-------------

- **Breaking change** Switch encoding of ``nread_total`` field of
  ``P2p_events.read_fd`` in Octez-p2p library to ``Data_encoding.int64`` to fix an
  overflow.
