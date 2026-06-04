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

Breaking changes and deprecated features should be prefixed with the
appropriate tag **Breaking change** or **Deprecation**, and also added
to the breaking changes page in
``docs/introduction/breaking_changes.rst``, section "Upcoming Octez
Release".


General
-------

Node
----

Client
------

Signer
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

Packaging
---------

Smart Rollup node
-----------------

Smart Rollup WASM Debugger
--------------------------

Data Availability Layer (DAL)
-----------------------------

DAL node
~~~~~~~~

- Decoupled the storage lifetimes of slot payloads and shards. Shards are now
  always retained for a single, profile-independent period (about 150 levels);
  slot payloads of slot indices in the node's operator profile are retained
  according to ``--history-mode``, which defaults to the new ``archive`` mode
  (keep slot payloads indefinitely). The previous ``--history-mode full`` value
  remains accepted as a synonym for ``archive``, and the configuration file is
  migrated to version 3. (MR :gl:`!21939`)

- Added a ``--ignore-l1-history-check`` flag to the ``run`` command, which
  downgrades the startup check that the L1 node retains enough block history
  from an error to a warning. This is intended for test networks where the
  operator does not control the L1 node and cannot recompile the DAL node. The
  flag is not honored on mainnet, where the check is always enforced. (MR
  :gl:`!21934`)

Miscellaneous
-------------
