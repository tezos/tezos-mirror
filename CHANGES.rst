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

- **Breaking Changes**: Improved a few lib_shell logs in default level by
  shortening the display to completion time only instead of the full status of
  the operation.

- Added an option ``daily-logs`` to file-descriptor sinks, enabling
  log rotation based on a daily frequency.

- Fixed a bug while reconstructing the storage after a snapshot import
  that would result in wrong context hash mapping for some blocks.

- **Breaking Change**: disabled snapshot export support for storage
  that was created with Octez v13 (or earlier).

- Fixed a bug raising an error when a context split was called on a
  context that was created with Octez v13 (or earlier).

- Deprecated the RPC ``GET /monitor/valid_blocks`` and introduced
  ``GET /monitor/validated_blocks`` and ``GET /monitor/applied_blocks``
  which respectively returns validated blocks, which are not yet applied
  nor stored, and applied blocks which are fully applied and stored by
  the node. (MR :gl: `!7513`)

- Replaced some "precheck" occurrences with "validate" in event and
  error identifiers and messages. (MR :gl: `!7513`)

Client
------

Baker
-----

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

Rollups
-------

Miscellaneous
-------------
