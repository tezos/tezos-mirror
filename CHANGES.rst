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

Node
----

- Fixed a bug that caused snapshot import to ignore the data directory
  of the configuration file when the configuration file was specified
  from the command-line using ``--config-file``. Note that ``--data-dir``
  can still be used to override the data directory location from the
  configuration file, whether it is specified from the command-line or not.

- Fixed a bug that caused the ``snapshot import`` command to fail when
  used on data directories configured with an explicit number
  additional cycles.

- Fixed an issue that could left a temporary directory if a snapshot
  export was cancelled. Additionally, a cleanup now ensures the
  absence of leftovers temporary directories when exporting a
  snapshot.

- Fixed an issue that could left a lock file if a snapshot import was
  cancelled.

- **Breaking change**: the default ``?version`` of the ``pending_operations``
  RPC is now 1 instead of 0. Version 1 is more consistent as
  ``branch_delayed``/``branch_refused``/``outdated``/``refused`` operations are
  encoded like ``applied`` operations: the ``"hash"`` field is included in the
  object instead of being separate in an array. The same change applies to
  ``unprocessed`` operations, except that those do not contain the ``error``
  field. More details can be found by calling the
  ``describe/chains/main/mempool/pending_operations`` RPC. You can get back the
  previous encoding with ``?version=0`` but note that version 0 is now
  deprecated and may be removed starting from the next major release of Octez.
  (MR :gl:`!6783`)

- The ``pending_operations`` RPC can now be run in ``binary`` format when using
  version ``1``. (MR :gl:`!6783`)

- Removed the ``node_`` prefix in identifiers of the
  ``config_validation`` and ``config_file`` events and errors.

Client
------

- Added command to get contract's balance of ticket with specified ticketer, content type, and content. Can be used for both implicit and originated contracts.
  ``octez-client get ticket balance for <contract> with ticketer '<ticketer>' and type <type> and content <content>``. (MR :gl:`!6491`)

- Added command to get the complete list of tickets owned by a given contract by scanning the contract's storage. Can only be used for originated contracts.
  ``octez-client get all ticket balances for <contract>``. (MR :gl:`!6804`)

Baker
-----

Accuser
-------

Signer
------

Proxy Server
------------

- The proxy server can now serve endpoints about blocks of all known economic
  protocols instead of only one chosen at boot time.

Protocol Compiler And Environment
---------------------------------

Codec
-----

- Added the ``dump encoding <id>`` command to dump the description of a single
  registered encoding.

Docker Images
-------------

Rollups
-------

Miscellaneous
-------------
