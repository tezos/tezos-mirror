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

Client
------

- Added command to get contract's balance of ticket with specified ticketer, content type, and content:
  ``octez-client get ticket balance for <contract> with ticketer '<ticketer>' and type <type> and content <content>``. (MR :gl:`!6491`)

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
