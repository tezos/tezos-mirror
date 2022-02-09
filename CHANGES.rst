Development Changelog
'''''''''''''''''''''

**NB:** The changelog for releases can be found at: https://tezos.gitlab.io/CHANGES.html


This file lists the changes added to each version of tezos-node,
tezos-client, and the other Octez executables. The changes to the economic
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

- Add optional query parameters ``applied``, ``refused``, ``outdated``,
  ``branch_refused``, and ``branch_delayed`` to RPC
  ``GET /chains/main/mempool/pending_operations``.
  These new parameters indicate the classifications for which the RPC should
  or shouldn't return the corresponding operations. If no option is given, all
  the parameters are assumed to be ``true``, making this extension
  backward-compatible (i.e. and all operations are returned).

- The tezos-node configuration file parameter
  ``shell.prevalidator.limits.max_refused_operations`` is now
  deprecated and may be removed starting from version 13.0.

- Added optional parameter ``--media-type`` and its corresponding field
  in the configuration file. It defines which format of data serialisation
  must be used for RPC requests to the node. The value can be  ``json``,
  ``binary`` or ``any``. By default, the value is set to ``any``.

- Added an option ``--listen-prometheus <PORT>`` to ``tezos-node run`` to
  expose some metrics using the Prometheus format.

- Fixed missing removal of replaced operation in the plugin when another better
  one takes its place (when the mempool is full).

- The output of ``tezos-client get ledger high watermark for <ledger>``
  now also displays the high-water mark for the round, if available.
  Rounds are introduced in Tenderbake.

- Adds ``tezos-node storage head-commmit`` command to print the current
  context head commit hash to stdout.

Client
------

- A new ``--force`` option was added to the ``transfer`` command. It
  makes the client inject the transaction in a node even if the
  simulation of the transaction fails.

- A new ``--self-address`` option was added to the ``run script``
  command. It makes the given address be considered the address of
  the contract being run. The address must actually exist in the
  context. If ``--balance`` wasn't specified, the script also
  inherits the given contract's balance.

Baker / Endorser / Accuser
--------------------------

Signer
------

- Added global option ``--password-filename`` which acts as the client
  one. Option ``--password-file`` which actually was a complete no-op
  has been removed.

Proxy server
------------

Protocol Compiler And Environment
---------------------------------

Codec
-----

Docker Images
-------------

Miscellaneous
-------------
