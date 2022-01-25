Changelog
'''''''''

This file lists the changes added to each version of tezos-node,
tezos-client, and the other Tezos binaries. The changes to the Tezos
protocol are documented in the ``docs/protocols/`` directory; in
particular in ``docs/protocols/alpha.rst``.

Development Version
===================

When you make a commit on master, you can add an item in one of the
following subsections (node, client, …) to document your commit or the
set of related commits. This will ensure that this change is not
forgotten in the final changelog. By having your commits update this
file you also make it easy to find the commits which are related to your
changes using ``git blame``.

Only describe changes which affect users (bug fixes and new features),
not refactorings or tests. Changes to the documentation do not need to
be documented here either.

Node
----

- Added optional parameter ``--media-type`` and its corresponding field
  in the configuration file. It defines which format of data serialisation
  must be used for RPC requests to the node. The value can be  ``json``,
  ``binary`` or ``any``. By default, the value is set to ``any``.

- Added an option ``--listen-prometheus <PORT>`` to ``tezos-node run`` to
  expose some metrics using the Prometheus format.

- Fixed missing removal of replaced operation in the plugin when another better
  one takes its place (when the mempool is full).

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
