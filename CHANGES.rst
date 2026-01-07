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

- Hardened ``lib_bees`` worker lifecycle for OCaml 5.x Eio domains: switched
  the worker registry to Saturn lock-free tables, serialized worker creation
  with mutex protection, added retries under resource pressure, ensured worker
  launch/initialization runs on the Event_loop main switch, and exposed clean
  shutdown for tests. Improves reliability under domain contention and low
  resources. (MR :gl:`!19990`) (MR :gl:`!20258`) (MR :gl:`!20261`)

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

- **Breaking change**: Debian/Ubuntu packages now install services as disabled
  by default. Users must explicitly enable services with
  ``systemctl enable octez-node`` (and similar for other services) before
  starting them. This prevents accidental service starts during package
  installation or upgrades. (MR :gl:`!19996`)

- Octez-node service is now automatically restarted after package upgrade
  if it was running before the upgrade. If the service was stopped, it
  remains stopped. Note: octez-baker and octez-dal-node services are
  currently NOT automatically restarted after upgrade. (MR :gl:`!19996`)

Smart Rollup node
-----------------

- Remove protocol plugin for Nairobi (17). This still allows to replay Etherlink
  mainnet from genesis but not Etherlink testnet. (MR :gl:`!20301`)

Smart Rollup WASM Debugger
--------------------------

Data Availability Layer (DAL)
-----------------------------

DAL node
~~~~~~~~
- Option ``--slots-backup-uri`` now supports shard archives in addition to slot
  archives. Use URI fragments #shards or #slots (default if omitted). (MR :gl:`!19293`)

Miscellaneous
-------------
