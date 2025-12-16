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
  resources. (MR :gl:`!19990`)

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

Smart Rollup node
-----------------

- Fix issue where setting for ``l1_monitor_finalized`` would be ignored from the
  configuration file. (MR :gl:`!20239`)
- Fix issue where finalized blocks would not be notified when using
  ``--l1-monitor-finalized``. (MR :gl:`!20256`)

Smart Rollup WASM Debugger
--------------------------

Data Availability Layer (DAL)
-----------------------------

DAL node
~~~~~~~~

Miscellaneous
-------------
