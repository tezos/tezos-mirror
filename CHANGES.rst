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

- For protocol Seoul and later, The
  ``helpers/scripts/normalize_stack`` and
  ``helpers/scripts/normalize_data`` Michelson RPCs now accept
  ill-typed inputs. To test if some Michelson value is well-typed or
  not, the ``helpers/scripts/typecheck_data`` Michelson RPC can still
  be used. (MR :gl:`!20297`)

- Fixed the external validator process being unnecessarily restarted when a
  bootstrap pipeline is canceled (e.g. due to peer disconnection). Also improved
  canceled error matching to handle all forms of ``Lwt.Canceled`` errors,
  including serialized forms from IPC round-trips. (MR :gl:`!20903`)

- Fixed node shutdown ordering so the node shuts down before RPC servers,
  avoiding EPIPE errors and slow shutdown when many external RPC processes are
  running. (MR :gl:`!20476`)

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

- **Breaking change**: Services (octez-node, octez-baker, octez-dal-node) are
  now gracefully stopped during package upgrade and NOT automatically restarted.
  Users must manually restart services after upgrade with
  ``systemctl start octez-node`` (and similar). (MR :gl:`!19996`)

Smart Rollup node
-----------------

- Delayed the start of a refutation game by one commitment period (approximately
  15 minutes) to prevent some edge cases in dispute resolution. (MR
  :gl:`!20674`)

- Added ``max_batch_length`` configuration option in the injector
  settings to limit the maximum number of operations included in a
  single L1 batch. This allows operators to control batch length
  independently of the operation size limit, which can help avoid gas
  quota exceeded errors when multiple operations of the same type
  would consume too much gas when batched together. Configure via
  ``injector.max_batch_length`` in the rollup node configuration file.
  (MR :gl:`!20670`)

- Remove protocol plugin for Nairobi (17). This still allows to replay Etherlink
  mainnet from genesis but not Etherlink testnet. (MR :gl:`!20301`)

- Fix the status of traces for our workers’ handlers remaining unset. (MR
  :gl:`!20379`)

- The rollup node only monitors finalized L1 heads when used on a RISC-V
  rollup. (MR :gl:`!20448`)

Smart Rollup WASM Debugger
--------------------------

Data Availability Layer (DAL)
-----------------------------

DAL node
~~~~~~~~
- Option ``--slots-backup-uri`` now supports shard archives in addition to slot
  archives. Use URI fragments #shards or #slots (default if omitted). (MR :gl:`!19293`)

- Dal node now respect the exit code documentation on unhandled errors
  and exit with code 123. (MR :gl:`!20584`)

- Dal node exit with code 1 on configuration file handling error. (MR :gl:`!20584`)

- ``octez-dal-node config init`` now fails if a configuration file exists. new
  command ``octez-dal-node config reset`` should be used instead. (MR
  :gl:`!20584`)

- ``octez-dal-node config update`` now fails if no configuration file exists. (MR
  :gl:`!20584`)


Miscellaneous
-------------
