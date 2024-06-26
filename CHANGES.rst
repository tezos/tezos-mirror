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

- Add and **experimental** switch to enable the use of the Brassaia context
  backend using ``TEZOS_CONTEXT_BACKEND=Brassaia`` environment variable. (MR :gl:`!13054`)

- Removed binaries for Oxford. (MR :gl:`!13795`)

Node
----

- Add an ``source`` argument to ``GET
  /chains/<chain>/mempool/pending_operations`` which allows operations
  to be filtered by source. (MR :gl:`!11278`)

- Add an RPC
  ``/chains/<chain>/blocks/<block>/context/smart_rollups/smart_rollup/<sr1...>/consumed_outputs/<outbox_level>``
  that returns the consumed output's indexes for the given outbox
  level. (MR :gl:`!12776`)

- Restart the block application/peer validation once when encountering a context
  error to mitigate the error crashing by replaying the application/validation
  before raising an uncaught failure (MR :gl:`!13398` and :gl:`!13437`)

- Importing a snapshot uses the configuration if it exists. It's
  useful when an archive snapshot contains some
  ``unsafe_pvm_patches``. The same ``unsafe_pvm_patches`` must be set
  in the export and import or the import will fail. (MR :gl:`!13397`)

- When available, add some colors to some event logs: (MR :gl:`!13610`)
   - node_is_ready is now Green
   - synchronization status changes are now Magenta
   - store merge start/end events are now Cyan
   - update to protocol table is now Blue

- Removed Oxford mempool plugin. (MR :gl:`!13798`)

- Remove support for deprecated version ``0`` for RPCs ``GET
  ../mempool/monitor_operations``, ``POST ../helpers/preapply/operations``,
  ``GET ../blocks/<block>``, ``GET ../blocks/<blocks>/metadata``. and ``GET
  ../blocks/<blocks>/operations``. (MR :gl:`!13449`)

- Remove support for deprecated version ``1`` for RPC ``GET
  ../mempool/pending_operations``. (MR :gl:`!13449`)

Client
------

- The ``--extra-big-maps`` and ``--other-contracts`` command-line
  parameters, which are used to specify contextual information in some
  Michelson-related commands, now allow file names as argument. (MR
  :gl:`!13311`)

- **Breaking change** Removed read-write commands specific to Oxford. (MR :gl:`!13799`)

Baker
-----

- When available, add some colors to some event logs: (MR :gl:`!13611`)
   - block_injected is now Blue
   - revealing_nonce is now Cyan

- Branch used in consensus operation is now the grandparent block instead of the
  parent block. This is done to avoid having consensus operation branched on
  block that are not part of the canonical chain anymore.(MR :gl:`!13619`)

Accuser
-------

- When available, add some colors to some event logs: (MR :gl:`!13611`)
   - double_attestation_detected and double_preattestation_detected is now
     Magenta
   - double_attestation_denounced and double_preattestation_denounced is now
     Blue

Proxy Server
------------

Protocol Compiler And Environment
---------------------------------

- Added a new version of the protocol environment (V13). (MR :gl:`!12966`)

Codec
-----

Docker Images
-------------

Smart Rollup node
-----------------

- Updated RPC ``/local/batcher/injection`` with a new query argument
  possibility. When the rpc contains ``"drop_duplicate": true`` then
  the batcher will drop the messages that were already injected with a
  previous RPC call.  If ``"drop_duplicate": false`` then the rollup
  node defaults to its the previous behavior, where messages are
  injected again, even if the exact same one was previously
  injected. By default ``"drop_duplicate": false``. (MR :gl:`!13165`)

- RPC ``/health`` now returns meaningful health related data to asses if the
  rollup node operates correctly. Old ``/health`` RPC is renamed to ``/ping``.
  (MR :gl:`!12940`)

- Use a local cache per game for intermediate states of dissections. (MR
  :gl:`!12899`)

- Introduce the 5th version of the WASM PVM, which defaults to a higher tick
  limits to delegate refutability to the kernels. (MR :gl:`!12999`)

- Trigger GC every 1000 blocks (instead of 100) by default to reduce CPU
  consumption. (MR :gl:`!13177`)

- Default history mode is now "full". (MR :gl:`!13178`)

- Allow to import archive snapshots in "full" rollup node. (MR :gl:`!13186`)

- Fix a bug in how commitments are computed after a protocol migration
  where the the commitment period changes. (MR :gl:`!13588`)

- Ensure penultimate commitment is published on snapshot export as a
  failsafe. (MR :gl:`!13544`)

- Include commitment publication information in snapshots. (MR :gl:`!13724`)

- Under-approximate publication level for cementation when it is missing. (MR
  :gl:`!13725`)

Smart Rollup WASM Debugger
--------------------------

Data Availability Committee (DAC)
---------------------------------

Data Availability Layer (DAL)
-----------------------------

DAL node
~~~~~~~~

The following RPCs have been removed:

- ``POST /commitments`` (MR :gl:`!12949`), use ``POST /slots`` instead,
- ``GET /commitments/<c>/proof`` (MR :gl:`!13080`), also use ``POST /slots`` instead,
- ``PATCH /commitments`` (MR :gl:`!12886`),
- ``PUT /commitments/<c>/shards`` (MR :gl:`!12949`),
- ``GET /levels/<int32>/headers`` (MR :gl:`!13044`),

The paths or method of the following RPCs have been updated:
- ``GET /commitments/<c>/slot`` is now ``GET /levels/<l>/slots/<i>/content``  (MR :gl:`!13075`),
- ``GET /levels/<l>/slot_indices/<i>/commitment`` is now ``GET /levels/<l>/slots/<i>/commitment``  (MR :gl:`!13046`),
- ``POST /pages/<p>/proof`` is now ``GET /levels/<l>/slots/<i>/pages/<p>/proof``  (MR :gl:`!13083`),
- ``GET /shard/<c>/<s>`` is now ``GET /levels/<l>/slots/<i>/shards/<s>/content`` (MR :gl:`!13095`),
- ``POST /slot`` is now ``POST /slots`` (MR :gl:`!12949`),
- ``GET /slot/pages/<c>`` is now ``GET /levels/<l>/slots/<i>/pages`` (MR :gl:`!12880`),
- ``GET /commitments/<c>/headers`` is now ``GET /levels/<l>/slots/<i>/status`` (MR :gl:`!13055`).

Miscellaneous
-------------
