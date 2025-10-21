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

- **Deprecation** The ``adaptive-issuance-vote`` argument (placeholder
  ``vote``) is now deprecated, and will be removed in the next major
  version of Octez. It was meant to decide the activation of the
  Adaptive Issuance feature, and has had no effects since the Paris
  protocol has been voted in. (MR :gl:`!19215`)

- **Deprecation** The ``octez-baker-<protocol>`` binaries are
   deprecated, and will be removed in the next major version of
   Octez. Please use ``octez-baker`` instead, which automatically
   handles protocol switches. (MR :gl:`!19641`)


Accuser
-------

- **Deprecation** The ``octez-accuser-<protocol>`` binaries are
   deprecated, and will be removed in the next major version of
   Octez. Please use ``octez-accuser`` instead, which automatically
   handles protocol switches. (MR :gl:`!19641`)


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

- Update opentelemetry library to 0.12 which should fix the issue where a log
  protobuf encoding crashes the node when telemetry is activated. (MR
  :gl:`!19516`)


Smart Rollup WASM Debugger
--------------------------

Data Availability Layer (DAL)
-----------------------------

DAL node
~~~~~~~~

- Added RPC ``GET /profiles/{pkh}/monitor/attestable_slots`` to open a monitoring
  stream that emits a JSON ``slot_id`` each time a slot becomes attestable for the
  given public key hash (``pkh``). A slot id is emitted when all shards assigned to
  that ``pkh`` at the corresponding attestation level are available in the DAL
  node's store. If traps are detected within the slot, then it should not be attested,
  so the id is not sent via the stream. (MR :gl:`!19459`)

- The DAL node now starts propagating shards one level after the inclusion of the
  corresponding published slot header operation (i.e., when the operation is finalized),
  instead of two levels after, when the block is finalized. (MR :gl:`!19366`)

- **Breaking change** Slots status are not stored in dedicated files on disk
  anymore, but found in a cache and the skip list. A consequence of this is that
  the ``/levels/<slot_level>/slots/<slot_index>/status`` will only work with nodes that store the
  skip list, and therefore not with observer nodes. Also, the RPC will now answer
  a 500 error if querying a level at which the DAL was not supported instead
  of a 404 error. (MR :gl:`!19471`)

- **Breaking change** Enforced stricter validation for the JSON configuration
  file. Previously, the parser would silently ignore any content that appeared
  after the first valid JSON object. Now, any extraneous data will cause the
  function to return an error. (MR :gl:`!18745`)


Miscellaneous
-------------
