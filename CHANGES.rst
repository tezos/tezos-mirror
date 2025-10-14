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

- **Breaking change** Enforced stricter validation for the JSON configuration
  file. Previously, the parser would silently ignore any content that appeared
  after the first valid JSON object. Now, any extraneous data will cause the
  function to return an error. (MR :gl:`!18745`)

Client
------

- **Breaking change** Enforced stricter validation for the JSON files
  manipulated by the client. Previously, the parser would silently ignore any
  content that appeared after the first valid JSON object. Now, any extraneous
  data will cause the function to return an error. (MR :gl:`!18745`)

Signer
------

- **Breaking change** Enforced stricter validation for the JSON files
  manipulated by the signer. Previously, the parser would silently ignore any
  content that appeared after the first valid JSON object. Now, any extraneous
  data will cause the function to return an error. (MR :gl:`!18745`)

Baker
-----

- **Breaking change** Enforced stricter validation for the JSON files
  manipulated by the baker. Previously, the parser would silently ignore any
  content that appeared after the first valid JSON object. Now, any extraneous
  data will cause the function to return an error. (MR :gl:`!18745`)

Agnostic Baker
--------------

- **Breaking change** Enforced stricter validation for the JSON files
  manipulated by the agnostic baker. Previously, the parser would silently
  ignore any content that appeared after the first valid JSON object. Now, any
  extraneous data will cause the function to return an error. (MR :gl:`!18745`)

Accuser
-------

- **Breaking change** Enforced stricter validation for the JSON files
  manipulated by the accuser. Previously, the parser would silently
  ignore any content that appeared after the first valid JSON object. Now, any
  extraneous data will cause the function to return an error. (MR :gl:`!18745`)

Agnostic Accuser
----------------

- **Breaking change** Enforced stricter validation for the JSON files
  manipulated by the agnostic accuser. Previously, the parser would silently
  ignore any content that appeared after the first valid JSON object. Now, any
  extraneous data will cause the function to return an error. (MR :gl:`!18745`)

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

- **Breaking change** Enforced stricter validation for the JSON configuration
  file. Previously, the parser would silently ignore any content that appeared
  after the first valid JSON object. Now, any extraneous data will cause the
  function to return an error. (MR :gl:`!18745`)

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

- **Breaking change** Enforced stricter validation for the JSON configuration
  file. Previously, the parser would silently ignore any content that appeared
  after the first valid JSON object. Now, any extraneous data will cause the
  function to return an error. (MR :gl:`!18745`)


Miscellaneous
-------------
