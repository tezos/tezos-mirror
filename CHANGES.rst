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

Node
----

Client
------

Signer
------

Baker
-----

- Fixed the baker silently stopping to include DAL attestations after its DAL
  node restarted or the connection to it was reset or left half-open. The baker
  now detects a stalled ``/profiles/<pkh>/monitor/attestable_slots`` stream (via
  a periodic heartbeat) and reconnects on its own, instead of relying on a clean
  end-of-stream that a reset or half-open connection never delivers. (MR
  :gl:`!22471`)

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

Smart Rollup node
-----------------

Smart Rollup WASM Debugger
--------------------------

Data Availability Layer (DAL)
-----------------------------

DAL node
~~~~~~~~

- Fixed an issue where the attester branch of the shard reception callback
  could issue L1 committee RPCs for message levels far beyond the current
  head. The fetch is now skipped when the committee level exceeds
  ``head_level + attestation_lag + validation_slack``. Additionally, the
  application callback in the P2P receive loop is now spawned
  asynchronously, so a slow callback on one peer connection no longer
  delays shard ingestion from other peers.
  (MR :gl:`!22542`)

- The ``/profiles/<pkh>/monitor/attestable_slots`` streaming RPC now emits a
  periodic heartbeat event (one per level), letting consumers detect a stalled
  connection during periods with no attestable-slot activity. (MR :gl:`!22471`)

- Bounded the amplification reconstruction queue of the DAL node and stopped it
  from enqueuing duplicates. This prevents a sustained flow of shards from
  growing the queue without bound and driving observer/operator nodes out of
  memory. When the bound is reached, excess amplification requests are dropped
  and a warning is logged. (MR :gl:`!22169`)

- Fixed a DAL node crash that could occur after a restart with a persisted
  store: the startup status backfill could leave a slot's status inconsistent
  with the skip-list store, tearing down the node's daemon (gossipsub, P2P and
  RPC) while the process stayed alive. (MR :gl:`!22469`)

- Fixed the attester monitoring logs (``attested slot(s) …``) which, with the
  dynamic attestation lag, could attribute an attestation to the wrong delegate
  and report slots as attested by a baker that did not attest them.
  (MR :gl:`!22472`)

- The DAL gossipsub worker now caps the number of distinct topics a remote peer
  may subscribe to (default 65,536) and the depth of the P2P input queue
  (default 8,192); excess subscriptions and messages are silently dropped.
  (MR :gl:`!22611`)

Miscellaneous
-------------
