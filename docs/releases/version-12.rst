Version 12.2
============

Version 12.0 contains a new version (V4) of the protocol environment,
which is the set of functions that protocols can call. This new
version is used by protocol Ithaca, which is a proposal for the
successor of Hangzhou. This release candidate also contains Ithaca
itself as well as its daemons (baker and accuser — there is no
endorser for Ithaca) and the ``ithacanet`` built-in network alias
for the Ithacanet test network.

The second release candidate (12.0~rc2) replaced protocol Ithaca
(``PsiThaCa``) with Ithaca2 (``Psithaca2``) and updated the
``ithacanet`` built-in network alias to the new version of Ithacanet,
which uses protocol Ithaca2 instead of Ithaca. This second release
candidate also included a number of bug fixes and improvements related
to Ithaca and to the storage backend.

Version 12.1 fixes a case where the baker could crash.
It also updates RPC ``selected_snapshot`` by adding an optional parameter,
useful in particular for indexers aiming to support Ithaca.
Warning: using version 12.1 also automatically upgrades the version
of the storage backend; as a consequence, your data directory will no longer be
usable with older versions of Octez.

Version 12.2 increased the size limit for stored metadata and made
it configurable.

Update Instructions
-------------------

To update from sources::

  git fetch
  git checkout v12.2
  rm -rf _opam _build
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v12.2`` Docker images of Tezos.

Changelog
---------

- `Version 12.2 <../CHANGES.html#version-12-2>`_
- `Version 12.1 <../CHANGES.html#version-12-1>`_
- `Version 12.0 <../CHANGES.html#version-12-0>`_
- `Version 12.0~rc2 <../CHANGES.html#version-12-0-rc2>`_
- `Version 12.0~rc1 <../CHANGES.html#version-12-0-rc1>`_
