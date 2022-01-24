Version 12.0~rc2
================

Version 12.0 contains a new version (V4) of the protocol environment,
which is the set of functions that protocols can call. This new
version is used by protocol Ithaca, which is a proposal for the
successor of Hangzhou. This release candidate also contains Ithaca
itself as well as its daemons (baker and accuser — there is no
endorser for Ithaca) and the ``ithacanet`` built-in network alias
for the Ithacanet test network.

The second release candidate (12.0~rc2) replaces protocol Ithaca
(``PsiThaCa``) with Ithaca2 (``Psithaca2``) and updates the
``ithacanet`` built-in network alias to the new version of Ithacanet,
which uses protocol Ithaca2 instead of Ithaca. This second release
candidate also includes a number of bug fixes and improvements related
to Ithaca and to the storage backend.

Update Instructions
-------------------

To update from sources::

  git fetch
  git checkout v12.0-rc2
  rm -rf _opam _build
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v12.0-rc2`` Docker images of Tezos.

Changelog
---------

- `Version 12.0~rc2 <../CHANGES.html#version-12-0-rc2>`_
- `Version 12.0~rc1 <../CHANGES.html#version-12-0-rc1>`_
