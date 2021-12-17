Version 12.0~rc1
================

Version 12.0 contains a new version (V4) of the protocol environment,
which is the set of functions that protocols can call. This new
version is used by protocol Ithaca, which is a proposal for the
successor of Hangzhou. This release candidate also contains Ithaca
itself as well as its daemons (baker and accuser — there is no
endorser for Ithaca) and the ``ithacanet`` built-in network alias
for the Ithacanet test network.

Update Instructions
-------------------

To update from sources::

  git fetch
  git checkout v12.0-rc1
  rm -rf _opam _build
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v12.0-rc1`` Docker images of Tezos.

Changelog
---------

- `Version 12.0~rc1 <../CHANGES.html#version-12-0-rc1>`_
