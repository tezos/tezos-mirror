Version 11.0~rc1
================

This release candidate contains a new version (V3) of the protocol environment,
which is the set of functions that protocols can call. This new
version is used by protocol Hangzhou, which is a proposal for the successor
of Granada. This release candidate also contains Hangzhou itself as well as its
daemons (baker, endorser and accuser).

Update Instructions
-------------------

To update from sources::

  git fetch
  git checkout v11.0-rc1
  rm -rf _opam _build
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v11.0-rc1`` Docker images of Tezos.

If you are installing Octez using Opam, note that Opam packages for
Octez v11.0~rc1 are not yet available. Also note that the required
OCaml version is now 4.12.0. This means that you will need to create a
new switch with ``opam switch create 4.12.0``.

Changelog
---------

- `Version 11.0~rc1 <../CHANGES.html#version-11-0-rc1>`_
