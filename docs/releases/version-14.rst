Version 14.0~rc1
================

The first release candidate of Octez version 14.0 (14.0~rc1) contains a new
version (V6) of the protocol environment, which is the set of functions that
a protocol can call. This new version is used by protocol Kathmandu, which is a
proposal for the successor of Jakarta. This release also contains Kathmandu
itself as well as its daemons.

Note that this release includes experimental executables provided for testing
purpose only: tx-rollup nodes and clients as well as sc-rollup nodes and
clients.


Update Instructions
-------------------

To update from sources::

  git fetch
  git checkout v14.0-rc1
  opam switch remove .
  rm -rf _opam _build
  make build-deps
  eval $(opam env)
  make

Note that ``opam switch remove .`` is only needed if you are updating an already
compiled repository, not if you are compiling from a freshly cloned repository.
This command is needed because Octez now requires OCaml 4.14.0.

If you are using Docker instead, use the ``v14.0-rc1`` Docker images of Tezos.

Changelog
---------

- `Version 14.0~rc1 <../CHANGES.html#version-14-0-rc1>`_
