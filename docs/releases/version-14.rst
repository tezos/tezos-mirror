Version 14.0
============

Version 14.0 contains a new version (V6) of the protocol environment,
which is the set of functions that a protocol can call. This new
version is used by protocol Kathmandu, which is a proposal for the
successor of Jakarta. This release also contains Kathmandu itself as
well as its daemons.

Note that this release includes experimental executables provided for testing
purpose only: tx-rollup nodes and clients as well as sc-rollup nodes and
clients.

If you are updating from version 13.0, note that version 14.0 changes
the storage format. Run ``tezos-node upgrade storage`` to update your
storage. This upgrade is instantaneous but the data-directory can no
longer be used with version 13.0 once upgraded.

Update Instructions
-------------------

To update from sources::

  git fetch
  git checkout v14.0
  opam switch remove .
  rm -rf _opam _build
  make build-deps
  eval $(opam env)
  make

Note that ``opam switch remove .`` is only needed if you are updating an already
compiled repository, not if you are compiling from a freshly cloned repository.
This command is needed because Octez now requires OCaml 4.14.0.

If you are using Docker instead, use the ``v14.0`` Docker images of Tezos.

Changelog
---------

- `Version 14.0 <../CHANGES.html#version-14-0>`_
- `Version 14.0~rc1 <../CHANGES.html#version-14-0-rc1>`_
