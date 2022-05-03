Version 13.0
============

Version 13.0 contains a new version (V5) of the protocol environment,
which is the set of functions that protocols can call. This new
version is used by protocol Jakarta, which is a proposal for the
successor of Ithaca. This release also contains Jakarta itself as well
as its daemons.

Update Instructions
-------------------

To update from sources::

  git fetch
  git checkout v13.0
  rm -rf _opam _build
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v13.0`` Docker images of Tezos.

Changelog
---------

- `Version 13.0 <../CHANGES.html#version-13-0>`_
- `Version 13.0~rc1 <../CHANGES.html#version-13-0-rc1>`_
