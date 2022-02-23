Version 11.0
============

Version 11.0 contains a new version (V3) of the protocol
environment, which is the set of functions that protocols can
call. This new version is used by protocol Hangzhou, which is a
proposal for the successor of Granada, and by Hangzhou2, which is a
modified version of Hangzhou with a number of critical bug fixes. This
release candidate also contains Hangzhou2 itself as well as its daemons
(baker, endorser and accuser).

Version 11.0 contains a user-activated protocol override from
Hangzhou to Hangzhou2 on Mainnet. This means that nodes using version
11.0 (more precisely 11.0~rc2 and later) will activate Hangzhou2
instead of Hangzhou if Hangzhou was
to be activated by the on-chain governance process.

Update Instructions
-------------------

To update from sources::

  git fetch
  git checkout v11.0
  rm -rf _opam _build
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v11.0`` Docker images of Tezos.

If you are installing Octez using Opam, note that the required
OCaml version is now 4.12.1. This means that you need to create a
new switch with ``opam switch create 4.12.1`` before you run ``opam install tezos``.

Known Issues
------------

Users with older CPUs may encounter a "core dumped: illegal instruction" error
when using pre-built executables (Docker images or static binaries).
Such users can fix this issue by:

- using Docker images tagged ``v11.0_no_adx`` (see on `Docker Hub <https://hub.docker.com/layers/tezos/tezos/v11.0_no_adx/images/sha256-b0532eb8cc4201983e24034cf5252992db5c99bb0f7cb10afd1bf4675153ea4a>`_);
- using the no-ADX static binaries
  (`AMD64 <https://gitlab.com/tezos/tezos/-/jobs/1805009867/artifacts/browse/tezos-binaries/>`_ or
  `ARM64 <https://gitlab.com/tezos/tezos/-/jobs/1805009868/artifacts/browse/tezos-binaries/>`_);
- or compiling from source or with Opam (see :doc:`../introduction/howtoget`).

Changelog
---------

- `Version 11.0 <../CHANGES.html#version-11-0>`_
- `Version 11.0~rc2 <../CHANGES.html#version-11-0-rc2>`_
- `Version 11.0~rc1 <../CHANGES.html#version-11-0-rc1>`_
