.. _version-8:

Version 8.3
===========

Version 8.0 contains a new version (V1) of the protocol
environment, which is the set of functions that protocols can call. Up
to Delphi, all protocols used protocol environment V0. The new version
(V1) is used by Edo, which is a proposal for the next protocol after
Delphi. The release candidate also contains Edo itself as well as its
daemons (baker, endorser and accuser) so that you can test it easily.

We have also spawned a test network for Edo, named Edonet, that
replaces Ebetanet, which was a test network for a beta version of
Edo. The release candidate contains the necessary configuration to
join Edonet: just configure your node with
``tezos-node config init --network edonet``.

Version 8.1 fixes a performance regression related to operations
involving ``tz3`` addresses and several compilation problems in
some contexts.

Version 8.2 replaces `PtEdoTez` by `PtEdo2Zk` and provides RPCs to
"normalize" Michelson expressions returned by the Edo protocol along
with constraining the size of p2p messages at low level and updating
some external dependencies.

Version 8.3 fixes a couple of issues that caused the baker to not include
some operations.

Update Instructions
-------------------

Starting from version 8.0, compiling Tezos requires the Rust compiler,
version 1.44.0, and the Cargo package manager to be installed.
See :ref:`instructions to set up Rust<setup_rust>`.

To update from sources::

  git fetch
  git checkout v8.3
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v8.3`` Docker images of Tezos.

Changelog
---------

- `Version 8.3 <../CHANGES.html#version-8-3>`_
- `Version 8.2 <../CHANGES.html#version-8-2>`_
- `Version 8.1 <../CHANGES.html#version-8-1>`_
- `Version 8.0 <../CHANGES.html#version-8-0>`_
- `Version 8.0~rc2 <../CHANGES.html#version-8-0-rc2>`_
- `Version 8.0~rc1 <../CHANGES.html#version-8-0-rc1>`_
