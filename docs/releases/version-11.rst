Version 11.1
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

Version 11.1 fixes some storage issues, as well as some build issues.

Update Instructions
-------------------

To update from sources:

.. code-block:: shell

  git fetch
  git checkout v11.1
  rm -rf _opam _build
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v11.1`` Docker images of Tezos.

If you are installing Tezos using Opam, note that the required
OCaml version is now 4.12.1. This means that you need to create a
new switch with ``opam switch create 4.12.1`` before you run ``opam install tezos``.

Changelog
---------

- `Version 11.1 <../CHANGES.html#version-11-1>`_
- `Version 11.0 <../CHANGES.html#version-11-0>`_
- `Version 11.0~rc2 <../CHANGES.html#version-11-0-rc2>`_
- `Version 11.0~rc1 <../CHANGES.html#version-11-0-rc1>`_
