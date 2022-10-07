Version 15.0~rc1
================

The first release candidate of version 15.0 contains a new version (V7)
of the protocol environment, which is the set of functions that a
protocol can call. This new version is used by protocol :doc:`Lima<../protocols/015_lima>`,
which is a proposal for the successor of Kathmandu. This release also
contains Lima itself as well as its daemons.

This version introduces the context pruning for the context part of
the storage backend enabled by default for all nodes running with a
full or rolling history mode. Thanks to it, the size taken on disk is
no longer proportional to the time since the node is running because
the states of the chains below the savepoint (by default 6 cycles) are
erased.

.. warning::

   All executables have been renamed.  The ``tezos-`` prefix
   has been replaced by ``octez-`` and protocol numbers have been
   removed. For instance, ``tezos-node`` is now named ``octez-node``
   and ``tezos-baker-014-PtKathma`` is now named
   ``octez-baker-PtKathma``.  Same renaming occurred in the Docker
   entrypoint. For instance, ``tezos-node`` is now named
   ``octez-node`` and ``tezos-baker`` is now named ``octez-baker``.

   Note that if you compile from source using ``make``, symbolic links
   from the old names to the new names are created, so you can still
   use the old names.  But those old names are deprecated and may stop
   being supported starting from version 16.0. Idem for the old command
   names in the docker entrypoint.

Update Instructions
-------------------

To update from sources::

  git fetch
  git checkout v15.0-rc1
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v15.0-rc1`` Docker images of Tezos.

Changelog
---------

- `Version 15.0-rc1 <../CHANGES.html#version-15-0-rc1>`_
