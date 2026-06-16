Version 15.1
============

Version 15.0 contains a new version (V7)
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

Version 15.1 fixes a bug that would cause the bootstrap pipeline to apply a
block without prechecking it first, when the active protocol is Lima.

.. warning::

  The nature of this bug would prevent the correct operation of the Lima protocol
  (due to activate on Tezos Mainnet on block
  `#2,981,889 <https://tzkt.io/2981889>`__)
  with earlier Octez versions, including Octez v15.0.

  As a result, we **strongly recommend** to upgrade to Octez v15.1 or later instead.

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

To update from sources:

.. code-block:: shell

  git fetch
  git checkout v15.1
  rm -rf _opam _build
  make build-deps
  eval $(opam env)
  make

If you are using Docker instead, use the ``v15.1`` Docker images of Tezos.

Then upgrade your storage by following the instructions in :ref:`first_pruning`.

.. _first_pruning:

Context Pruning Requirements and Optimizations
----------------------------------------------

Context pruning is enabled automatically.
However, data directories that have been created by Octez
versions 12.4 or earlier do not support context pruning. If you run a
node on such a data directory you will get a warning. The solution is
to import a fresh snapshot before running the Octez 15.0 node
for the first time, to ensure that the context can be pruned.

The first pruning operation for nodes that have been running for a
long time can take a while and use a significant amount of memory. To
avoid this, it is also recommended to import a fresh snapshot before
running the Octez 15.0 node for the first time. You can refer to
:ref:`the snapshot documentation <importing_a_snapshot>` to get
instructions regarding the snapshot import command.

Changelog
---------

- `Version 15.1 <../CHANGES.html#version-15-1>`_
- `Version 15.0 <../CHANGES.html#version-15-0>`_
- `Version 15.0-rc1 <../CHANGES.html#version-15-0-rc1>`_
