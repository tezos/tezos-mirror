Version 14.1
============

Version 14.1 contains a new version (V6) of the protocol environment,
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

Version 14.1 fixes a number of issues with JSON encodings
and updates built-in network aliases.

Update Instructions
-------------------

To update from sources:

.. code-block:: shell

  # Removes tezos folder from PATH if added with Octez <= v13 instructions
  PATH=${PATH##"$HOME"/tezos/:}
  git fetch
  git checkout v14.1
  opam switch remove .
  rm -rf _opam _build
  make build-deps
  eval $(opam env)
  make

.. note::

   Note that ``opam switch remove .`` is only needed if you are
   updating an already compiled repository of Octez v13.0 or older,
   not if you are compiling from a freshly cloned repository or if you are
   upgrading from Octez v14.0. This command is needed because
   Octez now requires OCaml 4.14.0.

.. warning::

   If you are updating to Octez v14 using a development
   environment which had been used to build Octez versions up to
   v13.x, and also you have previously exported the ``tezos``
   directory to the ``$PATH`` environment variable, the following
   stanza is necessary to avoid potential issues with opam in the
   ``make build-deps`` step::

     PATH=${PATH##"$HOME"/tezos/:}

   Otherwise, it is possible for ``make build-deps`` to fail with the
   following (or a similar) error::

     make: opam: Permission denied
     Makefile:53: *** Unexpected opam version (found: , expected: 2.*).  Stop.

If you are using Docker instead, use the ``v14.1`` Docker images of Tezos.

If you are using other forms of Octez distributions (e.g. binary packages), check the update instructions at the end of the corresponding section in :doc:`../introduction/howtoget`.

Changelog
---------

- `Version 14.1 <../CHANGES.html#version-14-1>`_
- `Version 14.0 <../CHANGES.html#version-14-0>`_
- `Version 14.0~rc1 <../CHANGES.html#version-14-0-rc1>`_
