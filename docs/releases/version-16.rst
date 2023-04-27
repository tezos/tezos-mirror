Version 16.1
============

Version 16.0 contains a new version (V8) of the protocol environment,
which is the set of functions that a protocol can call. This new version is used by protocol :doc:`Mumbai<../protocols/016_mumbai>`,
which is a proposal for the successor of Lima. This release also
contains Mumbai itself as well as its daemons.

New executables have been introduced for Smart Rollups.
This includes ``octez-smart-rollup-node-PtMumbai``, ``octez-smart-rollup-client-PtMumbai`` and ``octez-smart-rollup-wasm-debugger``.
They can be used with the ``Mumbai`` protocol.

Note that the Octez v16.0 uses the ``PtMumbai2`` variant of the Mumbai protocol, as announced `here <https://research-development.nomadic-labs.com/mumbai2-announcement.html>`_, instead of ``PtMumbaii``.


.. warning::

   Octez version 15.0 renamed all executables. The ``tezos-`` prefix
   has been replaced by ``octez-`` and protocol numbers have been removed.
   However, the old naming convention was still available via symbolic links.
   Starting from version 16.0, the symbolic links from old names to new names are no longer created when compiling from source.
   The old names are no longer supported.

Octez v16.0 uses a new data format for the storage layer, introduced in order to reduce disk usage during context pruning.
There is no particular action needed to upgrade the storage, as it is done automatically when restarting the node.
However, during the next 6 cycles after upgrading to Octez v16,
the total disk usage will be slightly higher due to the automatic storage migration.
This will result in an overhead of a few gigabytes (around 10GB) of disk usage during a 6 cycles period.
To avoid this overhead, you can import a fresh snapshot.
This version also fixes an issue where old contexts (created by a node running Octez v13.0 or earlier) would be corrupted at cycle switch.
Consequently, the snapshot export is disabled for these old contexts.

This release includes the following baker improvements:

- More efficient block production thanks to a lighter operation validation mechanism. The previous validation mechanism can be restored by passing the ``--force-apply`` option when starting the baker binary.
- The baker no longer verifies the signature of operations when baking a new block. It should be done by the (external) mempool.
- Reduced delay of preendorsement injection.
  The baker now preendorses block proposals as soons as the node considers the block as valid.
- Baker injects preendorsements twice: once the block is considered as valid by the node and once it is fully applied by the node.
  This is a safety measure in case the early preendorsement is dropped by the mempool.

Version 16.1 fixes an implementation bug in the baker binary that would made bakers pre-endorse block proposals without subsequently endorsing them, preventing the optimal functioning of the baking process and potentially deteriorating network liveness.

Update Instructions
-------------------

To update from sources::

  git fetch
  git checkout v16.1
  make clean
  rm -rf _opam  # if updating from v15 or older
  make build-deps
  eval $(opam env)
  make

Note that, as the minimal required OCaml version is now 4.14.1, you may have to remove the ``_opam/`` directory when updating from Octez v15.1 or older released versions.
See more details :ref:`here <update_from_sources>`.

If you are using Docker instead, use the ``v16.1`` Docker images of Octez.

If you are installing Octez using Opam, note that the minimal required
OCaml version is now 4.14.1. This means that you might need to create a
new switch with ``opam switch create 4.14.1`` before you run ``opam install tezos``.


Changelog
---------

- `Version 16.1 <../CHANGES.html#version-16-1>`_
- `Version 16.0 <../CHANGES.html#version-16-0>`_
- `Version 16.0~rc3 <../CHANGES.html#version-16-0-rc3>`_
- `Version 16.0~rc2 <../CHANGES.html#version-16-0-rc2>`_
- `Version 16.0~rc1 <../CHANGES.html#version-16-0-rc1>`_
