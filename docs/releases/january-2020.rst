Mainnet January 2020
====================

This version was released on Friday, January 10th 2020 with commit hash
``28309c81cc485467f22fb55e96e89302e1f2cd8a``. This is a minor release.

Changelog
---------

- Add the Carthage protocol, to be ready to fork the testchain in case the
  exploration vote passes.

- Add efficiency improvements to the new storage backend to prevent some freezes.

- Restore the possibility to use both a key hash or a key alias with the ``submit proposal``
  and ``submit ballot`` commands.

- Prevent running on 32bit architectures because the protocol does not support them
  and running on such an architecture could cause wrong block hashes to be computed.

Update Instructions
-------------------

These instructions assume that you are running
the :doc:`December 2019 release<december-2019>`, i.e. that
you are already using the new storage backend.

If you are running Tezos using Docker, new Docker images are available.

To update from sources:

- checkout the latest version with ``git checkout mainnet`` followed by ``git pull``;

- ensure the compilation environment is ready with ``make build-deps``
  followed by ``eval $(opam env)``;

- compile with ``make`` (if you node is running from the compilation directory,
  you may have to stop it first);

- restart your node.
