Mainnet-Staging October 2019
============================

**This release changes the storage backend and requires specific care when updating.**

The new storage backend uses a different file format for context
files, which contain chain data. As a result, a mainnet node in
archive mode requires less than 40GB disk space, compared to more than
200GB before that.

To upgrade a mainnet node which is **in archive mode**, you need to
bootstrap from scratch (delete the old context and store folders in the
``.tezos-node`` data directory, otherwise the node will not start).
The synchronization process usually takes a few days. Don't forget to
run your node with ``--history-mode=archive`` as this setting will be lost.

To upgrade a mainnet node which is **not in archive mode**, export a
snapshot **before** updating to this release. Then update your node
and import the snapshot.

If you were testing the feature on branch ``mainnet-pack-validator``,
you were already using the new storage format and
you do not need to bootstrap from scratch or use a snapshot.
However, the storage format did change slightly since this branch
and you need to convert it. To this end, update your node and run:
``dune exec -- vendors/index/fix.exe <data-dir>``
Depending on your hardware this can take a few minutes.

Another notable change is that validation now runs in an external process.
This means, in particular, that your node should be more responsive to
RPC calls. This behavior can be disable using command-line flag ``--singleprocess``.

Changelog
---------

Shell
~~~~~

- Improve storage efficiency by switching from LMDB to new ``pack`` backend

- Make validation run in an external process (can be disabled with --singleprocess)

- Improve bootstrap efficiency

- Slightly improve log readability

- Remove hardcoded testchain TTL

Docker Images
~~~~~~~~~~~~~

- Default Docker image is now stripped of debug symbols, reducing size by 70Mb

- New ``bare`` image is also stripped and does not have the ``alphanet.sh`` entrypoint

- New ``debug`` image is the old default image, i.e. not stripped

Codec
~~~~~

- New binary: ``tezos-codec`` to encode and decode Tezos values

Baker
~~~~~

- Use ``preserved_cycles`` from configuration instead of hard-coded constant

Client
~~~~~~

- Add ``--verbose-signing`` to client for consistency

- Change output of command ``hash data`` (replace ``Hash`` with ``Script-expression-ID-Hash``
  and ``Raw Script-expression-ID-Hash``)

All Binaries
~~~~~~~~~~~~

- Handle ``SIGINT`` and ``SIGTERM`` more consistently

Build System
~~~~~~~~~~~~

- Fix ``make build-dev-deps`` so that installing merlin does not change other packages
