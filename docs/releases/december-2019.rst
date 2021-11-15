Mainnet December 2019
=====================

**This release changes the storage backend and requires specific care when updating.**

The new storage backend uses a different file format for context
files, which contain the ledger state. As a result, a mainnet node in
archive mode requires around 40GB disk space, compared to more than
220GB before that. See instructions in the `Storage Upgrade`_ section.

Another notable change is that validation now runs in an external process.
This means, in particular, that your node should be more responsive to
RPC calls. This behavior can be disable using command-line flag ``--singleprocess``.

The node also features a new ``reconstruct`` command.
It converts a node in ``full`` history mode into an ``archive`` node.

Changelog
---------

Shell
~~~~~

- Improve storage efficiency by switching from LMDB to new ``pack`` backend

- Add new ``reconstruct`` command to convert a ``full`` node into an ``archive`` node

- Make validation run in an external process (can be disabled with ``--singleprocess``)

- Improve bootstrap efficiency

- Slightly improve log readability

- Remove hardcoded testchain TTL

- The unban RPC now also removes the requested address or peer from the greylist

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

Storage Upgrade
---------------

To upgrade your node, you first need to determine the situation you are currently in.

- If you are using the latest ``mainnet-staging`` branch (after Dec. 3rd 2019,
  commit hash ``fc8990b1a``), you already have the latest storage version.
  Just compile the new version of the node (see `How to Update the Node`_).

- If you are using the previous release of the ``mainnet-staging`` branch
  (before Dec. 3rd 2019), follow the instructions in
  `Upgrade From Mainnet-Staging (Released Before Dec. 3rd 2019)`_.

- If you are using a previous release of the ``mainnet`` branch:

  - find out whether you are using the ``archive`` history-mode or not by running:
    ``./tezos-admin-client show current checkpoint | grep 'History mode'``
    while your node is running;

  - if you are in the default ``full`` mode, follow the instructions in
    `Upgrade From Previous Mainnet in Full Mode`_;

  - if you are in the ``archive`` mode and want to keep it that way:

    - if your hardware can handle running two nodes in parallel, follow the instructions in
      `Upgrade From Previous Mainnet in Archive Mode (Minimal Interruption)`_;

    - else, or if interrupting your node for several days is not an issue,
      follow the instructions in
      `Upgrade From Previous Mainnet in Archive Mode (Several Days Interruption)`_.

In any case, if you have an older storage version and
you upgrade and start your node, it will tell you to upgrade your storage.

Upgrade From Mainnet-Staging (Released Before Dec. 3rd 2019)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The previous version of the ``mainnet-staging`` branch already uses
the new storage backend and is thus simple to upgrade from.

- Stop your node.

- Compile the new version of the node (see `How to Update the Node`_).

- Run: ``./tezos-node upgrade storage``
  This takes less than a second.

- Start your node as usual with: ``./tezos-node run``

Upgrade From Previous Mainnet in Full Mode
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Export or download a snapshot (see `How to Export a Snapshot`_).

- Stop your node.

- Compile the new version of the node (see `How to Update the Node`_).

- Remove the ``context`` and ``store`` folders in your data directory,
  or simply move them away with: ``mv ~/.tezos-node/context ~/tezos-context-backup``
  and: ``mv ~/.tezos-node/store ~/tezos-store-backup``

- Import your snapshot using:
  ``./tezos-node snapshot import snapshot.full``
  This takes between about 10 minutes and one hour depending on your hardware.

- You are now ready to start your upgraded node with: ``./tezos-node run``

If your node is running well and you made backups of your ``context`` and ``store``
directories, you can now safely remove them:
``rm -rf ~/tezos-context-backup ~/tezos-store-backup``

Upgrade From Previous Mainnet in Archive Mode (Minimal Interruption)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The upgrade will take several days to complete.
In order not to stop your node for too long, we will perform
the upgrade using a second node running in parallel.
If your hardware cannot handle two nodes running in parallel,
you can run the second node on another machine.

- Compile the new version of the node (see `How to Update the Node`_).

- Use it to generate a new data directory with a new identity with:
  ``./tezos-node identity generate --data-dir ~/tezos-node-2``

Now choose between option A and option B.

- **Option A**: import a snapshot and reconstruct.

  - Export a snapshot from your first node or download one (see `How to Export a Snapshot`_).

  - Import it in your second node using
    ``./tezos-node snapshot import snapshot.full --data-dir ~/tezos-node-2 --reconstruct``
    (replace ``snapshot.full`` with the filename of your snapshot).
    The reconstruction takes a couple of days to complete.

  - Once it is done, start your node as usual with:
    ``./tezos-node run --data-dir ~/tezos-node-2``
    and let it run for a while so that it catches up with the latest blocks that were produced
    while you were reconstructing your context.

- **Option B**: bootstrap your node from scratch.
  Just start your second node with:
  ``./tezos-node run --history-mode=archive --data-dir ~/tezos-node-2``
  It will take about a week to synchronize.

You now have a second node which is running with the new storage backend.

- Stop your first node and your baker, endorser and accuser.

At this point you may want to move the default data directory away
and replace it with the second data directory (stop your second node first, and
start it again after that).
Whether you do that or not, you can now restart your baker, endorser and accuser
using your second node.

Upgrade From Previous Mainnet in Archive Mode (Several Days Interruption)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The upgrade will take several days to complete, during which your node
will not be usable. If this is an issue, read the above section instead.

- If you plan to choose option B (see later),
  export or download a snapshot (see `How to Export a Snapshot`_).

- Stop your node.

- Compile the new version of the node (see `How to Update the Node`_).

- Remove the ``context`` and ``store`` folders in your data directory,
  or simply move them away with: ``mv ~/.tezos-node/context ~/tezos-context-backup``
  and: ``mv ~/.tezos-node/store ~/tezos-store-backup``

Now choose between option A and option B.
Option A is faster but uses more RAM.

- **Option A**: import a snapshot and reconstruct.

  - Import your snapshot using:
    ``./tezos-node snapshot import snapshot.full --data-dir ~/tezos-node-2 --reconstruct``
    The reconstruction takes a couple of days to complete.

  - You are now ready to start your upgraded node with: ``./tezos-node run``

- **Option B**: bootstrap your node from scratch.
  Just start your node as usual with:
  ``./tezos-node run --history-mode=archive``
  It will take about a week to synchronize.

If your node is running well and you made backups of your ``context`` and ``store``
directories, you can now safely remove them:
``rm -rf ~/tezos-context-backup ~/tezos-store-backup``

How to Export a Snapshot
~~~~~~~~~~~~~~~~~~~~~~~~

Some of the previous instructions require you to export a snapshot.
Here is how to do so.
You may also just download a recent snapshot instead.

- Get the hash of the current block using:
  ``./tezos-client rpc get /chains/main/blocks/head | grep 'hash\": \"BL'``
  (or simply find the hash in the logs of your running node).

- Export the snapshot with: ``./tezos-node snapshot --block <BLOCK> export snapshot.full``
  (replace ``<BLOCK>`` with the hash of the current block).

If you do not specify ``--block`` the snapshot will be less recent
and thus your node will have to spend some time to catch up.

How to Update the Node
~~~~~~~~~~~~~~~~~~~~~~

This section assumes that you compile your node from a clone of the Tezos Git repository.

- Checkout the latest version: ``git checkout mainnet && git pull``

- Prepare the compilation environment: ``make build-deps && eval $(opam env)``

- Stop your node so that the compilation process can overwrite the binaries
  (unless you copied the binaries somewhere else before running them).

- Compile the new binaries: ``make``


Guide for ``mainnet.sh`` Users
------------------------------

This Guide assumes you have been running an archive node with the ``mainnet.sh`` script,
and helps you upgrade to the new storage format.

1) Create a New Docker Volume
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Run: ``docker volume create node_migration``

Now either download a full snapshot file, or create one from your Tezos node.

2a) Import an Existing Snapshot File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Create a new container that will upgrade to the new storage format.
This is going to take multiple days.
Run::

  docker run -d --name upgrader \
      --mount source=node_migration,target=/tezosdata \
      -v /path/to/snapshot/file.full:/snap.full \
      tezos/tezos-bare:master \
      tezos-node snapshot import /snap.full --data-dir /tezosdata --reconstruct

While this is running you can check the logs with ``docker logs -f upgrader``.
Wait until this command terminates.

2b) Create Your Own Snapshot File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Run::

  docker exec -it mainnet_node_1 tezos-node snapshot export /snap.full

to create the file, and copy it to your host with::

  docker cp mainnet_node_1:/snap.full ./snap.full

Then proceed with Step 2a.

3) Copy New Data to Your Tezos Node
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

First stop your node with ``mainnet.sh stop``, copy the files with::

  docker run --rm -it \
      -v mainnet_node_data:/old \
      -v node_migration:/new  \
      alpine cp -a /new/. /old

and start your node again with ``./mainnet.sh start``.
This will automatically update the docker image for your node to the most recent version.
