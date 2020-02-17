.. _snapshots:

Snapshots
---------

As the chain invariably grows every day, retrieving a full chain from
the peer-to-peer network can be a very long process.  Thanks to the
implementation of history modes, it is now possible to propose an
import/export feature: snapshots.  This procedure allows to gather all
the data necessary to bootstrap a node from a single file within few
minutes.

Importing a snapshot
~~~~~~~~~~~~~~~~~~~~

When bootstrapping from a snapshot, the first thing that you want to
do is check the point in history from when you start.

The snapshot format does not (and cannot) provide any evidence that
the imported block is actually a part of the current main chain of the
Tezos network. To avoid to be fooled by a fake chain, it is
**necessary** to carefully check that the block hash of the imported
block is included in the main chain. This can be done by comparing the hash
to one provided by another node under the user’s control, or by
relying on social cues to obtain a hash from a large number of trusted
parties which are unlikely to be colluding.

As the Tezos position paper states:

    *“Occasional checkpoints can be an effective way to prevent very long blockchain reorganizations[…]. Forming a consensus over a single hash value over a period of months is something that human institutions are perfectly capable of safely accomplishing. This hash can be published in major newspapers around the world, carved on the tables of freshmen students, spray painted under bridges, included in songs, impressed on fresh concrete, tattooed on pet ferrets… there are countless ways to record occasional checkpoints in a way that makes forgery impossible.”*

This same wisdom must be applied when using a snapshot.

After that careful selection and verification of the imported block
hash, you can trust the node with the rest of the procedure. In
particular, you do not need to trust the source of the file. The snapshot
format contains everything necessary for the node to detect any
inconsistency, malicious or not.

This safety comes from the fact that block headers are designed to
make sure that applying a block has the same result for everyone in
the network. To achieve this, the block header include hashes of their operations
and predecessor, as well as the resulting chain state. The import
process does the same checks, recomputing and checking all the hashes
it encounters in the snapshot.

To bootstrap a Tezos node from a file ``FILE.full`` to an empty Tezos
node directory (running this command from an already synchronised node
will not work), run:

.. code-block:: console

   tezos-node snapshot import FILE.full --block <BLOCK_HASH>

The ``--block <BLOCK_HASH>`` option argument aims to verify that the
block contained in the snapshot is the one that you are expecting to
import. Otherwise, don’t forget to check the hash of the imported
block displayed by the node when importing.

When importing a ``full`` snapshot you can **optionally** trigger an
``archive`` storage reconstruction using the ``--reconstruct``
option. After importing the snapshot's data, all the chain data will
be recomputed. This operation may take a couple of days to complete.

Exporting a snapshot
~~~~~~~~~~~~~~~~~~~~

To export a snapshot, we first select a block hash which will
represent the point in history at which consumers of this snapshot
will start bootstrapping. By default, if no block hash is provided, we
automatically choose a block corresponding to the last
checkpoint. This is important as nodes bootstrapped from this snapshot
will not be able to reorganise their chain below this block (they will
set their checkpoint to this block).

Depending on the snapshot export option, additional history may also
be put in the snapshot file.  By default, the snapshot export command
will create a ``full`` snapshot. Such a snapshot will contain all the
blocks from a given block hash back to the genesis. The whole chain will be
exported into a snapshot, from the beginning to the selected
point. This kind of snapshot can only be created from a ``full`` or
an ``archive`` node.

.. code-block:: console

   tezos-node snapshot export --block <BLOCK_HASH> FILE.full

If no ``--block <BLOCK_HASH>`` option is given, the last checkpoint
will be chosen as the default block to export.

Rolling snapshots can be exported if you want to deploy a node quickly
or for test and experimentation purposes (such as in a classroom
setting) as they are much smaller. However, to bootstrap a long
running node on the network, we recommend using ``full`` snapshots to
participate into the network wide preservation and sharing of chain
history.

.. code-block:: console

   tezos-node snapshot export --block <BLOCK_HASH> FILE.rolling --rolling


The following table recapitulate the different kind of snapshot that
can be exported from a given history mode node.

+---------+---------------+-----------------+
| From/To | Full snapshot | Rolling snapshot|
+=========+===============+=================+
| Archive | Yes           | Yes             |
+---------+---------------+-----------------+
| Full    | Yes           | Yes             |
+---------+---------------+-----------------+
| Rolling | No            | Yes             |
+---------+---------------+-----------------+
