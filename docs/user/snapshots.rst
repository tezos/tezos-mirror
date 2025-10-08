Snapshots
=========

When a node in the Tezos network is started, it has to catch up the current state of the blockchain (a process known as "bootstrapping"), making sure that this state is coherent with the whole history of the ledger.
As the chain invariably grows every day, retrieving a full chain from
the peer-to-peer network can be a very long process.  Thanks to the
implementation of :doc:`history modes <history_modes>`, it is now possible to propose an
import/export feature: snapshots.  This procedure allows to gather all
the data necessary to bootstrap a node from a single file within few
minutes.

.. _importing_a_snapshot:

Importing a snapshot
--------------------

When bootstrapping from a snapshot, the first thing that you want to
be sure of is that the loaded state is not fake.

The snapshot format does not (and cannot) provide any evidence that
the imported block is actually a real point in the history of the
Tezos network blockchain. To avoid to be fooled by a fake chain, it is
**necessary** to carefully check that the block hash of the imported
block is included in the main chain. (Note that all the websites providing
snapshots mention the hash corresponding to the head block in each snapshot.)
This check can be done by comparing the hash
to one provided by another node under the user’s control, or by
relying on social cues to obtain a hash from a large number of trusted
parties which are unlikely to be colluding.

For instance, one may use one or more :ref:`Tezos block explorers <block_explorers>` to check whether this block hash matches an existing block.

After that careful selection and verification of the imported block
hash, you can trust the node with the rest of the procedure. In
particular, you do not need to trust the source of the file. The snapshot
format contains everything necessary for the node to detect any
inconsistency, malicious or not.

This safety comes from the fact that block headers are designed to
make sure that applying a block has the same result for everyone in
the network. To achieve this, a block header includes hashes of its operations
and predecessor, as well as the resulting chain state. The import
process does the same checks, recomputing and checking all the hashes
it encounters in the snapshot.

To bootstrap an Octez node from a file to an empty Tezos
node directory (running this command from an already synchronised node
will not work), run:

.. code-block:: console

   octez-node snapshot import <FILE> --block <BLOCK_HASH> [--data-dir <NODE_DIR>]

The ``--block <BLOCK_HASH>`` option argument aims to verify that the
block contained in the snapshot is the one that you are expecting to
import.

.. warning::

   While importing a snapshot, many checks are performed to ensure the
   consistency of the imported data. In order to speed up the process
   and only if the snapshot's source is highly trusted (or exported by
   yourself), it is possible to disable some checks. The validity of
   the target block will be, of course, ensured. However, the rest of
   the data will be copied directly, without additional consistency
   checks. To do so, use the ``--no-check`` option.


Snapshot information
~~~~~~~~~~~~~~~~~~~~

When retrieving a snapshot, it can be useful to check the actual
content of the snapshot. To do so, the node's ``snapshot info``
command can be used to display snapshot's information such as:

 - snapshot's version
 - chain name
 - history mode
 - targeted block hash, level and timestamp
 - ...

This information is displayed by the following command:

.. code-block:: console

   octez-node snapshot info <FILE>

As can be seen in the snapshot information, a snapshot contains
historical data corresponding to a given history mode, which can be:
Full, Archive, or Rolling (see :doc:`history modes <history_modes>`).

Storage reconstruction from a snapshot
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When importing a ``full`` snapshot you can **optionally** trigger an
``archive`` storage reconstruction using the ``--reconstruct``
option. After importing the snapshot's data, all the chain data will
be recomputed. This operation may take a couple of days to complete.

Exporting a snapshot
--------------------

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
blocks from a given block hash back to the genesis. Thus, the whole chain
will be exported into the snapshot, from the beginning to the selected
point. This kind of snapshot can only be created from a ``full`` or an
``archive`` node.

.. code-block:: console

   octez-node snapshot export --block <BLOCK>

The ``<BLOCK>`` hint can be given as a *block hash*, a *block level*,
an alias (*head*, *savepoint* or *checkpoint*) and a relative block
target using the ``~``, ``-`` or ``+`` notation (such as ``head~42``).

If no ``--block <BLOCK>`` option is given, the checkpoint level will
be chosen as the default block to export.

By default, the snapshot will be exported into a file with a name
following this pattern
``<NETWORK>-<BLOCK_HASH>-<BLOCK_LEVEL>.<SNAPSHOT_KIND>``. A specific
snapshot file name can be given as an additional argument. For example:

.. code-block:: console

   octez-node snapshot export recent_head_snapshot.full --block head

.. note::

   If your node's current storage was created with Octez v13.0 (or
   earlier versions), it is not compatible with the context pruning
   storage layout, and therefore it is no longer possible to export a
   storage snapshot.

   In order to benefit from recent storage features and optimizations,
   it is strongly recommended to restart your node with the latest
   Octez version and with a fresh, up-to-date storage -- either by
   importing a recent snapshot (that is, exported by an Octez node
   version >= v13.0), or by bootstrapping from scratch if this is not
   possible.

Rolling export
~~~~~~~~~~~~~~

Rolling snapshots are useful to quickly deploy a node
or for testing and experimentation purposes (such as in a classroom
setting), as they are much smaller. However, to bootstrap a long
running node on the network, we recommend using ``full`` snapshots to
participate into the network-wide preservation and sharing of chain
history.

.. code-block:: console

   octez-node snapshot export <FILE>.rolling --block <BLOCK_HASH> --rolling

Snapshot file format and IPFS
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default, the snapshot is exported as an archive file (in the
``.tar`` format). Such a single archive file is suitable for applying
compression mechanisms. However, the compression of a snapshot file is
not handled by the node.

If one prefers not to export the snapshot as a single archive file, it
is possible to add the flag ``--export-format raw`` to the export
command. The snapshot is then exported as a folder containing all the
necessary files. As the structure of the snapshot follows the storage
representation which is based on the network's cycles, the major part
of the ``raw`` snapshot's data is canonical. The block history is thus
represented as *cemented cycles* and will stay as it is for ever. Only
the rest of the file contains data relative to the snapshot's target
block, such as the current incomplete cycle and the block's associated
ledger state. This canonical representation is well suited for
distributing snapshots through `IPFS <https://ipfs.tech/>`_.


Export capabilities
~~~~~~~~~~~~~~~~~~~

The following table recapitulates the different kinds of snapshots that
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

Snapshot providers
~~~~~~~~~~~~~~~~~~

There are several services providing node snapshots. They create snapshots
of their nodes on a regular basis (usually daily) and make them available for
download. These include:

* `Tzinit snapshots <https://snapshots.tzinit.org/>`_
* `Teztnets (Tezos Testnets) <https://teztnets.com/>`_
