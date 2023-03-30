*****************
The storage layer
*****************

This document explains the inner workings of the storage layer of the
Octez shell. The storage layer is responsible for aggregating blocks
(along with their respective ledger state) and operations within
blocks (along with their associated metadata). It is composed of two
main components: a :ref:`store component <store_component>`
providing storage abstractions for blockchain data such as blocks and operations; and the :ref:`context component <context_component>` providing storage abstractions for ledger states (also called contexts).

.. _store_component:

Store
#####

The store component is the :package:`tezos-store` package implemented in the :src:`src/lib_store` library. It handles the on-disk storage of static objects such as
blocks, operations, block's metadata, protocols and chain data. The
store also handles the chain's current state: current head, invalid
blocks, active test chains, etc. The store component is designed to
handle concurrent accesses to the data. Both a mutex and a lock file
are present to prevent concurrent access to critical sections. The
store also provides an accessor to the :ref:`context<context_component>` and handles
its initialization, but it is not responsible to commit contexts
on disk. This is done by the :doc:`validation toolchain <validation>`.

The store is initialized using a :doc:`history
mode<../user/history_modes>` that can be either *Archive*, *Full* or
*Rolling*. Depending on the chosen history mode, some data will be
pruned while the chain is growing. In *Full* mode, all blocks that are
part of the chain are kept but their associated metadata below a
certain threshold are discarded. In *Rolling* mode, blocks under a
certain threshold are discarded entirely. The thresholds of *Full* and *Rolling* modes may
be varied by specifying a number of :ref:`additional cycles to keep <History_mode_additional_cycles>`.

The moments when data may be pruned are when a cycle is completed.
When this happens, the store performs two operations.
First, the block history is linearized by trimming branches in the completed cycle.
Secondly, the remaining blocks in the completed cycle (or just their metadata), and possibly their context (ledger state), can be pruned, according to the history mode.
Both operations are explained next.

Trimming
********

.. _lafl:

To notice when a cycle has completed, the store uses the
latest head's metadata that contains the **last allowed fork
level**. This specifies the point under which the local chain cannot be
reorganized. When a protocol validation operation returns a changed
value for this point, it means that a cycle has completed. Then, the store
retrieves all the blocks from ``(head-1).last_allowed_fork_level + 1``
to ``head.last_allowed_fork_level``, which contain all the blocks of the
completed cycle, that cannot be reorganized anymore, and trims the
potential branches to yield a linear history.

Pruning
*******

When the complete (hence, un-reorganizable) cycle is retrieved, it is
archived with the *cemented cycles*. This process is
called a **merge** and is performed asynchronously. Depending on which
history mode is ran and on the amount of additional cycles, blocks
and/or their associated metadata present in these cemented cycles may
or may not be preserved. For instance, if the history mode is
*Archive*, every block is preserved, with all its metadata. If it is
*Full* with 5 additional cycles, all the cemented cycles will be
present but only the 10 most recent cemented cycles will have some
metadata kept (see details at :ref:`History_mode_additional_cycles`).
Older metadata is pruned.

Starting with Octez v15.0, the store also triggers *context pruning* when a cycle is completed, after finishing the store trimming and cementing.
Thus, whenever pruning the metadata of a block, its context (ledger state associated to that block) is pruned as well.

For the operational details of pruning, see :ref:`first_pruning`.

Other features
**************

Note that after pruning metadata of some blocks, the store has the capability to reconstruct it
by replaying every block and operation present and repopulating the
context. Hence, it is possible to transform a ``Full`` store into an ``Archive`` one (see also :ref:`Switch_mode_restrictions`).

It is also possible to retrieve a canonical representation of the
store and context for a given block (provided that its metadata are
present) as a :doc:`snapshot<../user/snapshots>`.

The store also writes on disk the sources of protocols no longer active.
This allows to recompile them or even share them on the network if needed.

Store variables
***************

The store maintains two specific variables related to the pruned data, whose values depend on the
history mode:

- The *caboose*, which represents the oldest block known by the
  store. The latter block may or may not have its metadata in
  store. In *Archive* and *Full* mode, this would always be the
  genesis block.

- The *savepoint* which indicates the lowest block known by the store
  that possesses metadata.

The *checkpoint* is another variable maintained by the store, that indicates one block that
must be part of the chain. This special block may be in the future.
Setting a future checkpoint on a fresh node before bootstrapping adds
protection in case of eclipse attacks where a set of malicious peers
will advertise a wrong chain. When the store reaches the level of a
manually defined checkpoint, it will make sure that this is indeed the
expected block or will stop the bootstrap. When the checkpoint is
unspecified by the user, the store sets it to the :ref:`last allowed fork level <lafl>`, each time this latter is updated. In any case, the store will maintain the following invariant:
``checkpoint ≥ head.last_allowed_fork_level``.

While the node is running, it is possible to
call the following RPCs to access the values of all these variables:

- the checkpoint: `GET /chains/<chain_id>/levels/checkpoint <http://tezos.gitlab.io/shell/rpc.html#get-chains-chain-id-levels-checkpoint>`__
- the savepoint `GET /chains/<chain_id>/levels/savepoint <http://tezos.gitlab.io/shell/rpc.html#get-chains-chain-id-levels-savepoint>`__
- the caboose: `GET /chains/<chain_id>/levels/caboose <http://tezos.gitlab.io/shell/rpc.html#get-chains-chain-id-levels-caboose>`__
- the history mode: `GET /config/history_mode <http://tezos.gitlab.io/shell/rpc.html#get-config-history-mode>`__

Files hierarchy
***************

The Store maintains data on disk in the
``store`` subdirectory of the node's ``<data-dir>``, organized as follows:

- ``<data-dir>/store/protocols/`` the directory containing stored
  protocols.

- ``<data-dir>/store/protocols/<protocol_hash_b58>*`` files containing
  the stored encoded protocol.

- ``<data-dir>/store/<chain_id_b58>/`` the *chain_store_dir* directory
  containing the main chain store.

- ``<data-dir>/store/<chain_id_b58>/lock`` the lock file.

- ``<data-dir>/store/<chain_id_b58>/config.json`` the chain store's
  configuration as a JSON file.

- ``<data-dir>/store/<chain_id_b58>/cemented/`` contains the cemented
  cycles and index tables.

- ``<data-dir>/store/<chain_id_b58>/cemented/metadata`` contains the
  cemented cycles' compressed metadata (using *zip* format).

- ``<data-dir>/store/<chain_id_b58>/{ro,rw}_floating_blocks`` contains
  the most recent blocks in the chain not yet ready to be archived and
  potential branches.

- ``<data-dir>/store/<chain_id_b58>/<stored_data>*`` files containing
  encoded simple data structures such as: genesis block, checkpoint,
  savepoint, caboose, protocol levels, forked chains, alternate heads,
  invalid blocks, etc.

- ``<data-dir>/store/<chain_id_b58>/testchain/<chain_id_b58>*/``
  contains the stores for every encountered test chains throughout the
  network. The underlying hierarchy follows the same format as
  the *chain_store_dir* directory containing the main chain store, described above.

.. _context_component:

Context
#######

The context component is the the :package:`tezos-context` package, implemented in the :src:`src/lib_context`
library. It is a versioned key/value store that associates to each
block a view of its ledger state. The :package-api:`on-disk context API <tezos-context/Tezos_context_disk/index.html>` exports versioning concepts similar
to `Git <https://git-scm.com/>`_. The current implementation is using
`Irmin <https://github.com/mirage/irmin>`_ as a backend.

The API provides generic accessors/modifiers for manipulating a concrete context object and
git-like commands: ``commit``, ``checkout`` to manipulate different
context branches.

The Tezos context comes with a specific context hash function that
cannot be changed. Otherwise, the replicated consistency would not be
maintained. In particular, the resulting hash of the application of a
block is stored in its header. When validated, a block's announced
``context hash`` is checked against our local validation result. If
the two context hashes are different, the block is considered invalid.

The context of a block can be accessed using the protocols' RPCs such as
`GET ../\<block_id\> <https://tezos.gitlab.io/active/rpc.html#get-block-id>`__, and more specifically by RPCs under the path ``../<block_id>/context``.

The context of the blockchain is only modified by :doc:`blocks applications <../active/validation>`. Only the
contexts resulting from the application of valid blocks is committed on disk, by the validation toolchain.

It is possible to export to a file a concrete context associated to a specific
block's ledger state. This feature dumps a canonical representation of
this ledger state that may be incorporated in a :doc:`snapshot <../user/snapshots>`, exposing a
minimal storage state.

Note that it is possible to enable :doc:`logging <../user/logging>` for the context backend
using the ``TEZOS_CONTEXT`` environment variable. There are two
possible values for this variable: ``v`` for ``Info`` logging and
``vv`` for ``Debug`` logging (warning: the ``Debug`` mode is very
talkative). Additionally, this environment variable allows to tweak,
with care, the following context parameters (using the standard
``TEZOS_CONTEXT="variable=value"`` pattern, separating the items with
commas such as ``TEZOS_CONTEXT="v, variable=value"``):

- ``index-log-size``: number of entries stored in the Irmin's index
  (default ``2_500_000``)
- ``auto-flush``: number of tree mutations allowed before a disk flush
  (default ``10_000``)
- ``lru-size``: number of entries stored in the Irmin's LRU cache
  (default ``5_000``)
