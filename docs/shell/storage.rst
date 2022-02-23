*****************
The storage layer
*****************

This document explains the inner workings of the storage layer of the
Tezos shell. The storage layer is responsible for aggregating blocks
(along with their respective ledger state) and operations (along with
their associated metadata). It is composed of two main parts: the
:ref:`store<store_component>` and the
:ref:`context<context_component>`.

.. _store_component:

Store
#####

This component handles the on-disk storage of static objects such as
blocks, operations, block's metadata, protocols and chain data. The
store also handles the chain's current state: current head, invalid
blocks, active test chains, etc. The store component is designed to
handle concurrent accesses to the data. Both a mutex and a lockfile
are present to prevent concurrent access to critical sections. The
store also manages the :ref:`context<context_component>` and handles
its initialization, but it is not responsible to commit contexts
on-disk. This is done by the :doc:`validator<validation>` component.

The store is initialized using a :doc:`history
mode<../user/history_modes>` that can be either *Archive*, *Full* or
*Rolling*. Depending on the chosen history mode, some data will be
pruned while the chain is growing. In *Full* mode, all blocks that are
part of the chain are kept but their associated metadata below a
certain threshold are discarded. In *Rolling* mode, blocks under a certain
threshold are discarded entirely. *Full* and *Rolling* may take a
number of additional cycles to increase or decrease that threshold.

.. _lafl:

To decide whether a block should be pruned or not, the store uses the latest
head's metadata that contains the **last allowed fork level**. This threshold
specifies that the local chain cannot be reorganized below it.
When a protocol validation returns a change to this value,
it means that a cycle has completed. Then, the store retrieves all the
blocks from ``(head-1).last_allowed_fork_level + 1`` to
``head.last_allowed_fork_level``, which contain all the blocks of a
completed cycle that cannot be reorganized anymore, and trims the
potential branches in the process to yield a linear history.

When an un-reorganizable former cycle is retrieved, it is then
**archived** in what is called the *cemented cycles*. This process is
called a **merge** and is performed asynchronously. Depending on
which history mode is ran and on the amount of additional cycles,
blocks and/or their associated metadata present in these cemented
cycles may or may not be preserved. For instance, if the history mode
is *Archive*, every block and metadata are preserved, if *Full* with 5
additional cycles is given, all the cemented cycles will be present
but only the five most recent cemented cycles will have some metadata
kept, lastly, if *Rolling* with 0 additional cycles, no cemented
cycles will be preserved.

Depending on its history mode, the store maintains two specific
values:

- The *caboose* which represents the lowest block known by the
  store that may or may not possess metadata. In *Archive* and *Full*
  mode, this would always be the genesis block.

- The *savepoint* which indicates the lowest block known by the store
  that possesses metadata.

The *checkpoint* is also a special value that indicates the block
that must be part of the chain. This special block may be in the
future.  Setting a future checkpoint on a fresh node before
bootstrapping adds protection in case of eclipse attacks where a set
of malicious peers will advertise a wrong chain. When the store
reaches the level of a manually defined checkpoint, it will make sure
that this is indeed the expected block or will stop the
bootstrap. When the checkpoint is unset or reached, the store will
maintain the following invariant: ``checkpoint ≥
head.last_allowed_fork_level``.

To access those values, it is possible, while the node is running, to
call the RPC ``/chains/main/checkpoint`` to retrieve the checkpoint,
savepoint, caboose and its history mode.

The store also has the capability to reconstruct its blocks' metadata
by replaying every block and operation present and repopulating the
context. Hence, transforming a `Full` store into a `Archive` one.

It is also possible to retrieve a canonical representation of the
store and context for a given block (provided that its metadata are
present) as a :doc:`snapshot<../user/snapshots>`.

Protocols no longer active are also written on-disk.

Files hierarchy
***************

The store directory in the node's ``<data-dir>`` is organized as follows:

- ``<data-dir>/store/protocols/`` the directory containing stored
  protocols.

- ``<data-dir>/store/protocols/<protocol_hash_b58>*`` files containing
  the stored encoded protocol.

- ``<data-dir>/store/<chain_id_b58>/`` the *chain_store_dir* directory
  containing the main chain store.

- ``<data-dir>/store/<chain_id_b58>/lock`` the lockfile.

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
  described.

.. _context_component:

Context
#######

The context is a versioned key/value store that associates for each
block a view of its ledger state. The versioning uses concepts similar
to `Git <http://git-scm.com/>`_. The current implementation is using
`irmin <https://github.com/mirage/irmin>`_ as backend and abstracted
by the ``lib_context`` library.

The abstraction provides generic accessors/modifiers: ``set``,
``get``, ``del``, etc. manipulating a concrete context object and
git-like commands: ``commit``, ``checkout`` to manipulate different
context branches.

The Tezos context comes with a specific context hash function that
cannot be changed. Otherwise, the replicated consistency would not be
maintained. In particular, the resulting hash of the application of a
block is stored in its header. When validated, a block's announced
``context hash`` is checked against our local validation result. If
the two context hashes are different, the block is considered invalid.

A context is supposed to be accessed and modified using the protocols'
API. It may be through RPCs or via blocks application. Only the
resulting context of valid blocks application is committed on disk.

It is possible to export a concrete context associated to a specific
block's ledger state. This feature dumps a canonical representation of
this ledger state that may be incorporated in a snapshot to expose a
minimal storage state.

Note that it is possible to enable logging for the context backend
using the ``TEZOS_CONTEXT`` environment variable. There are two
possible values for this variable: ``v`` for ``Info`` logging and
``vv`` for ``Debug`` logging (warning, the ``Debug`` mode is very
talkative). Additionally, this environment variable allows to tweak,
with care, some context parameters (using the standard
`TEZOS_CONTEXT="variable=value"` pattern, separating the items with
commas such as `TEZOS_CONTEXT="v, variable=value"`):

- "index-log-size": number of entries stored in the Irmin's index
  (default `2_500_000`)
- "auto-flush": number of tree mutations allowed before a disk flush
  (default `10_000`)
- "lru-size": number of entries stored in the Irmin's LRU cache
  (default `5_000`)
