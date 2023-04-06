History modes
=============

History modes allow a node to require less disk storage. Indeed,
depending on the chosen history mode, some parts of the complete chain
history can be deleted as they are not required anymore.

Three history modes are provided:

- **Full** (default mode with 1 additional cycle)

  The node stores the minimal data since the genesis required to reconstruct
  (or 'replay') the complete chain's ledger state.

  * Upsides:

    + Can synchronize using a snapshot.
    + Keep all necessary information in order to reconstruct all the
      chain's ledger state (balances, contracts, etc..) since the
      genesis block.
    + Requires little disk storage.
    + Suitable for bakers as you can still query any block information
      or operation at any level.
    + Help other nodes to bootstrap and synchronize with the chain.


  * Downsides:

    - The node is not able to query the balances or staking rights
      before the current checkpoint.
    - Disk storage slowly increases as the node keeps the history.

  See how to :ref:`set up a full node<Set_up_a_full_node>`.

- **Rolling**

  This is the lightest mode as it only maintains a minimal rolling fragment of the
  chain data so the node can still validate new blocks and synchronize with the head.

  * Upsides

    + Only requires a minimal and bounded disk storage.
    + Can run on low resources architectures.
    + Can be bootstrapped within minutes, thanks to a rolling snapshot import.

  * Downsides

    - The node is not able to query block information of balances and
      staking rights before the current checkpoint.
    - The node does not help other nodes to bootstrap as it is not able to
      send the whole chain history.

  See how to :ref:`set up a rolling node<Set_up_a_rolling_node>`.

- **Archive**

  This is the heaviest mode as it keeps the whole chain data to be able to
  query any information stored on the chain since the genesis. It is
  particularly suitable for indexers or block explorers.

  * Upsides

    + The whole chain data is available.

  * Downsides

    - Consume an increasing and rather large amount and data storage.

  See how to :ref:`set up an archive node<Set_up_an_archive_node>`.

.. _Recap:

History modes in a nutshell
---------------------------

+---------+----------------+---------------------+--------------------+
|         | Storage amount | Suitable for bakers | Operations history |
+=========+================+=====================+====================+
| Archive | High           | Yes                 | Complete           |
+---------+----------------+---------------------+--------------------+
| Full    | Limited        | Yes                 | Complete           |
+---------+----------------+---------------------+--------------------+
| Rolling | Low            | Restricted*         | Restricted*        |
+---------+----------------+---------------------+--------------------+

(*) Suitable for delegation services if the number of additional
kept cycles is high enough to allow the computation of rewards.
See :ref:`Keeping additional cycles<History_mode_additional_cycles>` for
details.

History modes use some markers which are used to describe the state
of the storage:

- `checkpoint`: the last allowed fork level of the chain (as defined
  in the Tezos position paper),
- `savepoint`: the last known block which contains metadata,
- `caboose`: the last known block.

.. _Set_up_a_full_node:

Setting up a node in full mode
------------------------------

To run a ``full`` node you can either use the command line arguments:

.. code-block:: console

   octez-node run --history-mode full

or use your configuration file as described in :doc:`here <node-configuration>`:

.. code-block:: json

   { "shell": {
       "history_mode": "full"
   }}

Note that, since the full mode is the default one, this configuration is optional.

You can then verify that your history mode is set to full by using the checkpoint RPC.

.. code-block:: console

   octez-client rpc get /chains/main/checkpoint

.. code-block:: json

    { "block": { "some": "data" },
       "savepoint": 4096, "caboose": 0, "history_mode": "full" }

In full mode, the `savepoint` is the last block which contains its
metadata. The `caboose` is the last known block which is pruned (that
contains partial data).

.. _Set_up_a_rolling_node:

Setting up a node in rolling mode
---------------------------------

To run a ``rolling`` node you can either use the command line arguments:

.. code-block:: console

   octez-node run --history-mode rolling

or use your configuration file as described in :doc:`here <node-configuration>`:

.. code-block:: json

   { "shell": {
       "history_mode": "rolling"
   }}

In ``rolling`` mode, the `caboose` is the genesis at its early state,
and then, it is updated to the last known block of the rolling
window. The `savepoint` is moved in accordance to the number of
configured additional cycles.

``$ tezos rpc get /chains/main/checkpoint``


.. _Set_up_an_archive_node:

Setting up a node in archive mode
---------------------------------

To run an ``archive`` node you can use the command line arguments:
``$ octez-node run --history-mode archive``

Or the configuration file:
``{ "shell": {"history_mode": "archive"} }``

If you want to start an ``archive`` node, it is now mandatory to pass
this argument the first time you launch your node. Indeed, there are
some restrictions when switching from one mode to another.

In ``archive`` mode, both the `savepoint` and `caboose` are located
down to the genesis.

.. _History_mode_additional_cycles:

Keeping additional cycles
-------------------------

When running a node in ``full`` or ``rolling`` mode, you have a full
access to the block information in a sliding window of
history. Indeed, at each new cycle, a garbage collection phase removes
the ledger state and the block metadata (operation receipts, rewards
updates, etc.) of blocks outside the offset of this sliding
window. Depending on the network, a minimum number of cycles are
kept. These cycles correspond to the ones above the last
allowed fork level, containing blocks subjects to a potential chain
reorganization (this minimal number of cycles is currently given by
the :ref:`preserved_cycles<ps_constants>` protocol parameter, which
on mainnet is currently set to 5 cycles). However, the
node is able to keep an additional number of cycles that is
configurable.

By default, 1 additional cycle is kept for both ``full`` and
``rolling`` nodes. It is possible to increase this parameter to keep
more history or, on the contrary, decrease it to reduce the storage
size. For example, it is possible to run a node with *5* additional
cycles. On mainnet, this would total *10 cycles* of complete history
(approximately four weeks), as we keep 5 cycles beyond the minimal
number of cycles, that is *5 + 5 = 10*.


When initializing your node on an empty storage, you may specify the
history mode and number of additional cycles using ``--history-mode
<HISTORY_MODE>:<NB_CYCLES>`` when running it. For example, running a
node with ``--history-mode rolling:5`` would allow full RPC queries of
the 10 previous cycles.


It is also possible to modify the number of additional cycles kept of
a previously configured node, see :ref:`Switch mode
restrictions<Switch_mode_restrictions>`. When updating the number of
additional cycles to keep on an already configured node, one must
consider that the change may require time. If the update aims to
shrink the number of additional cycles to keep, it is just a matter of
deleting some data, and the operation is performed instantaneously. If
the update aims to increase the number of additional cycles to keep,
one must consider that the switch will be complete only after waiting
for the end of the aforementioned number of cycles. Indeed, the
"cycles window" will be expanded as blocks/cycles are processed by the
node and it won't fetch blocks prior to the previous limit. For
example, switching from ``5`` to ``7`` cycles requires to wait ``2``
complete cycles to reach the target number of cycles to keep. On the
contrary, switching from ``7`` to ``5`` is instantaneous.

.. _Switch_mode_restrictions:

Switching between node's modes
------------------------------

It is possible to switch between history modes and/or to modify the
number of additional cycles. To do so, it is necessary to restart the
node with the desired history mode and add the flag
``--force-history-mode-switch``. This flag is required to prevent
erroneous history switches. Indeed, changing from one history mode to
an other can irremediably remove data from the storage. The history
mode switches must be manipulated with care.

However, as the different modes rely on different storage schemes,
there are some restrictions when switching from one mode to another.

+---------+---------+------+---------+
|From/to  | Archive | Full | Rolling |
+=========+=========+======+=========+
| Archive | X       | Yes  | Yes     |
+---------+---------+------+---------+
| Full    | Yes*    | Yes  | Yes     |
+---------+---------+------+---------+
| Rolling | No      | No   | Yes     |
+---------+---------+------+---------+

(*) Switching from a ``full`` node to an ``archive`` one is possible
using the ``reconstruct`` feature. To do so, run ``octez-node
reconstruct`` on your node. Note that the storage reconstruction is a
long process that, on the main network, may require more than a week to
complete. Reconstruction also requires a machine with at least 16GB of
memory (for the flattening of the context storage operation) and takes up
about 1TB of storage once completed.
