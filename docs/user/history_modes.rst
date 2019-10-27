History modes
-------------------

History modes allow a node to require less disk storage. Indeed,
depending on the chosen history mode, some parts of the complete chain
history can be deleted as they are not required anymore.

Three history modes are provided:

- **Full mode** (default mode)

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

  See how to :ref:`set up a full node<Set up a full node>`.

- **Rolling mode**

  This is the lightest mode as it only maintains a minimal rolling fragment of the
  chain data so the node can still validate new blocks and synchronize with the head.

  * Upsides

    + Only requires a minimal and bounded disk storage.
    + Can run on low resources architectures.
    + Can be bootstrapped within minutes.

  * Downsides

    - The node is not able to query block information of balances and
      staking rights before the current checkpoint.
    - The node does not help other nodes to bootstrap as it is not able to
      send the whole chain history.

  See how to :ref:`set up a rolling node<Set up a rolling node>`.

- **Archive**

  This is the heaviest mode as it keeps the whole chain data to be able to
  query any information stored on the chain since the genesis. It is
  particularly suitable for indexers or block explorer.

  * Upsides

    + The whole chain data is available.

  * Downsides

    - Consume an increasing and rather large amount and data storage.

  See how to :ref:`set up an archive node<Set up an archive node>`.

.. _Recap:

History modes in a nutshell
~~~~~~~~~~~~~~~~~~~~~~~~~~~

+---------+----------------+---------------------+--------------------+
|         | Storage amount | Suitable for bakers | Operations history |
+=========+================+=====================+====================+
| Archive | High           | Yes                 | Complete           |
+---------+----------------+---------------------+--------------------+
| Full    | Limited        | Yes                 | Complete           |
+---------+----------------+---------------------+--------------------+
| Rolling | Low            | Restricted*         | Last cycle         |
+---------+----------------+---------------------+--------------------+

(*) Not suitable for delegation services which needs more than one
previous cycle to compute rewards.

.. _Set up a full node:

Setting up a node in full mode
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To run a ``full`` node you can either use the command line arguments:

.. code-block:: console

   tezos-node run --history-mode full

or use your configuration file as described in :ref:`here <node-conf>`:

.. code-block:: json

   { "shell": {
       "history_mode": "full"
   }}

Note that, since the full mode is the default one, this configuration is optional.

You can then verify that your history mode is set to full by using the checkpoint RPC.

.. code-block:: console

   tezos-client rpc get /chains/main/checkpoint

.. code-block:: json

    { "block": { "some": "data" },
       "save_point": 4096, "caboose": 0, "history_mode": "full" }

In full mode, the save point corresponds to the checkpoint of the current chain.
It is the oldest block that contains all the data.
The caboose is the oldest pruned block (that contains partial data).

.. _Set up a rolling node:

Setting up a node in rolling mode
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To run a ``rolling`` node you can either use the command line arguments:

.. code-block:: console

   tezos-node run --history-mode experimental-rolling

or use your configuration file as described in :ref:`here <node-conf>`:

.. code-block:: json

   { "shell": {
       "history_mode": "experimental-rolling"
   }}

Please note that the ``rolling`` mode is still an experimental feature.

In this mode, the new checkpoint RPC will also give you the save point
(the oldest block that contains all the data) and caboose (the oldest
pruned block).
``$ tezos rpc get /chains/main/checkpoint``

.. _Set up an archive node:

Setting up a node in archive mode
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To run an ``archive`` node you can use the command line arguments:
``$ tezos-node run --history-mode archive``

Or the configuration file:
``{ "shell": {"history_mode": "archive"} }``

If you want to start an ``archive`` node, it is now mandatory to pass
this argument the first time you launch your node. Indeed, there are
some restrictions when switching from one mode to another.

.. _Switch mode restrictions:

Switching between node's modes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As the different modes relies on different storage schemes, there are
some restrictions when switching from one mode to another.

Going from ``archive`` to ``full`` or ``rolling`` or from ``full`` to
``rolling`` is allowed, as it is just dropping data. It is not allowed
to switch from the ``full`` or ``rolling`` to ``archive``, since the
last one would require to rebuild dropped archives.

+---------+---------+------+---------+
| From/To | Archive | Full | Rolling |
+=========+=========+======+=========+
| Archive | X       | Yes  | Yes     |
+---------+---------+------+---------+
| Full    | No      | X    | Yes     |
+---------+---------+------+---------+
| Rolling | No      | No   | X       |
+---------+---------+------+---------+
