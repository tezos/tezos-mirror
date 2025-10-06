Sandboxed mode
--------------

.. note::

   The following scripts and commands assume that you have built Octez from the source code. 
   See :ref:`Build from sources<build_from_sources>`.
   To switch to the master branch, use ``git checkout master`` instead of ``git checkout latest-release``.


To run a ‘localhost-only’ instance of a Tezos network, we provide two
helper scripts:

-  ``./src/bin_node/octez-sandboxed-node.sh``
-  ``./src/bin_client/octez-init-sandboxed-client.sh``

Run a sandboxed node
~~~~~~~~~~~~~~~~~~~~

For instance, if you want to run a local network with two nodes, in the
first terminal, the following command will initialize a node listening
for peers on port ``19731`` and listening for RPC on port ``18731``.

::

    ./src/bin_node/octez-sandboxed-node.sh 1 --connections 1

This node will store its data in a temporary directory
``/tmp/octez-node.xxxxxxxx`` which will be removed when the node is stopped.

The option ``--connections`` specifies the ideal number of peers the node tries
to connect to. Lowering the number of expected connections removes the spurious
“Too few connections” warnings. Set it to ``1`` for our two-nodes network (and
you would set it to ``0`` for a single-node network).

More information can be found in the :package-api:`api page of the octez node
config <octez-node-config/Octez_node_config/Shared_arg/index.html#type-t>`: or
by simply calling

::

   ./src/bin_node/octez-sandboxed-node.sh 1 --connections 1 --help

To launch the second node, run the following command in another terminal, and
it will listen on port ``19739`` and ``18739``:

::

    ./src/bin_node/octez-sandboxed-node.sh 9

You might replace ``1`` or ``9`` by any number in between if you want to
run more than two nodes.


Use the sandboxed client
~~~~~~~~~~~~~~~~~~~~~~~~

Once your node is running, open a new terminal and initialize the
“sandboxed” client data in a temporary directory:

::

    eval `./src/bin_client/octez-init-sandboxed-client.sh 1`

It will also define in the current shell session an alias ``octez-client``
preconfigured for communicating with the same-numbered node.

When you bootstrap a new network, the network is initialized with a
dummy economic protocol, called *genesis*. If you want to run the whole implemented
protocol, ``init-sandboxed-client`` also defines an
alias ``octez-activate-alpha``, that you need to execute once for
activating the whole network.
For instance:

::

    $ octez-client rpc get /chains/main/blocks/head/metadata
      { "protocol": "PrihK96nBAFSxVL1GLJTVhu9YnzkMFiBeuJRPA8NwuZVZCE1L6i",
        "next_protocol": "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im",
        ... }
    $ octez-activate-alpha
      Injected BMV9KnSPE1yw
    $ octez-client rpc get /chains/main/blocks/head/metadata
      { "protocol": "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im",
        "next_protocol": "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK",
        ... }

We now have the possibility to send transactions to the sandboxed network.
As the genesis block used to initialize the sandboxed network differs from the
one used in :ref:`test networks<test_networks>`, it is not possible to activate
accounts obtained from the faucet. However, we can use the
preconfigured accounts which can be listed with:

::

   $ octez-client list known addresses

     activator: tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV (unencrypted sk known)
     bootstrap5: tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv (unencrypted sk known)
     bootstrap4: tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv (unencrypted sk known)
     bootstrap3: tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU (unencrypted sk known)
     bootstrap2: tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN (unencrypted sk known)
     bootstrap1: tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx (unencrypted sk known)

We can run the following command to transfer some Tez from one account to
another:

::

   $ octez-client transfer 42 from bootstrap1 to bootstrap2 &
   ...
   Waiting for the operation to be included...

You will notice that this command doesn't terminate (hence the ``&``),
as usual it is waiting for the network to include the transaction in a
block.
Given that we are in a sandbox we need to bake a block ourselves and
we can do so with the following command:

::

   $ octez-client bake for --minimal-timestamp

If the previous transaction is valid, the operation is included in the
chain and the transfer terminates returning the usual receipt.
Note that the ``bake for`` command of the client is exclusively for
testing purposes, all baking should be done using the ``octez-baker``
binary.

We can now observe the transaction with:

::

   $ octez-client rpc get /chains/main/blocks/head
     { "protocol": "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK",
       ...
       "header":
         { "level": 2,
           ... },
       "operations":
         [ ...
           [ { ...
               "contents":
                 [ { "kind": "transaction",
                     "source": "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
                     "fee": "268", "counter": "2", "gas_limit": "169",
                     "storage_limit": "0", "amount": "42000000",
                     "destination": "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
                     ... } ] } ] ]

Tune protocol Alpha parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``octez-activate-alpha`` alias uses parameters from
``src/proto_alpha/parameters/sandbox-parameters.json`` to activate protocol
Alpha. It can be useful to tune these parameters when you need to debug
something, for example, change the number of blocks per cycle, the time between
blocks, etc.


Preserve data
~~~~~~~~~~~~~

If you want to preserve data and configuration files at the end of your run, you
can use the ``DATA_DIR`` environment variable.

::

    mkdir /tmp/tz-data
    DATA_DIR='/tmp/tz-data' ./src/bin_node/octez-sandboxed-node.sh 1 --connections 1

You can even provide a custom ``identity.json`` and ``config.json`` to the
sandboxed node by placing them in the data directory.

Baking multiple blocks
~~~~~~~~~~~~~~~~~~~~~~

To bake multiple blocks in a single command the ``-n <number_of_blocks>`` option can be used like

::

   $ octez-client bake for --minimal-timestamp -n 1_000

Once the current timestamp is caught up, blocks are produced every second or every ``minimal_block_delay`` set in the parameters file. To speed up the process the protocol can be activated in the past with

::

   $ octez-activate-alpha --timestamp "2024-01-01T00:00:00Z"

This increases the number of blocks needed to reach the current timestamp and speeds up the blocks production.
