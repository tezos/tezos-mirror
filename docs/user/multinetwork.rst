Connecting to a Network
=======================

Tezos is run on several networks, such as Mainnet (the main network)
and various :ref:`test Networks<test_networks>`. Some users may also want to run
their own networks for various reasons. Networks differ in various ways:

- they start from their own genesis block;

- they have different names so that each node can choose the network to connect to;

- they may run different protocols (or upgrade protocols at different moments);

- protocols may run with different :ref:`constants <protocol_constants>` (for instance, test networks move faster);

- they have different bootstrap peers (nodes that new nodes connect to initially);

- some networks may change the protocol without going through the regular voting process, via user-activated upgrades or user-activated protocol overrides.

The Octez node can be configured to connect to a given network when it is started.
By default, the node connects to Mainnet.
To connect to other networks, you can either use one of the
`Built-In Networks`_ or configure the node to connect to `Custom Networks`_.
See also `Alias Versus Explicit Configuration`_ for a discussion
regarding what happens when you update your node, in each case.

.. _test_networks:

Test Networks
-------------

Mainnet is the main Tezos network, but is not appropriate for testing.
A number of `test networks <https://teztnets.com>`__ are available to this end. Test networks usually run
with different :ref:`constants <protocol_constants>` to speed up the chain.

.. _faucet:

Each test network listed there also indicates a **faucet** delivering test tokens. Enter the public key hash of any test
account on the corresponding website to receive test tokens.

The faucet interface is meant for interactive use.
If you are looking for a scriptable alternative, check the programmatic `faucet frontend <https://github.com/tacoinfra/tezos-faucet>`__ which includes the ``get-tez`` tool.

.. _builtin_networks:

Built-In Networks
-----------------

The simplest way to select the network to connect to is to use the ``--network``
option for selecting a :ref:`test network<test_networks>` when you initialize your :doc:`node configuration <./node-configuration>`.

For instance, to run on Shadowtnet::

  octez-node config init --data-dir ~/tezos-shadownet --network shadownet
  octez-node identity generate --data-dir ~/tezos-shadownet
  octez-node run --data-dir ~/tezos-shadownet

.. note::
   Once initialized, the node remembers its network settings on subsequent runs
   and reconnects to the same network every time you run it. If you specify a
   different network when running the node again, it will refuse to start. In
   order to switch to a different network you need to either reinitialize it
   with a different data directory using the ``--data-dir`` option or remove
   everything from the existing data directory, which defaults to ``~/.tezos-node``
   (and also initialize again).

The ``--network`` option is not case-sensitive and can be used with
the following built-in networks:

- ``mainnet`` (this is the default)

- ``sandbox``

- ``shadownet``

- ``ghostnet``

If you did not initialize your node configuration, or if your configuration
file contains no ``network`` field, the node assumes you want to run Mainnet.
You can use the ``--network`` option with ``octez-node run`` to make sure
your node runs on the expected network. For instance, to make sure that
it runs on Shadownet::

  octez-node run --data-dir ~/tezos-shadownet --network shadownet

This command will fail with an error if the configured network is not Shadownet.
The node also displays the chain name (such as ``TEZOS_MAINNET``) when it starts.
Also mind opening the :doc:`RPC interface <../developer/rpc>` as appropriate.

The list of built-in networks is in :src:`src/lib_node_config/config_file.ml`.
Octez developers edit the ``builtin_blockchain_networks_with_tags`` variable in this file to
add or remove built-in networks.

Custom Networks
---------------

If the network you want to connect to is not in the list of built-in networks,
you need a corresponding network configuration file. There are several ways to
set that up. If you have an appropriate file, you can specify it with the ``--network``
argument when you initialize your node configuration (see above), and the node will load it. If you know a URL from which the file can be
downloaded, you can also specify it with ``--network``. The node will then
download the config automatically. The network configuration should be in JSON format,
containing an object matching the contents of the ``network`` field in
``config.json`` (see below for an example, and the :doc:`node configuration <./node-configuration>` documentation for reference).

.. note::
   The contents of the network configuration file will be saved in your node
   configuration file ``config.json``, so it won't be downloaded again on
   subsequent runs of the node.

Finally you can manually edit the main configuration file of the node (``config.json``).
Here is an example configuration file for Mainnet::

  {
    "p2p": {},
    "network": {
      "genesis": {
        "timestamp": "2018-06-30T16:07:32Z",
        "block": "BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2",
        "protocol": "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P"
      },
      "chain_name": "TEZOS_MAINNET",
      "old_chain_name": "TEZOS_BETANET_2018-06-30T16:07:32Z",
      "incompatible_chain_name": "INCOMPATIBLE",
      "sandboxed_chain_name": "SANDBOXED_TEZOS_MAINNET",
      "user_activated_upgrades": [
        {
          "level": 28082,
          "replacement_protocol": "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt"
        },
        {
          "level": 204761,
          "replacement_protocol": "PsddFKi32cMJ2qPjf43Qv5GDWLDPZb3T3bF6fLKiF5HtvHNU7aP"
        }
      ],
      "user_activated_protocol_overrides": [
        {
          "replaced_protocol": "PsBABY5HQTSkA4297zNHfsZNKtxULfL18y95qb3m53QJiXGmrbU",
          "replacement_protocol": "PsBabyM1eUXZseaJdmXFApDSBqj8YBfwELoxZHHW77EMcAbbwAS"
        },
        {
          "replaced_protocol": "PtEdoTezd3RHSC31mpxxo1npxFjoWWcFgQtxapi51Z8TLu6v6Uq",
          "replacement_protocol": "PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA"
        },
        {
          "replaced_protocol": "PtHangzHogokSuiMHemCuowEavgYTP8J5qQ9fQS793MHYFpCY3r",
          "replacement_protocol": "PtHangz2aRngywmSRGGvrcTyMbbdpWdpFKuS4uMWxg2RaH9i1qx"
        }
      ],
      "default_bootstrap_peers":
        [
         "boot.tzinit.org",
         "boot.tzboot.net",
         "boot.tzbeta.net"
       ]
    }
  }

This is equivalent to doing ``config init --network mainnet``, or using ``"network": "Mainnet"``
in the configuration file (or to doing nothing, as Mainnet is the default), except
that you will not automatically get updates to the list of bootstrap peers and
user-activated upgrades (see `Alias Versus Explicit Configuration`_).

- ``genesis`` is the description of the genesis block, i.e. the first block of the chain.
  Inspect the genesis block using ``octez-client rpc get /chains/main/blocks/0``
  to find these values.

- ``chain_name`` is the name of the network (nodes only talk to other nodes which use
  the same network name).

- ``old_chain_name`` is usually the same as ``chain_name``, except for networks
  that were renamed.

- ``incompatible_chain_name`` is a name which must be different from ``chain_name``
  and ``old_chain_name``. It is thus ensured to be incompatible. It is used for testing
  purposes.

- ``sandboxed_chain_name`` is the name of the network in sandbox mode. It can be the same
  as ``chain_name`` but it is safer to pick a different name.

- ``user_activated_upgrades`` is the list of past user-activated upgrades.
  Each item has a field ``level``, which is the level at which the protocol must
  be changed, and a field ``replacement_protocol``, which is the hash of the protocol
  to switch to.

- ``user_activated_protocol_overrides`` is the list of past user-activated protocol
  overrides. Each item has a field ``replaced_protocol`` and a field ``replacement_protocol``.
  Both are protocol hashes. If ``replaced_protocol`` is to be activated using on-chain
  voting, ``replacement_protocol`` is activated instead.

- ``default_bootstrap_peers`` is the list of addresses of default bootstrap peers.
  They are only used if ``p2p.bootstrap_peers`` is not present in the configuration file,
  and ``--no-bootstrap-peers`` is not given on the command-line.

Genesis Parameters
~~~~~~~~~~~~~~~~~~

In addition to the above fields, you can also specify custom genesis parameters.
That is, you can additionally specify the
activation key::

  {
    "p2p": {},
    "network": {
      "genesis": { ... },
      "genesis_parameters": {
        "context_key": "sandbox_parameter",
        "values": {
          "genesis_pubkey": "edpk..."
        }
      },
      ...
    }
  }

The ``genesis_parameters`` object contains:

- ``context_key``, the name of the key in the context part of the storage,
  whose value must be modified (if omitted, the default context key is
  ``sandbox_parameter``);

- ``values``, which contains the protocol parameters.

In the above example, we set the ``genesis_pubkey`` parameter of ``proto_genesis``.

Note that the genesis parameters that you specify in the configuration file
can be overridden by the ``--sandbox`` parameter on the command-line.
Similarly, if you are using a built-in network and if this built-in network
comes with genesis parameters, you can override them with ``--sandbox``.

Alias Versus Explicit Configuration
-----------------------------------

The previous sections explained two different ways to configure the network a node is connecting to:

- alias configuration: using the name (also called the "alias") of an existing, built-in network
- explicit configuration: explicitly specifying the parameters of the network, which can be an existing or a custom network.

When connecting to existing networks, both options may apply, so here are some useful explanations to inform your choice.

If you use alias configuration, the configuration file stores
the name of the network to connect to. For instance, if you configured it
to connect to Shadownet, it will contain something like::

  {
    "p2p": {},
    "network": "shadownet"
  }

For Mainnet, it would contain ``mainnet``, or nothing as this is actually the default.

When you update your node to new versions, built-in network parameters may
change. For instance, the list of bootstrap peers may be updated with
new addresses; new user-activated upgrades or user-activated protocol
overrides may be added. Because the configuration file only contains the name
of the network and not its parameters, it will automatically use the updated values.

However, if you use explicit configuration, the configuration file will
no longer contain an alias such as ``mainnet`` or ``shadownet``. Instead,
it will explicitly contain the list of bootstrap peers, user-activated upgrades
and user-activated protocol overrides that you specify. This means that when
you update your node, the updated values will not be used.

As a consequence, if you use explicit configuration, you need to update
its parameters yourself when you update your node, unless you wish to keep the old network parameters.
