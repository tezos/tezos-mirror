Multinetwork Node
=================

Tezos is run on several networks, such as Mainnet (the main network)
and various :ref:`Test Networks<test-networks>`. Some users may also want to run
their own networks for various reasons. Networks differ in various ways:

- they start from their own genesis block;

- they have different names so that nodes know not to talk to other networks;

- they may run (or have run) different protocols;

- protocols may run with different constants (for instance, test networks move faster);

- they have different bootstrap peers (nodes that new nodes connect to initially);

- they may have had user-activated upgrades or user-activated protocol overrides
  to change the protocol without going through the voting process.

The current ``master`` branch is capable of connecting to multiple networks,
including Mainnet and the test networks Carthagenet and Zeronet. By contrast,
the ``mainnet`` branch is only capable of connecting to Mainnet,
the ``carthagenet`` branch is only capable of connecting to Carthagenet,
and so on. The goal is to remove the need for such branches.
Currently, the multinetwork node has not been released, but we hope that
the next major Mainnet release is nothing but a tag on ``master``, or maybe
a very small branch.

By default, the multinetwork node connects to Mainnet.
To connect to other networks, you can either
`Select Network From Command-Line`_ to connect to a built-in network,
or configure the node to connect to `Custom Networks`_.

Select Network From Command-Line
--------------------------------

The simplest way to select the network to connect to is to use the new ``--network``
option. For instance, to run Carthagenet::

  tezos-node run --data-dir ~/tezos-carthagenet --network carthagenet

The ``--network`` option is non case-sensitive and can be used with
the following built-in networks:

- ``mainnet`` (this is the default)

- ``sandbox``

- ``carthagenet``

- ``zeronet``

- ``babylonnet`` (deprecated)

If you run ``tezos-node run`` or ``tezos-node snapshot import`` and your node has
no configuration file yet, a configuration file is created and stores the value of the
``--network`` option so that you do not have to specify ``--network`` everytime.
This also prevents you from accidentally running with the wrong network.

If you want to initialize a configuration file without running ``tezos-node run``
or ``tezos-node snapshot import``, you can use ``tezos-node config init``.
For instance, to configure your node to run Carthagenet assuming your
data directory is ``~/tezos-carthagenet``, use::

  tezos-node config init --data-dir ~/tezos-carthagenet --network carthagenet

Custom Networks
---------------

If the network you want to connect to is not in the list of built-in networks,
you can configure a custom network in the configuration file.
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
        }
      ],
      "default_bootstrap_peers": [ "boot.tzbeta.net" ]
    }
  }

This is equivalent to using ``--network mainnet``, or ``"network": "Mainnet"`` in the
configuration file (or to doing nothing, as Mainnet is the default).

- ``genesis`` is the description of the genesis block, i.e. the first block of the chain.
  Inspect the genesis block using ``tezos-client rpc get /chains/main/blocks/0``
  to find these values.

- ``chain_name`` is the name of the network (nodes only talk to other nodes which use
  the same network name).

- ``old_chain_name`` is usually the same as ``chain_name``, except for networks
  which were renamed.

- ``incompatible_chain_name`` is a name which must be different than ``chain_name``
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
------------------

In addition to the above fields, you can also specify custom genesis parameters.
For instance, if your genesis protocol is ``proto_genesis``, you can specify the
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

Development
-----------

The list of built-in networks is in ``src/bin_node/node_config_file.ml``.
Edit the ``builtin_blockchain_networks_with_tags`` variable in this file to
add or remove built-in networks.

To be able to connect to multiple networks without having to download the protocols,
and to provide all the relevant baker / endorser / accuser binaries, all current and
past protocols are compiled and linked. This means that if you patch the client commands
for a protocol, you should patch the other protocols as well (at least the ones which
are still in use).
