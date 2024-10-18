Running a DAL attester node
===========================

To attest the data published on the :doc:`DAL<./dal_overview>`, you need an active baker and node.
While you can run a DAL node without an active baker, when you do so, your account is not assigned DAL attestation rights and therefore your node can distribute data among other DAL nodes but cannot attest that it is available.
Adding a DAL node to a baker's setup improves the Tezos ecosystem because the DAL greatly expands the amount of data that Tezos can distribute without causing congestion on layer 1.

Before you begin, make sure that you have an instance of the ``octez-node`` binary running in operator mode.
For instructions on setting up the ``octez-node`` binary, see :doc:`Running Octez <../introduction/howtorun>`.

.. warning::

   For the most secure setup, run the DAL node and layer 1 node on different IP addresses; for more information, see :doc:`Bakers & the DAL <./dal_bakers>`.

Running the DAL node
--------------------

Follow these steps to run a DAL node along with a layer 1 node and a baker.

#. Verify that you have attestation rights by running this command, where ``MY_ADDRESS`` is your account's address (not its ``octez-client`` alias):

   .. code-block:: shell

      octez-client rpc get /chains/main/blocks/head/helpers/attestation_rights?delegate="$MY_ADDRESS"

#. Install the DAL trusted setup as described in :ref:`Install DAL trusted setup <setup_dal_crypto_params>`.

#. Initialize the DAL node by running its ``config init`` command.
   For example, this command initializes the DAL node with the address of a local ``octez-node`` instance on port 8732 and stores data in the default DAL node directory (``~/.tezos-dal-node``):

   .. code-block:: shell

      octez-dal-node config init --endpoint http://127.0.0.1:8732

   You can specify a custom directory to store the DAL node data in by using the ``--data-dir`` argument:

   .. code-block:: shell

      octez-dal-node config init --endpoint http://127.0.0.1:8732 --data-dir .tezos-dal-node

   You can specify parameters such as the RPC node in the ``config init`` command or in the ``run`` command.
   These commands have the same parameters. For information about them, run ``octez-dal-node config init --help`` or see :ref:`DAL node commands <dal-node-commands>`.

   At minimum, you must specify with the ``--endpoint`` parameter the URL of an RPC node that the DAL node can use.

#. Recommended: Ensure that the P2P port that the DAL node runs on is accessible from outside its system.

   By default, the DAL node accepts P2P connections on port 11732, but you can change the port and address that the node listens on by setting the ``--net-addr`` argument, as in ``--net-addr 0.0.0.0:11732``.
   Depending on your network, you may need to adapt your firewall rules or set up network address translation (NAT) to direct external traffic to the DAL node.
   For example, you might need to redirect external traffic on TCP port ``<external_port>`` to your node at ``<local_ip_address>:<port>`` where ``<local_ip_address>`` is the IP address of the node on your local network and ``<port>`` is the port given in the ``--net-addr`` argument.

   If ``<external_port>`` is different from ``<port>``, then you should set the public address of your node via its configuration or the CLI option ``--public-addr <external_ip_address>:<external_port>``.

   This setup assumes that ``<external_ip_address>`` is fixed and won't change during the lifetime of the node.

#. Start the DAL node by running its ``run`` command, passing the directory that you set in the ``config init`` command if you changed the default.
   You can also pass any other parameters that you did not set in that command:

   .. code-block:: shell

      octez-dal-node run --data-dir .tezos-dal-node

   Leave the DAL node process running.

#. In a new terminal window, start or restart a baking daemon as usual, but tell it to connect to the DAL node by passing the ``--dal-node`` argument with the host name and RPC port of the DAL node.
   The DAL node accepts RPC calls on port 10732 by default, so the command might look like this example, where ``<PROTO_HASH>`` is the short hash of the current protocol of the network:

   .. code-block:: shell

      octez-baker-<PROTO_HASH> run with local node "$HOME/.tezos-node" bob --liquidity-baking-toggle-vote pass --dal-node http://127.0.0.1:10732

   The baker daemon connects to the DAL node and attests to the availability of DAL data as well as its usual layer 1 baking function.

#. In a new terminal window, verify that the DAL node is running properly:

   #. Verify that the node is connected to the DAL by running this command:

      .. code-block:: shell

         curl http://localhost:10732/p2p/gossipsub/connections

      This command should show a list of other DAL nodes that the node is connected to.

   #. Verify that the node is connected to topics by running this command:

      .. code-block:: shell

         curl http://localhost:10732/p2p/gossipsub/topics

      This command should return topics in the form ``{"slot_index":<index>,"pkh":"<ADDRESS OF BAKER>"}`` to represent the topics that the node is subscribed to on the DAL peer-to-peer network.

   #. Verify that your baker is assigned to attest DAL shards by running this command and using the address of the baker:

      .. code-block:: shell

         octez-client rpc get /chains/main/blocks/head/context/dal/shards?delegates=$MY_ADDRESS

      The response should show the address and the indexes of multiple shards that the baker is assigned to attest.

      For more information about topics and shards, see :doc:`DAL overview <./dal_overview>`.

#. Verify that the node is connected to the network by going to the `Explorus <https://explorus.io/consensus_ops>`_ block explorer, selecting the network, going to the Consensus Ops tab, and looking up your baker.

   The **DAL attested/total published slots** column shows information about bakers' attestations.

   If the column for your baker shows a symbol that looks like a missing image or empty page, then either the baker did not have shards assign to it at the corresponding level or the DAL node is not connected to the network. If you have sufficient stake and you always see the missing image symbol, check the steps above and make sure that the DAL node is running correctly.

   If the baker is connected to the network, the column shows the ratio of slots attested by the baker to published slots.

   - If the numbers are equal, as in ``2/2``, the baker is attesting all published slots (the DAL node has seen all shards that are assigned to it).

   - If the first number is always 0, as in ``0/2``, the DAL node and baker may not be configured correctly. See the troubleshooting section below.

   - If the baker attests all published slots sometimes and other times attests zero or fewer slots, the DAL node may be running too slowly or failing to fetch the data in time. Verify that the node hardware is sufficient, using these `Hardware and bandwidth requirements for the Tezos DAL <https://forum.tezosagora.org/t/hardware-and-bandwidth-requirements-for-the-tezos-dal/6230>`_.

Now the DAL node is running and subscribed to the relevant topics.

Troubleshooting
---------------

Troubleshooting connections
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Follow these steps if the DAL node is running but not connected to the network:

#. Upgrade your installation of Octez to the latest version.
   The Octez DAL node version 20.3 has some issues that are resolved in later versions.

#. Check if the ``config.json`` file of the ``octez-node`` daemon that the DAL node is connected to has a field named ``network``.
   If there is a ``network`` field, update the node's configuration by running this command:

   .. code-block:: shell

      octez-node config update --network <network>

   Use ``mainnet``, ``ghostnet``, or ``sandbox`` as the value for the ``--network`` argument.

#. Verify that the node is connected to a bootstrap peer by running this command with the address and RPC port of your DAL node:

   .. code-block:: shell

      octez-client --endpoint http://127.0.0.1:10732 rpc get /p2p/gossipsub/connections | jq ".[].connection.bootstrap"

   At least one entry in the output should show ``true`` to indicate that the peer is a bootstrap node.
   If not, run the command a few more times over a one-minute interval.
   If you still see no entries that say ``true``, restart the DAL node.

#. If the problem persists, contact Octez developers on the `tezos-dev <https://tezos-dev.slack.com/>`_ Slack or the Tezos `Discord <https://discord.gg/tezos>`_.

Troubleshooting firewall/NAT issues
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

By default, the P2P port for a DAL node is 11732, but you can change it with the ``--net-addr`` argument.

If you want to use an external port different from the one specified in ``--net-addr``, use the ``--public-addr`` argument.
Currently, there is a limitation requiring you to know your public IP address to do this, though we plan to improve this in the future.

For both producers and bakers, it is essential to maintain good connectivity by ensuring that your node can receive connections:

- If you're behind a NAT, you must implement a forwarding rule.

- If you're behind a firewall, you must configure it to allow both incoming and outgoing connections on the P2P port.

- If you're not using the ``--public-addr`` argument, ensure that the NAT forwarding rule uses the same external and internal ports.
