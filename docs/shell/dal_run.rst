Running a DAL attester node
===========================

To attest the data published on the :doc:`DAL<./dal_overview>`, you need an active baker and node.
While you can run a DAL node without an active baker, when you do so, your account is not assigned DAL attestation rights and therefore your node can distribute data among other DAL nodes but cannot attest that it is available.
Adding a DAL node to a baker's setup improves the Tezos ecosystem because the DAL greatly expands the amount of data that Tezos can distribute without causing congestion on Layer 1.

Therefore, we assume here you have installed Octez, see :doc:`../introduction/howtoget`.
Make also sure that you have an instance of the ``octez-node`` binary running, as explained in :doc:`../user/setup-node`.

Connecting to other nodes
-------------------------

The DAL node needs the address of at least one already running DAL node to connect to.

If the ``octez-node`` binary was started with the ``--network <network>`` argument, the DAL node can get the addresses of bootstrap nodes on the specified network from the node's configuration via the ``/config/network/dal`` RPC endpoint.
These bootstrap nodes do not propagate DAL data, but instead provide entry points for other DAL nodes to become part of the network.

If the ``octez-node`` binary was not started with the ``--network <network>`` argument, the bootstrap nodes are not running, or you are setting up a closed network, you may need to run your own DAL node in the ``bootstrap`` profile or manually connect the new DAL node to an existing DAL node via the ``--peers`` argument before you can run a DAL node in the ``attester`` profile with an active baker.

Running the DAL node
--------------------

Before anything else, make sure your machine satisfies the hardware requirements at :doc:`dal_node`.

Follow these steps to run a DAL node along with a Layer 1 node and a baker.

#. **If** you plan to run the DAL node in **operator or observer** :ref:`profiles <dal_profiles>`, install the DAL trusted setup as described in section :ref:`setup_dal_crypto_params` (**NB:** just that section, not the rest of the page such as compiling sources, etc.).

#. Initialize the DAL node by running its ``config init`` command, passing the address of a local ``octez-node`` instance and your attester's address.
   For example, this command initializes the DAL node with the address of a local ``octez-node`` instance on port 8732 and stores data in the default DAL node directory (``~/.tezos-dal-node``):

   .. code-block:: shell

      octez-dal-node config init --endpoint http://127.0.0.1:8732 --attester-profiles="$MY_ADDRESS"

   where ``$MY_ADDRESS`` is the baking key address (not the alias registered with ``octez-client``).

   You can specify a custom directory to store the DAL node data in by using the ``--data-dir`` argument:

   .. code-block:: shell

      octez-dal-node config init --endpoint http://127.0.0.1:8732 --attester-profiles="$MY_ADDRESS" --data-dir my-attester-tezos-dal-node

   If you are switching from running the DAL node on a different network, use an empty directory instead of re-using the directory from another network.

   You can specify parameters such as the RPC node in the ``config init`` command or in the ``run`` command.
   These commands have the same parameters. For information about them, run ``octez-dal-node config init --help`` or see :ref:`DAL node commands <dal-node-commands>`.

   At minimum, you must specify with the ``--endpoint`` parameter the URL of an RPC node that the DAL node can use.

   Check the section above on connecting to other nodes to make sure that your new DAL node can connect to other peers on the DAL P2P network.
   If you need to explicitly connect your node to running DAL nodes, pass the ``--peers`` argument with a comma-separated list of DAL node host names and ports.
   The default P2P port is 11732, so the argument might look like this: ``--peers=host1.example.com:11732,host2.example.com:11732``.

#. Ensure that the P2P port that the DAL node runs on is accessible from outside its local network.

   DAL nodes must be able to initiate connections outside their local network and accept connections from outside their local network.
   By default, the DAL node accepts P2P connections on port 11732, but you can change the address and port that the node listens on by setting the ``--net-addr`` argument.
   In simple setups with a single DAL node, routing configuration is usually not necessary.

   For more information, see :doc:`Routing <../user/routing>`.

#. Start the DAL node by running its ``run`` command, passing the directory that you set in the ``config init`` command if you changed the default.
   You can also pass any other parameters that you did not set in that command:

   .. code-block:: shell

      octez-dal-node run --data-dir .tezos-dal-node

   Leave the DAL node process running.

#. In a new terminal window, start or restart a baking daemon as usual, but tell it to connect to the DAL node by passing the ``--dal-node`` argument with the host name and RPC port of the DAL node.
   The DAL node accepts RPC calls on port 10732 by default, so the command might look like this example:

   .. code-block:: shell

      octez-baker run with local node "$HOME/.tezos-node" bob --liquidity-baking-toggle-vote pass --dal-node http://127.0.0.1:10732

   The baker daemon connects to the DAL node and attests to the availability of DAL data as well as its usual Layer 1 baking function.

#. In a new terminal window, verify that your baking daemon has attestation rights allocated for the current cycle, by running:

   .. code-block:: shell

      octez-client rpc get "/chains/main/blocks/head" | jq '.metadata.level_info.cycle'
      octez-client rpc get "/chains/main/blocks/head/helpers/attestation_rights?delegate=$MY_ADDRESS&cycle=<current-cycle>"

   Be aware that the last command may take several minutes to execute if it returns a long list of rights.
   In turn, if the previous command reports no attestation rights (``[]``), you may have to register as a delegate or re-activate your delegate and wait for a few cycles to get some rights (see :ref:`DelegateRegistration`).

#. Verify that the DAL node is running properly:

   #. Verify that the node is connected to other DAL nodes by running this command:

      .. code-block:: shell

         curl http://localhost:10732/p2p/points/info?connected

      This command should show a list of other DAL nodes that the node is connected to, as in this example:

      .. code-block:: json

         [
           {
             "point": "46.137.127.32:11732",
             "info": {
               "trusted": true,
               "state": {
                 "event_kind": "running",
                 "p2p_peer_id": "idrpUzezw7VJ4NU6phQYuxh88RiU1t"
               },
               "p2p_peer_id": "idrpUzezw7VJ4NU6phQYuxh88RiU1t",
               "last_established_connection": [
                 "idrpUzezw7VJ4NU6phQYuxh88RiU1t",
                 "2024-10-24T15:02:31.549-00:00"
               ],
               "last_seen": [
                 "idrpUzezw7VJ4NU6phQYuxh88RiU1t",
                 "2024-10-24T15:02:31.549-00:00"
               ]
             }
           },
           {
             "point": "52.31.26.230:11732",
             "info": {
               "trusted": true,
               "state": {
                 "event_kind": "running",
                 "p2p_peer_id": "idqrcQybXbKwWk42bn1XjeZ33xgduC"
               },
               "p2p_peer_id": "idqrcQybXbKwWk42bn1XjeZ33xgduC",
               "last_established_connection": [
                 "idqrcQybXbKwWk42bn1XjeZ33xgduC",
                 "2024-10-24T15:02:31.666-00:00"
               ],
               "last_seen": [
                 "idqrcQybXbKwWk42bn1XjeZ33xgduC",
                 "2024-10-24T15:02:31.666-00:00"
               ]
             }
           }
         ]

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

#. Check if the ``config.json`` file of the ``octez-node`` daemon that the DAL node is connected to has a field named ``network``.
   If there is a ``network`` field, update the node's configuration by running this command:

   .. code-block:: shell

      octez-node config update --network <network>

   Use ``mainnet``, ``shadownet``, or ``sandbox`` as the value for the ``--network`` argument.

#. Verify that the node is connected to a bootstrap peer by running this command with the address and RPC port of your DAL node:

   .. code-block:: shell

      octez-client --endpoint http://127.0.0.1:10732 rpc get /p2p/gossipsub/connections | jq ".[].connection.bootstrap"

   At least one entry in the output should show ``true`` to indicate that the peer is a bootstrap node.
   If not, run the command a few more times over a one-minute interval.
   If you still see no entries that say ``true``, restart the DAL node.

#. Verify that the baker can get information about the slots that are available to attest by running this command, where ``$MY_ADDRESS`` is your baker's account address and ``<level>`` is a recent level:

   .. code-block:: shell

      curl -v "http://127.0.0.1:10732/profiles/$MY_ADDRESS/attested_levels/<level>/attestable_slots"

   The baker uses this request to get a list of the attestable slots from the DAL node, that is, the slots for which the node has all the shards assigned to the baker.
   If the response is an HTTP error, the error code may help you determine why the baker cannot get the necessary information from the DAL node.

   For example, the following response shows that the DAL node is providing information about which slots are attestable, even though none of the slots at this particular level are attestable.

   .. code-block:: json

      {"kind":"attestable_slots_set","attestable_slots_set":[false,false,false,false,false,false,false,false,false,false,true,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false],"published_level":9290818}

   If the baker is not currently assigned any shards, the response may be ``{"kind":"not_in_committee"}``, which also means that the baker can get information from the DAL node.

#. If the problem persists, contact Octez developers on the `tezos-dev <https://tezos-dev.slack.com/>`_ Slack or the Tezos `Discord <https://discord.com/invite/tezos>`_.

Troubleshooting firewall/NAT issues
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

By default, the P2P port for a DAL node is 11732, but you can change it with the ``--net-addr`` argument.

If you want to use an external port different from the one specified in ``--net-addr``, use the ``--public-addr`` argument.
Currently, there is a limitation requiring you to know your public IP address to do this, though we plan to improve this in the future.

For both producers and bakers, it is essential to maintain good connectivity by ensuring that your node can receive connections:

- If you're behind a NAT, you must implement a forwarding rule.

- If you're behind a firewall, you must configure it to allow both incoming and outgoing connections on the P2P port.

- If you're not using the ``--public-addr`` argument, ensure that the NAT forwarding rule uses the same external and internal ports.

- If you are running two DAL nodes on a network that uses NAT, set up the NAT rules to forward the P2P ports of the DAL nodes to different public ports.

DAL and the Ledger
^^^^^^^^^^^^^^^^^^

If you are using a Ledger device, make sure it runs a version of the `Tezos baking app <https://github.com/trilitech/ledger-app-tezos-baking>`__ that supports the DAL, such as version 2.5.0 or later.
