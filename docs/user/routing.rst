Routing
=======

Octez nodes (including the Layer 1 node itself, the DAL node, the EVM node, and the Smart Rollup node) must be able to initiate connections outside their local network and accept connections from outside their local network.
This table shows the default ports that nodes accept connections and RPC requests from and the arguments that they support for routing configuration:

.. list-table::
  :header-rows: 1

  * - Node
    - Connections from other nodes
    - RPC requests
    - ``--public-addr``
    - ``--net-addr``
    - ``--rpc-addr``
    - ``--rpc-port``
    - ``--external-rpc-addr``
  * - Octez node
    - 9732
    - 8732
    - No
    - Yes
    - Yes
    - No
    - Yes
  * - DAL node
    - 11732
    - 10732
    - Yes
    - Yes
    - Yes
    - No
    - No
  * - EVM node
    - N/A
    - 8545
    - No
    - No
    - Yes
    - Yes
    - No
  * - Smart Rollup node
    - N/A
    - 8932
    - No
    - No
    - Yes
    - Yes
    - No

In simple setups with a single instance of each node, routing configuration is usually not necessary.
However, if you are using a load balancer or running multiple instances of the same node, you may need to configure port forwarding on your router and the ports and addresses that the nodes use.

These arguments are available on certain nodes to set the ports and addresses that the nodes use.
As shown in the table above, not all of these options are available on each node:

- The ``--public-addr`` argument sets the address and port at which other nodes can reach the node, such as if you are using a load balancer, firewall rule, or router to forward traffic to the node from another port.
  The node does not actually bind to this port or listen on it.

- The ``--net-addr`` argument sets the address and port that nodes listen for incoming connections on.
  For example, setting ``--net-addr 0.0.0.0:11733`` makes the node listen on port 11733 on all network interfaces.

- The ``--rpc-addr`` and ``--external-rpc-addr`` arguments sets the address on which the node accepts RPC requests from clients.
  The ``--rpc-addr`` argument is for the node itself and the ``--external-rpc-addr`` argument is for a secondary RPC server that runs on a process by itself, to reduce the strain on the node.

- The ``--rpc-port`` argument sets the port on which the node accepts RPC requests.

Additionally, you can restrict access to certain RPCs of the node with :ref:`Access Control Lists <node_access_control>`.
For more information about the RPC interface in general, see :doc:`JSON/RPC interface <../developer/rpc>`.

Ensuring that nodes are accessible
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Depending on your network, you may need to adapt your firewall rules or set up network address translation (NAT) to direct external traffic to the nodes.
For example, if you are running more than one DAL node, each node needs a unique socket (combination of IP address and port) to listen on.
In this case, use the ``--net-addr`` argument to set a different IP address and port for one of the nodes to listen on, as in ``--net-addr 0.0.0.0:11733`` to make the node listen on port 11733 on all network interfaces.
Then you must configure your firewall to permit incoming and outgoing TCP connections on port 11733.

If a firewall rule directs traffic from an external port that is different from the port that you set in the ``--net-addr`` argument, use the ``--public-addr`` argument to set the port from which the node can be reached by other nodes.
You may also need to set ``--public-addr`` if you are directing traffic from a load balancer to the DAL node.

.. _mapping_upnp:

Mapping ports with UPnP
~~~~~~~~~~~~~~~~~~~~~~~

Starting with :doc:`Octez v23 <../releases/version-23>`, the Octez node supports
mapping the port defined with the ``--listen-addr`` argument using UPnP. Note that this
feature is still experimental in Octez v23 and is done manually, but will become
automatic in later versions. Mapping this port should improve connectivity, and in
particular allow other nodes to open a connection with the Octez node.

Note that UPnP is generally not supported by professional networks and might
not be enabled by default in home networks.

The general workflow of using UPnP with Octez v23 is the following:

1. Ask for a redirection of the P2P port on any port:

   ::

      $ octez-node map-port --any-net-port

   The ``--any-net-port`` argument lets the gateway decide for a port to redirect octez-node's
   P2P port, and registers it in the configuration for ``advertised-net-port``::

      Redirecting <external_ip>:51397 to <octez-node_internal_ip>:9732


2. Update the lease of the redirection on a regular basis (generally
less than the lease, which by default is one week). This can be done by a cron
job, for example::

   0 0 * * */6 octez-node map-port

   Such a cron job will update the lease every 6 days. Note that ``--any-net-port``
   is not used, as the node already has an external port assigned, and it is read
   from the configuration.

.. warning::
  For port mapping to be working, the Octez node needs to be able to receive
  broadcast messages from the router. On certain setups, in particular using ``ufw``,
  broadcast messages are discarded and a rule must be added to accept them. For
  example::

     $ ufw allow in from <network mask>

  where ``network_mask`` is the local network mask, for example
  ``192.168.0.0/24``. Any ``Gateway not found`` error would be an indicator that
  the firewall is preventing the node to receive the router's messages.

  UPnP on some network might be available but not feature-complete. In particular,
  it might support mapping port but without the ability to map random ports. In
  such a case, remove ``--any-net-port`` and either use
  ``--advertised-net-port`` to choose an external port, or no option at all to try
  to bind the defualt P2P port.
