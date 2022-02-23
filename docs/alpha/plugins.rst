Protocol plugins
================

Protocol plugins implement extra APIs needed by the shell in order to interact with the economic protocol, beyond the one provided by the protocol environment.
This code is not strictly speaking part of the protocol code base, so this is not subject to on-chain governance (see :doc:`voting procedure <voting>`), but it is still protocol-dependent, which means that it may vary with different protocols.
For instance, the plugin code for protocol Alpha is located in file :src:`src/proto_alpha/lib_plugin/plugin.ml`.
Thus, a specific version is included in the Octez node for each protocol version (recall that a new release of Octez is usually delivered for each new protocol proposal, see :doc:`../releases/releases`)

So what kind of features may a protocol plugin provide?
For instance, protocol plugins do not define the context, or restrict the validity of operations.
In turn protocol plugins may, for example:

- be adjacent enough to the protocol code that it needs to have access to the protocol's internal representations for certain structures: e.g., it has some RPCs that can introspect on the protocol dependent content for certain operations;
- implement some common operations that are customized for each protocol (e.g., :ref:`prevalidator_filters`).

.. _prevalidator_filters_alpha:

Prevalidator filters
~~~~~~~~~~~~~~~~~~~~

The :ref:`prevalidator component <prevalidator_component>` of the shell is responsible for disseminating operations over the peer-to-peer network, that may be included in next blocks.
To prevent spam, the prevalidator implements built-in filtering mechanisms that limit to some extent the risk of flooding the network with invalid operations.
However, these selection mechanisms remain rather laxist to protect from DOS attacks.
In particular, a very affordable technique for attackers is based on flooding the network with, valid but useless, zero-fees operations.
This is why an additional and more flexible selection mechanism is included in the Octez node: *prevalidator filters*.

Prevalidator filters thus serve at further restricting the operations to propapate by the node to the network.
Filters are implemented as a node plugin and a specific filter is delivererd with each protocol version.
When the chain switches to a new protocol, the node installs its corresponding filter, *in lieu of* the filter of the previous protocol.

The prevalidator filter is based on restricting operations based on their associated fees, to reject "too cheap" or "zero-fees" operations.
However, the filters could also be used to restrict the propagation of consensus operations (e.g. endorsements).

The filter is tunable by several parameters, whose values can be retrieved and changed by users via the following RPC calls, respectively:

- ``rpc get /chains/<chain>/mempool/filter``
- ``rpc post /chains/<chain>/mempool/filter``

The following parameters can be thus inspected and modified:

- ``minimal_fees``: type ``int``, default ``100``
- ``minimal_nanotez_per_gas_unit``: type ``int``, default ``100``
- ``minimal_nanotez_per_byte``: type ``int``, default ``1000``
- ``allow_script_failure``: type ``bool``, default ``true``

For example, the following command modifies the ``minimal_fees`` parameter (and resets all the other parameters to their default values)::

   tezos-client rpc post /chains/main/mempool/filter with '{ "minimal_fees": "42" }'

See also:

- The `mempool plugin API <https://tezos.gitlab.io/api/odoc/_html/tezos-protocol-plugin-alpha/Tezos_protocol_plugin_alpha/Plugin/Mempool/index.html>`__
- MR :gl:`!3118` show defaults in RPC get mempool/filter
