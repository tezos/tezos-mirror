Protocol Plugins
================

This document describes the protocol plugins implemented in Octez.

Protocol-specific shell plugins, simply called protocol plugins,
implement extra APIs needed by the shell in order to interact with the
economic protocol, beyond the one provided by the :doc:`protocol
environment <../developer/protocol_environment>`.
This code is not strictly speaking part of the protocol
code base, so this is not subject to on-chain governance (see
:doc:`voting procedure <voting>`), but it is still protocol-dependent,
which means that it may vary with different protocols. For instance,
the plugin code for protocol Alpha is located in file
:src:`src/proto_alpha/lib_plugin/plugin.ml`. Thus, a specific version
is included in the Octez node for each protocol version (recall that a
new release of Octez is usually delivered for each new protocol
proposal, see :doc:`../releases/releases`)

In contrast to the Octez code, plugins are protocol-specific and as
such, know how to read the content of an operation.

So what kind of features may a protocol plugin provide? For instance,
protocol plugins do not define the context, or restrict the validity
of operations.
In turn protocol plugins may, for example:

- perform protocol-dependent computations that require data not available
  in the amendable part of the protocol like accessing the current time
  to reason on timestamps (see :ref:`consensus_filter_alpha`);
- preserve the opacity/abstraction barrier of the protocol's internal data
  by performing computations on internal data without revealing it:
  e.g., there are some RPCs that can introspect the protocol-dependent
  content for certain operations;
- implement some common operations that are customized for each
  protocol (e.g., :ref:`prevalidator_filters_alpha`).

.. _prevalidator_filters_alpha:

Prevalidator filters
--------------------

**Prevalidator filters**, which are detailed in the rest of this page, enable the node to discard some
operations (that cannot be included in the next block) faster and
restrict the operations it propagates to the network.
Filters are implemented as a node plugin and a specific filter is
delivered with each protocol version. When the chain switches to a new
protocol, the node installs its corresponding filters, *in lieu of*
the filters of the previous protocol. Notice that prevalidator filters are not
mandatory, their absence does not break the Tezos blockchain protocol.

The interface of the prevalidator plugin is described at the `mempool plugin API
<https://tezos.gitlab.io/api/odoc/_html/tezos-protocol-plugin-alpha/Tezos_protocol_plugin_alpha/Plugin/Mempool/index.html>`__

The different kinds of prevalidator filters are described below.

.. _fees_filter_alpha:

Fees filter
...........

A very affordable technique for attackers is based on flooding the
network with, valid but useless, zero-fees operations. This is why the
prevalidator filter currently restricts operations based on their
associated fees, to reject "too cheap" or "zero-fees" operations. This
can be configured via the ``minimal_fees``,
``minimal_nanotez_per_gas_unit`` and ``minimal_nanotez_per_byte`` (see
:ref:`filter RPCs<active_filter_rpc_alpha>`) parameters of the filter
configuration of your node.

This filtering strategy is implemented in the ``prefilter`` (see
:doc:`../shell/prevalidation`).

.. _consensus_filter_alpha:

Consensus filter
................

For technical reasons, the economic protocol cannot reject consensus
operations based on their timestamp. Consequently, many
consensus operations are declared as valid by the economic protocol
while being timestamped too early or too late.
The plugin filter aims to narrow down the number of consensus
operations declared as valid based on several heuristics. In
particular, the filter can use the current timestamp while the
protocol cannot.

This filter classifies a consensus operation according to the current
block proposal as follows:

- ``Outdated`` if it concerns a previous head at a previous level,
- ``Branch_refused`` if it concerns a previous head at the same level
  but in the previous round,
- ``Branch_delayed`` if it concerns a round in the future.


This filtering strategy is implemented in the ``prefilter`` (see
:doc:`../shell/prevalidation`).



Prechecking of manager operations
.................................

The aim of the ``precheck`` filter is to avoid fully executing manager operations
before deciding whether to gossip them to the network.

The detailed description of this feature is given in
:doc:`./precheck`. For operations other than manager operations, the
``precheck`` filter is a no-op, which entails that these operations need to be
fully executed to decide their propagation (see :doc:`../shell/prevalidation`).


One operation manager per manager per block
...........................................

The mempool filters, ``prefilter``, ``precheck`` and ``postfilter``
also ensure that, since the last head update (the last valid block which
increased the chain's fitness), only one operation per manager is propagated.
All other received operations originating from the same manager will be classified
as ``Branch_delayed`` and will not be propagated.

This criterion is used only by the prevalidator to decide the propagation of
operations. A baker can still include several operations originating from the same
manager in a single block, provided that it gets them in time (note that they can be
propagated by nodes using different versions or implementations).

Alternatively, a user can inject an operation with the same
manager and the same counter, but with a higher fee to replace an already existing
operation in the prevalidator. Only one of the two operations will be eventually
included in a block. To be able to replace the first operation, the fee and the
"fee/gas limit" ratio of the second one is supposed to be higher than the first's
by a factor (currently fixed to 5%). In case of successful replacement, the old
operation is re-classified as ``Outdated``.

.. _active_filter_rpc_alpha:

Filters RPCs
~~~~~~~~~~~~

Filters are tunable by several parameters, whose values can be
retrieved and changed by users via the following RPC calls,
respectively:

- ``rpc get /chains/<chain>/mempool/filter``
- ``rpc post /chains/<chain>/mempool/filter``

The following parameters can be thus inspected and modified:

- ``minimal_fees``: type ``int``, default ``100``
- ``minimal_nanotez_per_gas_unit``: type ``int``, default ``100``
- ``minimal_nanotez_per_byte``: type ``int``, default ``1000``
- ``allow_script_failure``: type ``bool``, default ``true``
- ``clock_drift`` : type ``Period.t option``, default ``None``

For example, the following command modifies the ``minimal_fees``
parameter (and resets all the other parameters to their default
values)::

   tezos-client rpc post /chains/main/mempool/filter with '{ "minimal_fees": "42" }'

Changing filters default configuration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Changing filters configuration may not have the expected outcome.
Because the configuration is only changed on your node, and not on all
the gossip network nodes, this will only impact how operations of your
nodes are propagated. For example, assuming there are three nodes
``A``, ``B`` and ``C`` such that ``A`` is connected to ``B`` and ``B``
is connected to ``C``. Assume that ``A`` modifies its filter
configuration so that ``minimal_fees`` is now ``0``. Then ``A`` may
propagate an operation with ``0`` fee to ``B``. However, because ``B``
has the default filter configuration, this operation will not be
propagated to ``C`` (so ``C`` may never see it).
