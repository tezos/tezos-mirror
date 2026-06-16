Protocol Plugins
================

This document describes the protocol plugins implemented in Octez.

Protocol-specific shell plugins, simply called protocol plugins,
implement extra APIs needed by the shell in order to interact with the
economic protocol, beyond the one provided by the :doc:`protocol
environment <../shell/protocol_environment>`.
This code is not strictly speaking part of the protocol
code base, so this is not subject to on-chain governance (see
:doc:`voting procedure <voting>`), but it is still protocol-dependent,
which means that it may vary with different protocols. For instance,
the plugin code for protocol Alpha is located in file
:src:`src/proto_024_PtTALLiN/lib_plugin/plugin.ml`. Thus, a specific version
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
  to reason on timestamps (see :ref:`consensus_filter_tallinn`);
- preserve the opacity/abstraction barrier of the protocol's internal data
  by performing computations on internal data without revealing it:
  e.g., there are some RPCs that can introspect the protocol-dependent
  content for certain operations;
- implement some common operations that are customized for each
  protocol (e.g., :ref:`prevalidator_filters_tallinn`).

.. _prevalidator_filters:
.. _prevalidator_filters_tallinn:

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

The interface of the prevalidator plugin is described at the
:package-api:`mempool plugin API
<octez-protocol-alpha-libs/Tezos_protocol_plugin_alpha/Mempool/index.html>`. The
following filtering strategies are implemented in the
:package-api:`pre_filter<octez-protocol-alpha-libs/Tezos_protocol_plugin_alpha/Mempool/index.html#val-pre_filter>`.

.. _fees_filter:
.. _fees_filter_tallinn:

Fees filter
...........

A very affordable technique for attackers is based on flooding the
network with, valid but useless, zero-fees operations. This is why the
prevalidator filter currently restricts operations based on their
associated fees, to reject "too cheap" or "zero-fees" operations. This
can be configured via the ``minimal_fees``,
``minimal_nanotez_per_gas_unit`` and ``minimal_nanotez_per_byte`` (see
:ref:`filter RPCs<active_filter_rpc_tallinn>`) parameters of the filter
configuration of your node.

.. _consensus_filter:
.. _consensus_filter_tallinn:

Consensus filter
................

To prevent the network from being flooded by valid (pre)attestations regarding
fanciful future levels and rounds, the plugin provides a filter that narrows down
the number of consensus operations declared as valid based on a timestamp
heuristic.

This filter will classify a consensus operation as ``Branch_refused`` if the
operation concerns a level and round combination that is far-fetched in the
future in regard to the latest proposal predecessor and the current timestamp.
It can be configured via the ``clock_drift`` (see :ref:`filter
RPCs<active_filter_rpc_tallinn>`) parameter of the filter configuration of your
node.

Operations prioritization and ordering
......................................

In addition to quick filtering of undesired operations, the
``prefilter`` provides a priority for each successfully filtered operation.
Concretely, the priority is either ``High``, ``Medium`` or ``Low`` in the
current implementation, depending on the :ref:`validation
pass<validation_passes_tallinn>`. Some extra information (like the fees, or the
gas/fees ratio of manager operations) are also provided along the priorities to
enable fine-grained operations ordering. This extra information is similar to
the one used by the baker's operations selection mechanism, that decides which
operations will be included in the next block.

.. _active_filter_rpc:
.. _active_filter_rpc_tallinn:

Filters RPCs
------------

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
- ``max_operations`` : type ``int``, default ``10_000``
- ``max_total_bytes`` : type ``int``, default ``10_000_000``
- ``replace_by_fee_factor`` : type ``rational``, default ``21/20`` (ie. ``1.05%``)

For example, each command below modifies the provided parameter and resets all
the others to their default values::

   octez-client rpc post /chains/main/mempool/filter with '{ "minimal_fees": "42" }'
   octez-client rpc post /chains/main/mempool/filter with '{ "replace_by_fee_factor": [ "23", "20" ] }'
   octez-client rpc post /chains/main/mempool/filter with '{ "max_operations": 7500 }'

Changing filters default configuration
......................................

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
