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

The interface of the prevalidator plugin is described at the :package-api:`mempool plugin API
<tezos-base/Tezos_base/Mempool/index.html>`.

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

.. _precheck_filter_alpha:

Prechecking of manager operations
.................................

.. FIXME tezos/tezos#3938:

   This section doesn't make much sense after the pipelining project
   has plugged validate into the plugin for Lima. Parts of this
   section be integrated into plugin.rst, and the relevant definitions
   should point to the validation entry.

The aim of the ``precheck`` filter is to avoid fully executing manager operations
before deciding whether to gossip them to the network.

The detailed description of this feature is given in
:doc:`./precheck`. For operations other than manager operations, the
``precheck`` filter is a no-op, which entails that these operations need to be
fully executed to decide their propagation (see :doc:`../shell/prevalidation`).


One manager operation per manager per block
...........................................

The mempool filters, ``prefilter``, ``precheck`` and ``postfilter``
also ensure that, since the last head update (the last valid block which
increased the chain's fitness), only one operation per manager is propagated.
All other received operations originating from the same manager will be classified
as ``Branch_delayed`` and will not be propagated.

Alternatively, a user can inject an operation with the same
manager and the same counter, but with a higher fee to replace an already existing
operation in the prevalidator. Only one of the two operations will be eventually
included in a block. To be able to replace the first operation, the fee and the
"fee/gas limit" ratio of the second one is supposed to be higher than the first's
by a factor (currently fixed to 5%). In case of successful replacement, the old
operation is re-classified as ``Outdated``.

Concretely, a user can replace a successfully prechecked manager operation in the
mempool, with the help of ``octez-client``, using two methods :

- manually provide a higher fee to bump the "fee/gas limit" ratio by at least 5% for the new
  operation,
- via option ``--replace``: In this case, ``octez-client`` will automatically
  compute the minimal amount of fee for the second operation to be able to
  replace the one in the mempool.



Operations prioritization and ordering
......................................


In addition to quick detection of operations that have no chance to be
prechecked or applied in the current context, the mempool's ``prefilter`` provides
a priority for each successfully filtered operation. Concretely, the priority is
either ``High``, ``Medium`` or ``Low`` in the current implementation, depending
on the :ref:`validation pass<validation_passes_alpha>`.Some extra information (like the fees, or the gas/fees
ratio of manager operations) are also provided along the priorities to enable
fine-grained operations ordering.
This extra information is similar to the one used by the baker's
operations selection mechanism, that decides which operations will be included
in the next block.


Bounding the number of propagated manager operations
.....................................................

Up to Hangzhou protocol (see :doc:`../protocols/011_hangzhou`), the protocol plugin
did not implement ``precheck``, so the prevalidator exclusively relies on ``apply_operation``
to classify manager operations. As a consequence, it could also check their
total gas consumption, and thus, naturally limit the number of successfully
applied/propagated operations.

Starting with Ithaca protocol (see :doc:`../protocols/012_ithaca`), the plugin
implements a lightweight classification function, called ``precheck``, that
doesn't check the total gas consumption. So with this modication and those of
Octez 12.0, the prevalidator, would propagate any succesfully prevalidated
operation. In order to protect nodes from potential DDoS, a new mechanism has
been added in the plugin to bound the number of successfully prechecked
operations. This mechanism works as follows:

- Advertise the *best* ``N`` successfully prechecked manager operations
  (where "best" is w.r.t. the priority described above, and N is a tunable parameter)
  found in the set of pending operations to the network after a new head is
  chosen and operations' classification reset. All other pending operations that
  should have been prechecked are instead classified as ``Branch_delayed``;
- Once the limit ``N`` is reached, the node may still receive additional manager
  operations (via the network or RPC injection) with higher priorities than
  those previously prechecked/advertised. Any such operation that is
  successfully prechecked is advertised, and, in turn, the previously prechecked/advertised
  manager operation with the lowest priority (not necessarily from the same
  source) is reclassified as ``Branch_delayed``.

The default value of the parameter ``N`` is chosen such that a node will always propagate enough
manager operations to allow the next baker to produce a filled block (if there are
enough operations in the network). Its value is currently fixed to 5000.
Indeed, the total size occupied by manager operations in a
block is currently bounded by 512 `KiB <https://en.wikipedia.org/wiki/Kilobyte>`_, and ``unset deposits limit`` seems
to be the smallest manager operation, with 126 Bytes, so there are at most
512 * 1024 / 126 = 4161 manager operations per block.


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
- ``max_prechecked_manager_operations`` : type ``int``, default ``5000``
- ``replace_by_fee_factor`` : type ``rational``, default ``21/20`` (ie. ``1.05%``)

For example, each command below modifies the provided parameter and resets all
the others to their default values::

   octez-client rpc post /chains/main/mempool/filter with '{ "minimal_fees": "42" }'
   octez-client rpc post /chains/main/mempool/filter with '{ "replace_by_fee_factor": [ "23", "20" ] }'
   octez-client rpc post /chains/main/mempool/filter with '{ "max_prechecked_manager_operations": 7500 }'

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
