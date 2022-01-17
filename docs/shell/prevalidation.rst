The Prevalidator
================

The *prevalidator* is responsible for determining which operations to
propagate over the gossip network. It uses the economic protocol to
classify each operation in order to decide whether the operation should
be propagated or not. 

The baker uses the prevalidator too, via the ``monitor_operations``
RPC, for selecting operations that can be included in the block being
crafted (if any), and for observing consensus operations/quorums (in
particular for the Tenderbake protocol).

The prevalidator maintains a set of operations with their associated
classifications. Each time the validation system switches to a new
head, operations' classification should be reconsidered, as the
previous ones may become invalid. This process is used for:

- Rejecting invalid operations.
- Reclassifying an operation that was not propagated but that may
  become valid after the arrival of a new valid block.
- Reclassifying a temporarily invalid operation.

The prevalidator aims to protect the network against DDoS attacks.
Consequently, it has to decide which operations are broadcast, and
which ones are kept in its bounded memory.


Prevalidator filtering mechanisms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The prevalidator implements several mechanisms to prevent DDoS
attacks. Some of them are protocol economic-dependent and are
implemented using filtering mechanisms that limit to some extent the
risk of flooding the network. The filtering can be done by looking at
the content of an operation, which the prevalidator cannot achieve
solely. So, it relies on filters defined in a :doc:`protocol-specific
plugin <../active/plugins>`.

The prevalidator implements three filtering mechanisms: a
``prefilter``, the ``precheck`` filter (starting from Octez version
12.0), and a ``postfilter``.

The ``prefilter`` is executed for each operation received from the
network and every time an operation needs to be reclassified (after
receiving a new block for example). This function should be quick to
execute and static: rejection is done solely based on the content of
the operation and already accepted operations, not taking into account
the state of the ledger.

Starting from Octez version 12.0, the ``precheck`` filter can be used
instead of ``applying_operation`` to classify operations, as follows:
If ``precheck`` cannot decide the classification
of an operation, the prevalidator uses ``apply_operation`` instead.
If an operation passes the ``precheck`` filter, or otherwise it has been successfully
applied, then the operation is propagated over the gossip network.

Currently, the ``precheck`` filter is only implemented for manager operations.
The prevalidator makes the assumption that it is faster to run than ``apply_operation``.
It can be disabled via the ``--disable-precheck`` node option.

The ``postfilter`` is executed on applied operations and can
be used to reject some of them based on their respective (application) receipts.
Since ``precheck`` is lighter and does not return receipts, prechecked operations
cannot be postfiltered.

A detailed description of the prevalidator filters is given in the dedicated plugin
page for the corresponding economic protocol (e.g., :doc:`../active/plugins` for the active protocol).

.. _operation_classification:

Operations classification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Octez prevalidator classifies an operation depending on the outcome
of its validity in the prevalidator's current context:

- ``Applied``: the operation was successfully applied on the current
  context of the prevalidator.
- ``Prechecked``: the operation was successfully prechecked on the current
  context of the prevalidator (so it can be propagated without being applied).
- ``Branch_delayed``: the operation could not be applied on the current
  context, but could be in the future.
- ``Branch_refused``: the operation could not be applied on the current
  context, but could be if a reorganisation happens.
- ``Outdated``: this operation will never be applicable in the future
  but was valid in the past.
- ``Refused``: There is no block on the current economic protocol that
  would accept this operation.

The prevalidator maintains a context built on top of the current
head. This context is updated when an operation is classified as
``Applied``. When a node switches to a new head, its prevalidation
context is reset accordingly.

Operations living in the prevalidator (already classified or not) can
be accessed via the ``pending_operations`` RPC.
