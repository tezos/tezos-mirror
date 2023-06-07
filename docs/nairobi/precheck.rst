Prechecking of manager operations
=================================

.. FIXME tezos/tezos#3938:

   This section doesn't make much sense after the pipelining project
   has plugged validate into the plugin for Lima. Parts of this
   section be integrated into plugin.rst, and the relevant definitions
   should point to the validation entry.

The prevalidator of Octez prevents the propagation of non-valid
operations by trying to apply (or execute) them.
However, executing
manager operations (like complex smart contract calls) can be
time-consuming.
To ensure a high throughput of the Tezos protocol, the
propagation of operations should be as fast as possible. This calls
for a lighter approach.

Prechecking manager operations implements such a lighter approach, by
discarding most invalid operations, without executing any of them.
This is possible because the validity of an operation (in the sense,
being able to include it in a block)
depends on its solvability (see below :ref:`solvability_nairobi`).

Fortunately, it turns out that deciding whether an operation is solvable can be achieved without
applying it.

However, restricting the propagation to solvable operations is not enough to
ensure resilience against DDoS attacks.
It does not ensure their validity in a direct successor of the current
head.
For instance, given two manager operations from the same source and
with the same counter, these operations can be solvable in the same
Tezos context ``C``, but none of them is solvable in the context
resulting from the application of the other operation on ``C``.
To mitigate this situation, we further restrict the propagation of
manager operations to at most one manager operation per manager per
block.
Note however that one can still inject successive operations with the
same counter using the ``replace-by-fee`` feature.


.. _solvability_nairobi:

Solvable operations
-------------------

The solvability of an operation depends on the content of the Tezos
context in which it is applied.


In the following, we consider the context kept by the prevalidator
(see :doc:`../shell/prevalidation`).
Checking operation solvability with this context is lighter than with
the contexts for block validation or block construction: it can be
decided without checking whether the gas announced by the operation
is below the remaining gas for the whole block.
Checking solvability consists in verifying that the
operation is well-formed and that fees can be paid: no execution
of operations (especially of smart-contract code) is done to determine
whether they would be applied in the next block with a success or a
fail status (as done by operation application).

Roughly, a manager operation is solvable when the following conditions hold:

- The manager can afford to pay the operation fees in the current prevalidator context;
- the operation's counter is the successor of the operation counter associated to
  the manager in the current context;
- the gas limit announced is below the gas limit per operation as
  defined by the economic protocol;
- for manager operations with Michelson parameters, the
  gas cost of deserialisation is under the gas limit announced by the
  operation; and
- the signature of the operation is valid.


Co-precheckable operations
--------------------------

Two manager operations are *compatible* if they correspond to distinct managers.

Two manager operations are *co-precheckable* in a context if they are
compatible and solvable in this context.

Considering two co-precheckable operations in a context, the
application of one of them preserves the solvability of the other.

This property extends to a set of manager operations if every pair of
distinct operations is co-precheckable.
In this case, the operations could be included in the next block in
any order, modulo block limits (eg. maximum gas, block size limit,
etc).

The precheck of a manger operation, with respect to a context and a
set of co-precheckable operations, consists in checking whether adding
the operation to the set preserves the operations of the set being
co-precheckable.

The prevalidator (see :doc:`../shell/prevalidation`) aims at
propagating operations that could be included in a direct successor
block of the current head.
It will propagate co-precheckable operations after checking their
solvability.

In conclusion, manager operation precheck is a simplification over the real
application of operations: the prevalidator checks whether each
manager operation would be valid in a direct successor of the
current head, as long as gas is available in that block.
In other terms, during prechecking of manager operations, they have no
effect on the prevalidator context.
