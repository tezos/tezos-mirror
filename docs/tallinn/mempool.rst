Mempool
=======

The economic protocol provides a :package-api:`protocol-side mempool
module<tezos-protocol-024-PtTALLiN/Tezos_raw_protocol_024_PtTALLiN/Mempool_validation/index.html>`
data structure intended for use by the shell prevalidator (see
:doc:`../shell/prevalidation`) and by ``octez-baker`` to incrementally accumulate operations
that can be safely used to bake a new block.

It ensures that :

- Every operation contained are :ref:`valid<operation_validity_tallinn>`;

- every operation are :ref:`co-valid<co-valid_operations_tallinn>`: they can be
  safely included in a block in any arbitrary order, meaning operations commute;

- the merging of two mempools also maintains the aforementioned
  properties.

The protocol leverages the :ref:`partial construction
mode<partial_construction_tallinn>` to incrementally validate new operations while
maintaining the aforementioned invariants.

During validation, operations are never actually applied, as it is unnecessary
for asserting their :ref:`validity<operation_validity_tallinn>`.

Merging Mempools
----------------

The protocol-side mempool data structure is designed to allow the merging of two
mempools. This is useful in practice when injecting an external mempool into the
current one. The design also considers the future possibility of parallelizing
operations validation and then merging mempools in a divide-and-conquer style,
an approach that could be implemented later to accelerate validation.

Conflict
--------

Of course, two mempools containing conflicting operations cannot be merged while
maintaining the desired invariants. Similarly, if an injected operation
conflicts with existing ones, the properties would not hold. To solve
conflicts, the structure requires a ``conflict_handler``, a function called upon
encountering conflicts to determine which operation remains and which is
discarded.
