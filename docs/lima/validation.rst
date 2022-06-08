==========================
Validation and Application
==========================

.. FIXME tezos/tezos#3921:

   Adapt to pipelined validation up to Lima and v7 environment

The :doc:`economic protocol<protocol>` is responsible for
providing the rules that govern the Tezos network, and for enforcing
that this rules implement a correct blockchain protocol. However, it
does so :ref:`in coordination with a Tezos shell<the_big_picture>`,
who ultimately implements these rules on its behalf. To this end, a
Tezos economic protocol must provide the shell with an interface
enabling roughly the following functionalities:

 - deciding whether an operations is *valid*, and thus can be safely
   included in a new block in the blockchain;

 - deciding whether a new block is *valid*, and thus it can be safely
   appended to the blockchain;

 - including an operation in a block, and executing it on the
   blockchain state, effectively modifying the ledger state; and,

 - appending a new block to a Tezos blockchain, and computing the
   updated ledger state.

From a higher-level, *abstract* perspective, the validation system in
the Tezos protocol implements this business logic in a functional,
state-passing machine where:

- Its state is given by the :ref:`context<Context>`, the internal
  representation of the state of Tezos ledger at a given blockchain
  level. For instance, the context contains the information of all
  activated accounts and contracts, and their balances. More
  generally, the context must provide enough information to determine
  the validity of operations, and blocks.

- The *apply_operation* method determines whether an operation is safe
  to be executed in a given context. If so, it proceeds to execute it
  and outputs the context resulting from the applied operation's
  effects.

- The *apply* method takes as input a block and a context, determines
  whether the block can be safely and correctly appended to the
  current head of the chain. If so, it appends the block to the chain
  and outputs a the resulting context, reflecting the state of the
  ledger after the block has successfully been applied. *apply* relies
  on (i.e., it should call) *apply_operation* to validate and apply
  each operation in the block, and compute intermediate states.

However, the *concrete* API exported from the Tezos economic protocol
does not implement this business logic *monolithically*, as described
above, but it rather presents a more fine-grained API. The rationale
is to provide specialized variations of the core *validation* and
*application* functionality, dubbed :ref:`Validation
modes<validation_modes_lima>`. For example, these modes enable the
protocol to distinguish operations "in the mempool", whose validation
is triggered by the :doc:`prevalidator<../shell/prevalidation>`, from
operations included in newly received blocks, whose validation is
triggered by the :ref:`block validator<block_validator>`, in order to
localize validation rules as needed. The resulting concrete API is
specified by the :package-api:`Protocol
<tezos-protocol-environment/Tezos_protocol_environment/V7/module-type-T/Updater/module-type-PROTOCOL/index.html>`
module in the :doc:`protocol
environment<../shell/protocol_environment>` ``V7``, and it is
implemented by this protocol in the
:package-api:`Main<tezos-protocol-015-PtLimaPt/Tezos_raw_protocol_015_PtLimaPt/Main/index.html>`
module.

The rest of this document is organized as follows: we first describe
the different validation modes implemented by this Tezos economic
protocol, and then we delve deeper into the particulars of the validation and application of blocks and the operations supported.

.. _validation_modes_lima:

Validation modes
================

The Tezos protocol provides different validation modes, intended to be
used by Tezos *shell* and *baker* software implementations when
needing to apply (or to assert the validity) of blocks and operations
under different, or specialized, circumstances -- for example, in
order to *bake* a block. For each of these validation modes, the API
specified by the protocol environment offers an entry-point so that
protocol agnostic components, the Octez shell for instance, are able
to use these different modes.

.. _full_application_lima:

Full Application
~~~~~~~~~~~~~~~~

The ``Full application`` mode is intended to be used to *fully*
validate and apply blocks. In particular, this mode is used to
validate and apply a **known** block, with a known operation trace. A
Tezos shell implementation should use the full application mode to
decide whether an incoming block can be safely included in the
blockchain. Thus, all validity checks are enabled: the block's
signature is correct, and **all** operations included in the block are
valid; the correct amount of consensus operations have been included
in order to satisfy the consensus' threshold, etc.

.. _full_construction_lima:

Full Construction
~~~~~~~~~~~~~~~~~

The ``Full construction`` mode is intended to be used when a
*delegate* is trying to bake a block and therefore needs to filter the
validity of the desired operation trace, to include only valid
operations. This mode is mostly similar to the ``Full application``
mode except that *some* global block validity checks are disabled, and
consensus operations are validated with slightly different
preconditions. For instance, since a delegate cannot sign a block
while it is being built, the signature check is disabled, and it will
be left to the baker to correctly sign the resulting block after its
construction is finalized.

In Octez, this mode is mainly used by the baker daemon.

.. _partial_construction_lima:

Partial Construction
~~~~~~~~~~~~~~~~~~~~

The ``Partial construction`` mode, also known as ``Mempool mode`` is
used by the :doc:`prevalidator component<../shell/prevalidation>` of
an Octez node to validate incoming operations -- that is, those
not-yet included into blocks. This mode's business-logic is very close
to the ``Full construction`` mode, and the differences boil down to
the intended usage. The partial construction mode does not try to
fully bake a block, but rather to inform the Octez prevalidator on the
potential validity of operations (and whether they can safely included
into a block), so that the later can **classify** incoming operations,
and further decide how to process them accordingly.

.. _protocol_classification_lima:

The protocol provides the shell with the following classification of
an operation, consisting of one valid kind -- ``Applied`` --, and
:ref:`four error category kinds <error_monad_within_protocol>` defined
by the protocol environment:

- ``Applied``: the operation is valid and can be included in a
  potential block in the current context.

- ``Temporary``: the operation is invalid in the current context, but
  it could *later* become valid -- in the context associated to a
  successor of the current head. For instance, a manager operation
  whose counter value is greater than the one expected (a
  *"counter-in-the-future"* error), or the manager's balance is
  insufficient to pay the operation's fees, etc.

- ``Branch``: the operation is invalid in the current context and in
  all possible context from its future successors, but it might still
  be valid in an alternative branch. For example: a manager operation
  with a smaller counter than the one expected (a
  *"counter-in-the-past"* error), an unexpected endorsement for the
  current level, etc.

- ``Permanent``: the operation is invalid in the current context,
  and there isn't any plausible context where it might become
  valid. For example, an operation carrying an invalid signature.

- ``Outdated``: the operation is *too old* to be included in a
  block. Furthermore, there might be still some value in the
  information provided by an ``Outdated`` operation. An example is the
  case of an endorsement which was received *too late*, but that could
  still be used to form a consensus quorum.

.. _partial_application_lima:

Partial Application
~~~~~~~~~~~~~~~~~~~

The ``Partial application`` mode is used for :ref:`multi-pass
validation<multi_pass_validation>`. Its aim is to provide Tezos shell
implementations with a light-weight (read, fast) block application
mechanism, which can determine whether a block has a *chance* of being
valid or not, in a situation when the provided context is *not a
recent one*. That is, when the block candidate succeeds neither the
head of the chain, nor a close ancestor.

This is validation mode is typically used when the node receives a
significantly large branch -- for instance, while bootstrapping. To
check whether this branch is plausibly valid or potentially malicious
spam, the shell retrieves the context from the most recent common
ancestor between its current head and the announced branch, and
proceeds to "partially apply" each block of this branch using the
common ancestor's context.

Indeed, by relying on the ancestor context, this mode can *only*
assert the validity of consensus-related preconditions (endorsing
power, block fitness, etc.), as future consensus slots are known in
advance -- how much in advance being specified by
the ``<PRESERVED_CYCLES>`` protocol constant. Thus, the `Partial
application` modes provides an over-approximation of the branch's
validity, and as a result intermediate results are not committed on
disk in order to prevent potential attacks.

.. _block_validation_overview_lima:

Block Validation
================

.. FIXME tezos/tezos#3921:

   Adapt to pipelined block validation up to Lima and v7 environment.

The validity of a blocks depends on a set of precondition checks
implemented in different steps, which happen at different stages of
the application (and the construction) of a block.

The first step in the process is to decide whether a candidate block
is *well-formed*, that is, that it has the expected "shape" of a valid
block under the current Tezos economic protocol. Given a block
candidate, the block validation process will then verify that the
candidate block declares consistent :ref:`level<Level>`,
:ref:`round<Round>`, and timestamp values; that it carries a valid
signature, etc. At this step, the block validation process will also
initialize the data-structures required for subsequent steps.

The second step iterates over the block's operations and proceeds to
apply them sequentially. When at least one operation is found to be
invalid, under the conditions described in
:ref:`operation_validity_lima` further below, the whole block is
considered as invalid.

The last step in the block validation process, known as "block
finalization", aims to verify that the collected consensus operations
constitute a sufficiently large :ref:`quorum<quorum_lima>`. That is,
it will verify that the total endorsing power present in the block is
greater than the ``CONSENSUS_THRESHOLD`` constant.

This last step also yields a new context -- the resulting state of the
Tezos ledger after the application of the candidate block. The shell
may decide to commit this context to disk.

The Tezos economic protocol also offers a cheap (read "faster")
alternative to determine an over-approximation of the validity of a
block (see :ref:`partial_application_lima` above). This feature
allows the shell to propagate blocks faster without needing to fully
validate them, speeding-up block propagation over the network. As this
is an over-approximation, this feature cannot be considered to provide
a safe guarantee that a block will be valid: in particular, it does
not validate all kinds of operations.

.. _operation_validity_lima:

Operation Validation and Application
====================================

.. _manager_operations_validity_lima:

Validity of Manager Operations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. _manager_operations_application_lima:

Application of Manager Operations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
