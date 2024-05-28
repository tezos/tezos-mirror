==========================
Validation and Application
==========================

.. FIXME tezos/tezos#3921:

   Adapt to pipelined validation up to Lima and v7 environment

The :doc:`economic protocol<protocol>` is responsible for providing
the rules that govern the Tezos network, and for enforcing that these
rules implement a correct blockchain protocol. However, it does so
:ref:`in coordination with a Tezos shell<the_big_picture>`, who
ultimately implements these rules on its behalf. To this end, a Tezos
economic protocol must provide the shell with an interface enabling
roughly the following functionalities:

- deciding whether an operation is *valid*, and thus can be safely
  included into a new block in the blockchain;

- deciding whether a new block is *valid*, and thus can be safely
  appended to the blockchain;

- including an operation in a block, and executing it on the
  blockchain state, effectively modifying the ledger state; and,

- appending a new block to a Tezos blockchain, and computing the
  updated ledger state.

From a higher-level, *abstract* perspective, the validation system in
the Tezos protocol implements this business logic in a functional,
state-passing machine where:

- Its state is given by the :ref:`context<def_context_alpha>`, the internal
  representation of the state of the Tezos ledger at a given blockchain
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
  and outputs the resulting context, reflecting the state of the
  ledger after the block has successfully been applied. *apply* relies
  on (i.e., it should call) *apply_operation* to validate and apply
  each operation in the block, and compute intermediate states.

.. TODO #4155:

   When creating a new environment, update references to V<N> in the
   paragraph below (only in the doc for Alpha!).

However, the *concrete* API exported from the Tezos economic protocol
does not implement this business logic *monolithically*, as described
above, but it rather presents a more fine-grained API. The rationale
is to provide specialized variations of the core *validation* and
*application* functionality, dubbed :ref:`Validation
modes<validation_modes_alpha>`. For example, these modes enable the
protocol to distinguish operations "in the mempool", whose validation
is triggered by the :doc:`prevalidator<../shell/prevalidation>`, from
operations included in newly received blocks, whose validation is
triggered by the :ref:`block validator<block_validator>`, in order to
localize validation rules as needed. The resulting concrete API is
specified by the :package-api:`Protocol
<octez-proto-libs/Tezos_protocol_environment/V12/module-type-T/Updater/module-type-PROTOCOL/index.html>`
module in the :doc:`protocol
environment<../shell/protocol_environment>` ``V12``, and it is
implemented by this protocol in the
:package-api:`Main<tezos-protocol-alpha/Tezos_raw_protocol_alpha/Main/index.html>`
module.

The rest of this document is organized as follows: we first describe
the different validation modes implemented by this Tezos economic
protocol, and then we delve deeper into the particulars of validation
and application for blocks and the operations supported.

.. _validation_modes_alpha:

Validation modes
================

The Tezos protocol provides different validation modes, intended to be
used by the Tezos *shell* and *baker* software implementations when
needing to apply (or to assert the validity) of blocks and operations
under different, or specialized, circumstances -- for example, in
order to *bake* a block. For each of these validation modes, the API
specified by the protocol environment offers an entry point so that
protocol-agnostic components, the Tezos shell for instance, are able
to use these different modes.

.. _full_application_alpha:

Full Application
~~~~~~~~~~~~~~~~

The ``Full application`` mode is intended to be used to *fully*
validate and apply blocks. In particular, this mode is used to
validate and apply a **known** block, with a known operation trace. A
Tezos shell implementation should use the full application mode to
decide whether an incoming block can be safely included in the
blockchain. That is, all validity checks are enabled: the block's
signature is correct, and **all** operations included in the block are
valid; the correct amount of consensus operations have been included
in order to satisfy the consensus' threshold, etc.

.. _full_construction_alpha:

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

.. _partial_construction_alpha:

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
into a block), so that the latter can **classify** incoming
operations, and further decide how to process them accordingly.

.. _protocol_classification_alpha:

The protocol provides the shell with the following classification of
an operation, consisting of one valid kind -- ``Applied`` --, and
:ref:`four error kinds <error_monad_within_protocol>` defined by the
protocol environment:

- ``Applied``: the operation is valid and can be included in a
  potential block in the current context.

- ``Temporary``: the operation is invalid in the current context, but
  it could *later* become valid -- in the context associated to a
  successor block of the current head. For instance, a manager
  operation whose counter value is greater than the one expected (a
  *"counter-in-the-future"* error), or the manager's balance is
  insufficient to pay the operation's fees, etc.

- ``Branch``: the operation is invalid in the current context and in
  any possible context from its future successors, but it might still
  be valid in an alternative branch. For example: a manager operation
  with a smaller counter than the one expected (a
  *"counter-in-the-past"* error), an unexpected attestation for the
  current level, etc.

- ``Permanent``: the operation is invalid in the current context, and
  there isn't any plausible context where it might be or become
  valid. For example, an operation carrying an invalid signature.

- ``Outdated``: the operation is *too old* to be included in a
  block. Furthermore, there might be still some value in the
  information provided by an ``Outdated`` operation. An example is the
  case of an attestation which was received *too late*, but that could
  still be used to form a consensus quorum.

.. _partial_application_alpha:

Partial Application
~~~~~~~~~~~~~~~~~~~

The ``Partial application`` mode is used for :ref:`multi-pass
validation<multi_pass_validation>`. Its aim is to provide Tezos shell
implementations with a light-weight (read "fast") block application
mechanism, which can determine whether a block has a *chance* of being
valid or not, in a situation when the provided context is *not a
recent one*. That is, when the block candidate succeeds neither the
head of the chain, nor a close ancestor.

This validation mode is typically used when the node receives a
significantly large branch -- for instance, while bootstrapping. To
check whether this branch is plausibly valid or potentially malicious
spam, the shell retrieves the context from the most recent common
ancestor between its current head and the announced branch, and
proceeds to "partially apply" each block of this branch using the
common ancestor's context.

Indeed, by relying on the ancestor context, this mode can *only*
assert the validity of consensus-related preconditions (attesting
power, block fitness, etc.), as future consensus slots are known in
advance -- how much in advance being specified by the
``<CONSENSUS_RIGHTS_DELAY>`` protocol constant. Thus, the ``Partial
application`` mode provides an over-approximation of the branch's
validity, and as a result intermediate results are not committed on
disk in order to prevent potential attacks.

.. _block_validation_overview_alpha:

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
candidate block declares consistent :ref:`level<def_level_alpha>`,
:ref:`round<def_round_alpha>`, and timestamp values; that it carries a valid
signature, etc. At this step, the block validation process will also
initialize the data-structures required for subsequent steps.

The second step iterates over the block's operations and proceeds to
apply them sequentially. When at least one operation is found to be
invalid, under the conditions described in
:ref:`operation_validity_alpha` further below, the whole block is
considered as invalid.

The last step in the block validation process, known as "block
finalization", aims to verify that the collected consensus operations
constitute a sufficiently large :ref:`quorum<quorum_alpha>`. That is,
it will verify that the total attesting power present in the block is
greater than the ``CONSENSUS_THRESHOLD`` constant.

This sequence of three steps also yields a new context -- the
resulting state of the Tezos ledger after the application of the
candidate block. The shell may decide to commit this context to disk.

The Tezos economic protocol also offers a cheap (read "faster")
alternative to determine an over-approximation of the validity of a
block (see :ref:`partial_application_alpha` above). This feature
allows the shell to propagate blocks faster without needing to fully
validate them, speeding-up block propagation over the network. Of
course, as this is an over-approximation, this feature cannot be
considered to provide a safe guarantee that a block will be valid: in
particular, it does not validate all kinds of operations.

.. _operation_validity_alpha:

Operation Validation and Application
====================================

In the Tezos economic protocol, we dissociate the notion of *validity*
from the notion of *applicability* for operations. A valid operation
is an operation that can be included safely in a block without
affecting the block's validity. Applying an operation, on the other
hand, actually performs the operation's side-effects which can be:
registering a new delegate, executing a smart contract, voting for a
new protocol amendment proposal, etc.

Note that an operation may fail during the application phase, even
though it has been checked as valid. For example, a smart contract
call that exceeds its gas limit can be included in a block even if an
error is raised at run-time. The application (that is, the operation's
side-effects) will not take effect, but fees will nonetheless be taken
from the account submitting the smart contract call.

In the sequel we refine the validity conditions and describe the
application process for each of the different validation passes.

.. FIXME tezos/tezos#3921:

   Expand validity and application for other validation classes.

.. _manager_operations_validity_alpha:

Validity of Manager Operations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this sub-section, we explain the conditions for manager operations
(and batches of managers operations) to be considered valid and hence
suitable for inclusion in a block.

Validity of Individual Manager Operations
.........................................

:ref:`Manager operation<manager_operations_alpha>` are a class of
operations, issued by a single *manager* account which signs the
operation and pays their fees. The different manager operation kinds
share several common fields:

- ``source``: the public key's hash of the *source* account of the
  manager operation -- that is, the *manager*.
- ``fee``: the amount of tez paid to the baker which decides to
  include this operation;
- ``counter``: the manager account's counter, incremented each time
  this account executes a manager operation, to prevent
  replay-attacks.
- ``gas_limit``: the maximum amount of gas that this operation may
  consume before failing.
- ``storage_limit``: the maximum amount of storage that this operation
  may require before failing.
- ``operation``: the actual operation(s) -- e.g., transfers,
  smart-contract calls, originations, delegations, etc.
- ``signature``: the manager's signature used to prove its identity.

A manager operation is **valid** if and only if all of the following
conditions hold:

- The operation source's public key has been previously *revealed*,
  unless the operation is itself a ``Reveal`` operation.
- The operation's signature is correct with regard to the manager
  account's public key.
- The provided ``counter`` value is the expected one for the manager.
- Depending on the operation's kind, the ``gas_limit`` is high enough
  to cover the minimal cost of parsing the operation and further
  minimal treatment.
- The manager account is solvent to pay the announced fees.

Validity of Manager Operation Batches
.....................................

A :ref:`batch<manager_operations_batches_alpha>` of manager operations
includes one or more manager operations for sequential and atomic
execution. The atomicity property imposes that the validity of a batch
should entail the validity of each individual operation in the batch,
as defined above. However, it also entails some additional *global*
constraints on manager batches.

For each of the operations present in a batch, the validation process
must check that the individual constraints above are satisfied, *with
the exception of* the signature constraint. Given that the signature
concerns the whole batch, as all operations in the batch are signed by
the same manager, it suffices to verify the signature only once.

The **global batch validity** constraint for this economic protocol is
defined as the conjunction of the following conditions:

- When a ``Reveal`` operation is present, **it must only occur once**,
  and **it must be placed at the head** of the batch -- that is, the
  ``Reveal`` operation must be the first operation in the batch.
- Every operation in the batch should declare the same ``source``.
- Each of the individual operation counters must be incremented
  correctly and sequentially.
- The sum of each individual operation's declared fees must be lower
  than the account's balance. That is, the manager account must be
  solvent to pay the announced fees for all the operations in the
  batch.

.. _manager_operations_application_alpha:

Application of Manager Operations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Once the validity of a manager operation (or, a batch of manager
operations) is established, the protocol proceeds to apply the
operation. This first step in this application phase is to transfer
the operation's fees to the baker that included this operation. Then,
the actual application depends on the operation kind. For instance,
this could be a smart contract execution, enacting a delegation, or
multiple actions executed as a batch. The application of a batch of
manager operations consists of the sequential application of each
operation in the batch, following their inclusion order -- the head of
the batch being the first manager operation being applied.

The application of each individual manager operation may either
succeed -- and therefore be reported as ``Applied`` --, or indeed fail
with an error. In both cases, the fees are taken and the counter for
the operation's manager is incremented.

When a manager operation fails, every side-effect which was previously
performed is backtracked. Moreover, the (rest of the) batch has to be
aborted.  Thus, depending on the position of the manager operation in
a batch, its failure has to be propagated accordingly:

- If there were other successfully applied operations in the batch
  prior to the offending one, the effect of each of them has to be
  reverted, and each of them will be reported as ``Backtracked``.

- If there were other operations pending application after the
  offending one, their application is aborted, and they are reported
  as ``Skipped``.

For example, let's consider a simple batch of three manager operations
``[op1, op2, op3]``, if ``op1`` is successful but ``op2`` fails, the
ticket result for the application of the manager operation batch will
report:

* ``op1`` -- ``Backtracked``, ``op1`` was applied successfully, but
  after ``op2`` failed, the operation was canceled;
* ``op2`` -- ``Failed``, the application of this particular operation failed;
* ``op3`` -- ``Skipped``, this operation was never executed because
  ``op2`` had previously failed.
