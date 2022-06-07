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

- Its state is given by the :ref:`context<Context>`, the internal
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
   paragraph below.
   
However, the *concrete* API exported from the Tezos economic protocol
does not implement this business logic *monolithically*, as described
above, but it rather presents a more fine-grained API. The rationale
is to provide specialized variations of the core *validation* and
*application* functionality, dubbed :ref:`Validation
modes<validation_modes_alpha>`. These modes enable the protocol to
distinguish the validation of operations "in the mempool", performed
by the :doc:`prevalidator<../shell/prevalidation>`, from the
validation of the operations included in newly received blocks,
performed by the :ref:`block validator<block_validator>`, in order to
localize validation rules as needed. The resulting concrete API is
specified by the :package-api:`Protocol
<tezos-protocol-environment/Tezos_protocol_environment/V8/module-type-T/Updater/module-type-PROTOCOL/index.html>`
module in the :doc:`protocol
environment<../shell/protocol_environment>` ``V8``, and it is
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

.. _full_application_alpha:

Full Application
~~~~~~~~~~~~~~~~

.. _full_construction_alpha:

Full Construction
~~~~~~~~~~~~~~~~~

.. _partial_construction_alpha:

Partial Construction
~~~~~~~~~~~~~~~~~~~~

.. _partial_application_alpha:

Partial Application
~~~~~~~~~~~~~~~~~~~

.. _block_validation_overview_alpha:

Block Validation
================

.. _operation_validity_alpha:

Operation Validation and Application
====================================

.. _manager_operations_validity_alpha:

Validity of Manager Operations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. _manager_operations_application_alpha:

Application of Manager Operations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
