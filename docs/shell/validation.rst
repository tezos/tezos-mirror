The validation subsystem
========================

This document explains the inner workings of the validation subsystem
of the Tezos shell, that sits between the peer-to-peer layer and the
economic protocol. This part is in charge of validating chains, blocks
and operations that come from the network, and deciding whether they
are worthy to propagate. It is composed of three main parts: the
:ref:`validator<validator_component>`, the
:ref:`prevalidator<prevalidator_component>`, and
the :ref:`distributed DB<DDB_component>`.

|Tezos validation diagram|

Concepts
--------

Before presenting these three components, we define some basic concepts.

Block
~~~~~

The Tezos blockchain is a linked list of blocks (or actually, a tree when several competing branches exist).
Blocks conceptually contain a header and a list of operations.
In the implementation, the list of operations in the block is represented as the hash of the Merkle tree containing them.

The header itself decomposes into a shell header (common to all protocols) and a protocol-specific header.

Fitness
~~~~~~~

To each block, we associate a measure of fitness which determines the quality of the chain leading to that block.
This measure is computed by the consensus protocol.
The shell changes the head of the chain to the valid block that has the highest fitness.

The fitness belongs to the shell part of the block header.
The shell does not know the exact representation of the fitness, except that it has a total order on it.


.. _shell_header:

Shell header
~~~~~~~~~~~~

The shell header contains:

-  ``level``: the height of the block, from the genesis block.
-  ``proto``: number of protocol changes since genesis (modulo 256).
-  ``predecessor``: the hash of the preceding block.
-  ``timestamp``: the timestamp at which the block is claimed to have
   been created.
-  ``validation_pass``: number of validation passes. Indeed, operations
   included in a block may be validated in several passes. This enables some
   kind of operations (e.g., consensus operations) to be validated in priority.
-  ``operations_hash``: Hash of the list of lists (actually root hashes of
   Merkle trees) of operations included in the block. There is one list of
   operations per validation pass.
-  ``fitness``: a sequence of sequences of unsigned bytes, shortlex-ordered (by
   length and then lexicographically). It represents the claimed fitness
   of the chain ending in this block.
-  ``context``: the hash of the state of the context after application of
   this block.

The rest of this page presents the three components of the validation subsystem.

.. _validator_component:

Validator
---------

The validator is the component responsible for checking that blocks
coming from the network or a baker are valid, w.r.t. the rules defined
by the economic protocol, and for selecting the block that it
considers to be the current head of the blockchain, based on its fitness.

The validator is written as a collection of workers: local event loops
communicating with each other via message passing. Workers are spawned
and killed dynamically, according to connected peers, incoming blocks
to validate, and active (test)chains.

.. _chain_validator:

A *chain validator* worker is launched by the validator for each
*chain* that it considers alive. Each chain validator is responsible for
handling blocks that belong to this chain, and select the best head for
this chain. A chain validator is spawned for the main chain that
starts at the genesis, a second one when there is an active test
chain. Forking a chain is decided from within the economic protocol.  In
protocol Alpha, this is only used to try new protocols before self
amending the main chain.

.. _peer_validator:

The chain validator spawns one *peer validator* worker per connected
peer. The set of peer validators is updated, grown, or shrunk on the fly, according to the
connections and disconnections signals from the peer-to-peer component.
Each peer validator will treat new head proposals from the associated
peer, one at a time, in a loop. In the simple case, when a peer
receives a new head proposal that is a direct successor of the current
local head, it launches a simple *head increment* task: it retrieves
all the operations and triggers a validation of the block. When the
difference between the current head and the examined proposal is
more than one block, mostly during the initial bootstrap phase, the
peer worker launches a *bootstrap pipeline* task.

Peer validators interact with the *distributed database* to retrieve block
headers and operations. The database functions as a cache, returning data
instantly if available or querying linked peers when it's missing. Once a block
is complete, peer validators submit it to the *block validator*.

.. _block_validator:

The block validator processes blocks sequentially, assuming all necessary data
has already been retrieved from the peer-to-peer network. Blocks are submitted
through a two-step process:

- :package-api:`validation<octez-shell-libs/Tezos_validation/Block_validation/index.html#val-validate>` performs a light check doing the minimal computation required to assess with certitude whether a block is well-formed and can be applied in the current context.

- :package-api:`application<octez-shell-libs/Tezos_validation/Block_validation/index.html#val-apply>` computes the resulting context by executing manager operations and, particularly, running smart contracts, which can be computationally expensive.

The block validator advertises a block to peers immediately after validation (if
successfull). This optimization ensures that blocks are broadcast as quickly as
possible while still guaranteeing that only valid blocks are shared across the
network.

Once the block is successfully applied, the chain validator is notified and may
update its head. The chain validator will then propagate this information to its
associated prevalidator.

The validator :ref:`relies on the protocol <shell_proto_interact>` for both
:package-api:`validation<tezos-protocol-alpha/Tezos_raw_protocol_alpha/Validate/index.html#block-validation>`
and
:package-api:`application<tezos-protocol-alpha/Tezos_raw_protocol_alpha/Apply/index.html>` functions.

.. _prevalidator_component:

Prevalidator
------------

Each chain validator is associated to a *prevalidator* that is
responsible for determining which operations to propagate for this chain over the
peer-to-peer network. The page :doc:`./prevalidation` gives a detailed
description of the prevalidator component.

The prevalidator also :ref:`interacts with the protocol <shell_proto_interact>` in order to determine valid operations in the mempool to propagate in the gossip network.

Distributed DB
--------------
.. _DDB_component:

The gathering of resources needed for validation is centralized in the
*distributed db*. This component allocates a slot per requested
resource, whose priority depends on the number of peer validators
requesting it.

.. |Tezos validation diagram| image:: validation.svg
