Transaction Rollups
=====================

High-frequency transactions are hard to achieve on a blockchain that
is decentralized and open. For this reason, many blockchains offer the
possibility to define "layer-2" solutions that relax some
constraints in terms of consensus to increase transaction
throughput. The **layer-1** (the main blockchain) acts as a gatekeeper
for several **layer-2** (secondary blockchains), and provides economic
incentives to prevent attacks.

Introduction to Optimistic Rollups
----------------------------------

**Optimistic rollups** are a popular layer-2 solution, *e.g.*, on the
Ethereum blockchain (Boba, Arbitrum, Optimism, etc.). When it comes to
an optimistic rollup, the layer-2 operates using a logic similar to
the layer-1, but it is updated off-chain, potentially much faster, and its changes are regularly committed to layer-1.

The layer-1 implements a decentralized ledger (called the **layer-1
context** thereafter) that participants of the network can update
thanks to authenticated layer-1 operations. These operations are
grouped together in **layer-1 blocks** (hence the name “blockchain”).

Similarly, the layer-2 implements a ledger (called the **layer-2
context**), that participants can update by sending **messages**
stored in the layer-1 context in an **inbox**, with a precise
semantics for the interpretation of messages on top
of a layer-2 context, resulting in the production of a new layer-2
context.

More precisely, an optimistic rollup works as follows:

#. Certain layer-1 operations will append messages to the
   inbox. The inbox is analogous to the layer-1 blocks. As such, the
   consensus of the layer-1 decides which messages the layer-2 has to
   consume, and in which order.
#. The layer-2 context is updated off-chain by a **rollup node**, using the semantics of
   the messages.
#. A layer-1 operation allows the rollup node to include the hash of the layer-2
   context after the execution of the layer-2 operations in the
   layer-1 context.
#. The layer-1 implements a procedure to reject erroneous hashes of
   the layer-2 context (*e.g.*, submitted by an attacker).
#. After a period of time specific to each rollup implementation, and
   in the absence of a dispute, the hashes of the layer-2 context
   become **final**, that is, they cannot be rejected. We call
   **layer-2 finality period** the time necessary for the hash of a
   layer-2 context to become final.
   In the meantime, new layer-1 operations may have filled the inbox with new messages that call for pursuing the same workflow.

The layer-2 context is encoded in a Merkle tree, an ubiquitous data
structure in the blockchain universe with many interesting
properties. One of these properties is significant in the context of
optimistic rollups: it is possible to prove the presence of a value in
the tree without having to share the whole tree, by means of Merkle
proofs. This property ensures that the procedure to reject a hash does
not require to compute the whole layer-2 context.

The rollup node is a daemon responsible for interpreting the
messages (as stored in the inbox) onto the layer-2 context, and for
posting the resulting hashes in the layer-1. In “optimistic rollup”,
the word “optimistic” refers to the assumption that at least one
honest transaction rollup node will always be active to reject
erroneous hash.
The presence of a single honest node is sufficient to guarantee the correct application of the layer-2 operations in the rollup.
In its absence, nothing prevents a rogue node to post a
maliciously tampered layer-2 context.

Introduction to Transaction Rollups
-----------------------------------

**Transaction Rollups** are an implementation of optimistic
rollups in Tezos, characterized by the following principles:

#. The semantics of the messages consists of the transfer of assets represented as Michelson tickets.
#. The procedure to reject erroneous hashes allows for a finality
   period of 40,000 blocks.
#. They are implemented as part of the economic protocol of Tezos
   directly, not as smart contracts.

The latter design choice, made possible by the amendment feature of
Tezos, allows for a specialized, gas- and storage-efficient
implementation of optimistic rollups.

Note that it is possible to create any number of transaction rollups on
Tezos. They are identified with **transaction rollup addresses**,
assigned by the layer-1 at their respective creation (called
origination in Tezos to mimic the terminology used for smart
contract).  They are prefixed by ``txr1`` when encoded in a base58
alphabet (see also the :ref:`kinds of address prefixes in Tezos <address_prefixes_alpha>`).

Workflow Overview
-----------------

Transaction rollups allow for exchanging financial assets, encoded as
`Michelson tickets
<https://tezos.gitlab.io/michelson-reference/#type-ticket>`_, at a
higher throughput than what is possible on Tezos natively.

Analogous to layer-1 addresses, **layer-2 addresses** identify assets
holders in the layer-2 ledger, meaning layer-2 addresses own and
exchange Michelson tickets.
They are prefixed by ``tz4`` when encoded in a base58 alphabet.

The expected workflow proceeds as follows.

#. Layer-1 smart contracts can **deposit** tickets to a transaction rollup for the benefit of
   a layer-2 address.
#. A layer-2 address is associated with a cryptographic public key,
   and the owner of the companion secret key (called “the owner of the
   layer-2 address” afterwards) can sign transfer orders. These orders
   are for the benefit of either another layer-2 address (meaning the
   transfer order only concerns the layer-2) or a layer-1 address
   (making the transfer order a withdrawal of their asset outside of
   the transaction rollup).

To be interpreted by the transaction rollup, transfer orders have to
be signed by (1) the owner of a layer-2 address, and (2) the owner of
a layer-1 address. This is because they are wrapped in a dedicated
layer-1 operation.

While owners of layer-2 addresses who also own a layer-1 address can
submit their transfer and withdraw orders themselves, the expected
workflow is that they delegate this to a trusted transaction rollup
node, which can batch together several layer-2 operations signed by
several owners of layer-2 addresses and submit only one layer-1
operation.

Implementation Overview
-----------------------

Here we examine the specific implementation of transaction rollups in
Tezos.

Origination
***********

Anyone can originate a transaction rollup on Tezos, as the result of
the layer-1 operation ``Tx_rollup_origination``. In a similar manner
as contracts, transaction rollups are assigned an address, prefixed by
``txr1`` when encoded with a base58 alphabet.

Exchanging Tickets
******************

The main objective of a transaction rollup is to allow Michelson
tickets to be exchanged between layer-2 addresses. Before diving into
more details on how these exchanges happen, it is necessary to discuss
how layer-2 addresses and tickets are identified in the layer-2.

First, a layer-2 address is primarily identified by a Blake2B,
20-bytes long hash of a BLS public key (prefixed by ``tz4`` when
encoded with a base58 encoding). Besides, the layer-2 assigns an
integer to each layer-2 address, which can be used in place of the
hash of the BLS public key. This design choice allows for reducing the
size of the layer-1 operations responsible for appending messages to
the inbox of a transaction rollup, which in turn allows for publishing
more of these layer-1 operations in a layer-1 block. This is an
essential property to give transaction rollup a high throughput.

Secondly, a similar mechanism is implemented for ticket
identifiers. By default, tickets are identified by 32-byte hashes
computed by the economic protocol. However, the layer-2 also assigns
integers to ticket hashes, to save up block space.

Ticket Deposit
^^^^^^^^^^^^^^

Initially, the layer-2 ledger of the newly created transaction rollup
is empty. This ledger needs to be provisioned with tickets, which are
deposited into layer-2 by layer-1 smart contracts. They do so by
emitting layer-1 transactions to the transaction rollup address,
targeting more specifically the ``deposit`` entrypoint, whose argument
is a pair consisting of:

#. a ticket (of any type), and
#. a layer-2 address (of type ``tx_rollup_l2_address`` in Michelson),
   which can either be a natural number or a base58 encoded public key
   hash.

Only smart contracts can send tickets to rollups.

Here is a minimal example of a smart contract depositing ``unit``
tickets to a Transaction Rollup::

    parameter (pair address tx_rollup_l2_address);
    storage (unit);
    code {
           # cast the address to contract type
           CAR;
           UNPAIR;
           CONTRACT %deposit (pair (ticket unit) tx_rollup_l2_address);

           IF_SOME {
                     SWAP;

                     # amount for transferring
                     PUSH mutez 0;
                     SWAP;

                     # create a ticket
                     PUSH nat 10;
                     PUSH unit Unit;
                     TICKET;

                     PAIR ;

                     # deposit
                     TRANSFER_TOKENS;

                     DIP { NIL operation };
                     CONS;

                     DIP { PUSH unit Unit };
                     PAIR;
                   }
                   { FAIL ; }
         }

When its ``default`` entrypoint is called, this smart contract emits
an internal transaction targeting a transaction rollup in order to
deposit 10 ``unit`` tickets for the benefit of a given layer-2
address.

Transfers
^^^^^^^^^

Once a layer-2 address has been provisioned with a ticket, the owner
of this address can transfer it to other layer-2 addresses.

Transfer orders are divided into two parts: a header, which identifies
the emitter, and one or more payloads, which specify as many transfer
orders.

More precisely, the header consists in:

#. The layer-2 account authoring the operation, also called its
   *signer*
#. The counter associated to this layer-2 address.

Counters are an anti-replay measure commonly used in blockchains. For
instance, Tezos uses a similar mechanism. See `the documentation
<https://tezos.gitlab.io/introduction/howtouse.html>`_ for more
information.

Then, the payload allows the signer to transfer the ownership of a
given ticket in a given quantity for the benefit of a given
address. More precisely, the payload consists in

#. A destination address. It can either be a layer-1 address, that is
   a ``tz1``, or a layer-2 address, that is a ``tz4`` or the integer
   associated with this address by the layer-2.
#. A ticket hash identifying the asset to exchange, or the integer
   associated with this ticket hash by the layer-2.
#. The quantity of the ticket being exchanged, encoded as an ``int64``
   value.

The mapping between the layer-2 addresses and their associated
integers is maintained by the transaction rollup node.

The interpretation of a transfer order will fail in the following
cases:

#. If the signer of the operation does not own the required
   quantity of the ticket.
#. If the new balance of the beneficiary of the transfer after the
   application of the operation overflows. The quantity of the ticket
   a layer-2 address owns is encoded using a ``int64`` value. This is
   a known limitation of the transaction rollups, made necessary to
   bound the size of the rejection payload so that it can fit in a
   layer-1 operation.

Transfers can be grouped inside a **transaction**. A transaction is
atomic: if any transfer of the transaction fails, then the whole
transaction fails and leaves the balances of the related addresses
unchanged. This can be useful to implement trades. For instance, two
parties can agree upon exchanging two tickets without having to trust
each other for the emission of the counter-part transfer. For a
transaction to be valid, it needs to be signed by the authors of the
transfers it encompasses.

If a transaction fails (because a transfer within that transaction fails),
the transfers are ignored, but the counters of their
signers are updated nonetheless. This means the transaction will need
to be submitted again, with updated counters, if the error is
involuntary.

Transactions are submitted in **batches** to the layer-1, *via* the
``Tx_rollup_submit_batch`` layer-1 operation. A batch of transactions
contains the following pieces of information:

#. The list of transactions that are batched together.
#. A BLS signature that aggregates together all the signatures
   of all the transactions contained by the batch.

A batch of transactions is invalid if the aggregated BLS signature is incorrect (e.g., if one of the included transactions is invalid). Such an invalid
batch is discarded by the transaction rollup node, and the counters of
the signers are not incremented. This means they can be submitted
again in a batch with a valid signature.

Numbering layer-2 inboxes
*************************

A **rollup level** is analogous to a block of layer-1.  It is
identified by a natural number, starting from zero.  For a given
rollup, a rollup level is assigned for each layer-1 block in which
there is at least one message in that rollup. Each rollup maintains
its own set of levels. So, layer-1 block 24601 might correspond to
rollup level 0 for rollup A, rollup level 3 for rollup B, and no
rollup level at all for rollup C.  We often speak of inboxes and
rollup levels interchangeably, as each rollup level corresponds to one
inbox.

A batch is one sort of **message**.  The other sort is a deposit.
Deposits are created by L1 operations which transfer tickets to the
rollup.

Proofs
******

**Merkle proofs** allow a computation to be proven, and then verified.
The roles in the proof game are:

#. The **prover** performs a computation, producing a Merkle proof.
#. The **verifier** is given the proof and re-runs the computation,
   producing a boolean: either the proof is correct, or the proof
   is incorrect.

The details of how Merkle proofs work are too complicated to explain
in this document.  The important thing to know is that the data
storage of the computation is encoded in a Merkle tree, and that the
proof includes the root node of this tree (in addition to possibly some
other nodes).


Commitments and rejections
**************************

In order to ensure that layer-2 transfers and withdrawals_ are
correctly computed, rollup nodes issue **commitments**. A commitment
describes (using Merkle tree hashes) the result of applying *all* the
messages of an inbox to the layer-2 state. For each message of the
inbox, the commitment includes the hash of the state, and the hash of
any withdrawals_ generated by the message.

In this section, we describe commitments primarily from the
perspective of layer-1.  Rollup nodes are responsible for executing
layer-2 operations and issuing commitments (and finalization,
deletions, and rejections if necessary), but their internal
deliberations are not described in this section. The incentive design
of optimistic rollups ensures that rollup nodes will behave correctly.

The usual lifecycle of a rollup level is: uncommitted, then committed,
then finalized, then deleted.

#. Uncommitted: At the uncommitted stage, there is no commitment. When
   a commitment for an inbox is submitted using a layer-1 operation,
   the level moves to the committed stage.  A commitment for an inbox
   can be issued in any layer-1 block after the block containing that
   inbox.

#. Committed: During this stage, commitments can be rejected_, moving
   the inbox back to the uncommitted stage.  An inbox remains in this
   stage until its commitment has been **finalized** by a finalize
   operation.  Finalize operations are only accepted for commitments
   that have survived for more than 40,000 blocks (the **finality
   period**, defined in ``tx_rollup_finality_period``) without being
   rejected_.  The "finalize" operation removes the inbox from the
   context.

#. Finalized: During this stage, any :ref:`Dispatch tickets<withdrawals>`
   operations from this rollup level can occur.  Commitments can no
   longer be rejected.

#. Deleted: Finally, after the **withdrawal period**
   (``tx_rollup_withdraw_period`` = 40,000 blocks), the commitment can
   be removed from the context by another layer-1 operation.  Dispatch
   tickets operations from this rollup level can no longer occur (since the
   commitment has been removed).

A commitment also includes the predecessor commitment's hash (except
in the case of the first commitment for a rollup) and the rollup level
of the block that it is committing to, as well as the level's inbox
hash (in case of reorganizations). For each message of the inbox, the
commitment has a hash of two hashes: the layer-2 context hash, and the
withdrawal list's hash. This saves operation size by storing only a
single hash, at the cost of more complex rejection and withdrawal
operations. There is exactly one valid commitment possible for a given
block, because the computation of the Merkle proof of the layer-2
operations is deterministic.

At most one commitment is stored per level. If a commitment operation
is attempted for a level that already has a non-rejected commitment,
it will fail. Commitments are stored in a compact fashion: their
message hash lists are themselves Merkelized.

Finalization is implemented as a layer-1 operation. This allows
finalization to be carbonated. It operates on the oldest unfinalized
commitment for a rollup.  The finalization period needs to be long
enough so that an attempt by 33% of bakers to steal from a rollup by
censoring rejections can be noticed, and avoided by forking the
chain.

After finalization, a commitment sticks around for the withdrawal
period, and then can be deleted by a layer-1 **remove commitment**
operation.  For the most recent commitment deleted, the context keeps the
commitment's hash and last message around in case we need to examine
them to reject its successor.

As discussed above, the inbox for a level is deleted during commitment
finalization.  If no commitments are made, it is possible for inboxes
to pile up, which violates our gas assumptions that inboxes are
temporary. To prevent this, if there are more than
``tx_rollup_max_unfinalized_levels`` = 40,100 inbox levels with messages
but without finalized commitments, no further messages are accepted on
the rollup until a commitment is finalized.

In order to issue a commitment, a **bond** is required: Tez tokens are
temporarily frozen, and is subject to slashing in the event of a
rejection. The bond is treated just like frozen balances for the
purpose of delegation. The bond is expensive enough
(``tx_rollup_commitment_bond`` = 10,000 Tez) to discourage bad
commitments. One bond can support any number of commitments from the
same source on a single rollup. The bond is collected at the time that
a given contract creates its first commitment on a rollup. It may be
refunded by another layer-1 operation, once the last commitment on the
rollup from its creator has been removed from the context (that is,
after the finality and withdrawal period).

.. _rejected:

If a commitment is wrong (that is, its Merkle proof does not
correspond to the correct execution trace of the layer-2 apply
function), it may be **rejected**. A rejection operation for a
commitment names one of the messages of the commitment, and includes a
Merkle proof of the computation.  It also includes the disputed
commitment message, and the Merkle proof that the commitment exists in
the compact commitment's Merkle tree. A layer-1 node can then replay
just the transfers of a single message to determine whether the
rejection is valid. Because of the compact structure of commitments, a
rejection also must include the predecessor message's layer-2 context
hash, as a staring point to verify the proof. And the withdrawal list
must be included, so that the layer-2 context hash can be verified
against the disputed message's predecessor.  A rejection must be
included in a block within the finality period of the commitment.

It might be possible to create a message whose proof is too long to
fit into a rejection operation. The limits we have imposed are
intended to be long enough to avoid this, but it is possible that our
limits are wrong. To handle this possibility, we impose a hard limit
on proof size, ``tx_rollup_rejection_max_proof_size``, which is less
than the operation size limit. If a proof turns out to be greater than
``tx_rollup_rejection_max_proof_size``, the entire message is treated
as a no-op.  To reject such a commitment, the rejection operation will
contain a truncated proof. If the commitment committed to anything
other than the state prior to applying the message, the rejection
succeeds. So such messages are always treated as no-ops.

In the case of a valid rejection, half of the commitment bond goes to
the rejector; the rest is burned.

.. _withdrawals:

Ticket withdrawals
******************

Withdrawing a ticket from a rollup back to layer-1 is a three-phase
operation.

First, a layer-2 operation is submitted requesting the withdrawal.
This is actually implemented as a transfer with a destination that is
a layer-1 address.  As with any other transfer, it is included in a
batch, which is included in an inbox, which gets a commitment.  After
that commitment is finalized, the next phase can begin.

Next, because commitments are stored Merkelized, a **dispatch
tickets** layer-1 operation must be sent.  This includes:

#. The rollup and level that the withdrawals are in

#. The message index in the inbox that contains the withdrawals.

#. A Merkle proof that the hash of these withdrawals is included in
   the specified Merkelized commitment.

#. The ticket contents and amounts for every ticket withdrawn in that
   message.

Because all of the ticket contents for every withdrawal in a message
must fit into a single layer-1 operation, there are hard limits on the
size and count of tickets.  The limit on ticket size,
``tx_rollup_max_ticket_payload_size``, is enforced at deposit time.
The limit on withdraw count is ``tx_rollup_max_withdrawals_per_batch``,
and it is enforced by the layer-2 apply function.  Withdrawals
beyond this limit are treated as no-ops.

The "dispatch tickets" operation updates the table of tickets to
transfer the tickets from the rollup to a layer-1 implicit account.
It also stores a record in the layer-1 context indicating that the
tickets for a message have been dispatched, so that the same dispatch
cannot happen again.  If a dispatch tickets operation is not sent
before the ``tx_rollup_withdraw_period``, the tickets withdrawn in the
corresponding inbox are destroyed irrecoverably.

After the dispatch tickets operation, a **transfer ticket** layer-1
operation can be sent, which will transfer the tickets to their final
smart contract.  There is no deadline for this.

Getting Started
---------------

Originating a Transaction Rollup
********************************

The ``tezos-client`` has a dedicated command that any implicit account holder
can use to originate a transaction rollup.

.. code:: sh

    tezos-client originate tx rollup from <implicit account address>

where ``tx`` is an abbreviation for transaction.

.. TODO: https://gitlab.com/tezos/tezos/-/issues/2152

The origination of a transaction rollup burns ꜩ15.

A transaction rollup address is attributed to the new transaction
rollup. This address is derived from the hash of the Tezos operation with the
origination operation similarly to the smart contract origination. It is always
prefixed by ``txr1``. For instance,::

   txr1YNMEtkj5Vkqsbdmt7xaxBTMRZjzS96UAi

is a valid transaction rollup address.

When using the ``tezos-client`` to originate a transaction rollup, it outputs
the newly created address.

Interacting with a Transaction Rollup using ``tezos-client``
************************************************************

The ``tezos-client`` provides dedicated commands to interact with a
transaction rollup. These commands are not intended to be used in a
daily workflow, but rather for testing and development purposes.

It is possible to use the ``tezos-client`` to submit a batch of
layer-2 operations.

.. code:: sh

    tezos-client submit tx rollup batch <batch content in hexadecimal notation> to <transaction rollup address> from <implicit account address>

It is also possible to retrieve the content of an inbox thanks
to a dedicated RPC of the ``tezos-node``.

.. code:: sh

    tezos-client rpc get /chains/main/blocks/<block>/context/tx_rollup/<transaction rollup address>/inbox/<offset>
