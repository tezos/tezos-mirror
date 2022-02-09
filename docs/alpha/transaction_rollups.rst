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
#. The procedure to reject erroneous hashes allows for a short
   finality period of 30 blocks.
#. They are implemented as part of the economic protocol of Tezos
   directly, not as smart contracts.

The latter design choice, made possible by the amendment feature of
Tezos, allows for a specialized, gas- and storage-efficient
implementation of optimistic rollups.

Note that it is possible to create any number of transaction rollups on
Tezos. They are identified with **transaction rollup addresses**,
assigned by the layer-1 at their respective creation (called
origination in Tezos to mimic the terminology used for smart
contract).  They are prefixed by ``tru1`` when encoded in a base58
alphabet (see also the :ref:`kinds of address prefixes in Tezos <address_prefixes>`).

Workflow Overview
-----------------

Transaction rollups allow for exchanging financial assets, encoded as
`Michelson tickets
<https://tezos.gitlab.io/michelson-reference/#type-ticket>`_, at a
higher throughput than what is possible on Tezos natively.

Analogous to layer-1 addresses, **layer-2 addresses** identify assets
holders in the layer-2 ledger, meaning layer-2 addresses own, and
exchange Michelson tickets.
They are prefixed by ``tru2`` when encoded in a base58 alphabet.

The expected workflow proceeds as follows.

#. Layer-1 smart contracts can **deposit** tickets for the benefit of
   a **layer-2 address** to a transaction rollup.
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
``tru1`` when encoded with a base58 alphabet.

Exchanging Tickets
******************

The main objective of a transaction rollup is to allow Michelson
tickets to be exchanged between layer-2 addresses. Before diving into
more details on how these exchanges happen, it is necessary to discuss
how layer-2 addresses and tickets are identified in the layer-2.

First, a layer-2 address is primarily identified by a Blake2B,
20-bytes long hash of a BLS public key (prefixed by ``tru2`` when
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
is a pair of

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
   a ``tz1``, or a layer-2 address, that is a ``tru2`` or the integer
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
prefixed by ``tru1``. For instance,::

   tru1HdK6HiR31Xo1bSAr4mwwCek8ExgwuUeHm

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
