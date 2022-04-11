Overview of the economic protocol
=================================

Tezos overview
~~~~~~~~~~~~~~

Tezos is a distributed system in which nodes agree upon a chain of blocks of
operations. Tezos is also an account-based crypto-ledger, where an account is
associated to a public-private key pair, and has a balance, that is, a number of
tokens. Tezos is a :doc:`proof-of-stake<proof_of_stake>` system in which any
account that has a minimal stake amount has the right to produce blocks, in
proportion to their balance.

A Tezos node has mainly three roles: it validates blocks and operations, it
broadcasts them to (and retrieves them from) other nodes, and it maintains a
main chain and its associated state (i.e. the ledger), which includes accounts
and their balances, among other things. Note that, as blocks only specify a
predecessor block, exchanged blocks do not necessarily form a chain, but rather
a tree. Nodes communicate over :doc:`a gossip network<../shell/p2p>`.

A Tezos node acts as a server, which responds to queries and requests from
clients. Such queries and requests are implemented via :doc:`RPC
calls<../developer/rpc>`. A client can query the chain’s state and can inject
blocks and operations into a node. One particular client is the :ref:`baker daemon <baker_run>`,
which is associated to an account. In particular the baker has access to the
account’s private key and thus can sign blocks and operations.

The main reason for using such a client-server architecture is safety: to insulate
the component that has access to the client keys, i.e. the baker, from the
component which is exposed to the internet, i.e. the node. Indeed, the node and
the baker can sit on different computers and the baker does not need to be
exposed to the internet. So nodes manage communication and shield bakers from
network attacks, and bakers hold secrets and bake blocks into the blockchain.

Another advantage of this architecture is that bakers can more easily have
different implementations, and this is important, for instance because different bakers may want
to implement different transaction selection strategies.

Tezos is a self-amending blockchain, in that a large part of Tezos can be
changed through a so-called amendement procedure. To this end, as mentioned in
:doc:`the big picture<../shell/the_big_picture>`, a Tezos node consists of two
components:

- the shell, which comprises the network and storage layer, and embeds
- the economic protocol component, which is the part that can be changed through amendment.

The role of the protocol
~~~~~~~~~~~~~~~~~~~~~~~~

At a very high level, a protocol must:

- implement protocol-specific types, such as the type of operations or protocol-specific block header data (in addition to the shell generic header),
- define under which conditions a block is a valid extension of the current blockchain, and define an ordering on blocks to arbitrate between concurrent extensions.

Validity conditions are implemented in the ``apply`` function which is called
whenever the node processes a block. The ``apply`` function takes as arguments a
*context* and a block. The context represents the *protocol state* and is
therefore protocol specific. The context may contain, for instance, a list of
accounts and their balances. More generally, the context must provide enough
information to determine the validity of a block. Given a context and a block,
the ``apply`` function returns the updated context if the block is valid and has
a higher :ref:`fitness<fitness_jakarta>`. The fitness determines a total ordering between blocks.

.. _shell_proto_interact_jakarta:

Shell-protocol interaction
~~~~~~~~~~~~~~~~~~~~~~~~~~

:doc:`Recall<../shell/the_big_picture>` that the economic protocol and the shell interact in order to ensure that the blocks being appended to the blockchain are valid. There are mainly two rules that the shell uses when receiving a new block:

- The shell does not accept a branch whose fork point is in a cycle more than ``PRESERVED_CYCLES`` in the past. More precisely, if ``n`` is the current cycle, :ref:`the last allowed fork point<lafl>` is the first level of cycle ``n-PRESERVED_CYCLES``. The parameter ``PRESERVED_CYCLES`` therefore plays a central role in Tezos: any block before the last allowed fork level is immutable.
- The shell changes the head of the chain to this new block only if the block is :doc:`valid<../shell/validation>` and has a higher fitness than the current head; a block is valid if the operations it includes are valid.


Blocks
~~~~~~

A block consists of a header and operations. A block's header is
composed of two parts: :ref:`the protocol-agnostic part<shell_header>`
and :ref:`the protocol-specific part<shell_proto_revisit_jakarta>`.
This separation enables the shell to interact with different
protocols.

.. _validation_passes_jakarta:

Operations & Validation Passes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The different kinds of operations are grouped in classes, such that operations belonging to different classes may be validated independently, and/or with different priorities.
Each class has an associated index, called a :ref:`validation pass<shell_header>`.
There are four classes of operations: :doc:`consensus <consensus>` operations, :doc:`voting <voting>` operations, anonymous operations, manager operations.

Consensus operations are endorsements, while `voting <voting>` operations are ballot and proposal.

Anonymous operations are operations which are not signed. There are three anonymous operations: seed nonce revelation, double baking evidence, and double endorsing evidence. The evidence for double baking and double endorsing is included in a block by the so-called accuser (see :ref:`slashing<slashing_jakarta>`).

Manager operations are activation, origination (see :doc:`smart contracts<michelson>`), transaction, reveal, and delegation (see :doc:`proof of stake <proof_of_stake>`). Manager operations are the only fee-paying operations.

Recall that users have associated :ref:`accounts <Account>` which they activate before being able to participate. By means of the operation :ref:`origination<Origination>`, accounts can be further associated with smart contracts in which they are called :ref:`originated accounts<originated account>`. :ref:`Transactions<transaction>` are used to either transfer tez between two accounts or run the code of a smart contract. Transactions are signed by an account's private key. Before making a transaction, a user must reveal her public key so that other users (not being aware of this public key) can effectively check the signature of the transaction.

Manager operations can be grouped into batches forming a so-called group operation. A group operation satisfies:

- atomicity: either all the operations in the batch succeed or none is applied
- efficiency: the whole batch is signed only once (by the same implicit account), thus it is much more efficient to check, and it requires much less gas
- usability: the batch only increments the counter of the signer account by one; for this reason it is easier for tools to provide sending several operations per block using operation batches than tracking counter changes.

The list of operations can be obtained with :ref:`this rpc <GET_..--block_id--operations>`.

See also
~~~~~~~~

An in-depth description of the inners of a protocol can be found in the blog
post `How to write a Tezos protocol
<https://research-development.nomadic-labs.com/how-to-write-a-tezos-protocol.html>`_.
