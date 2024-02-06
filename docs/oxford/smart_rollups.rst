Smart Optimistic Rollups
========================

A **rollup** is a processing unit that receives, retrieves and
interprets input messages to update its local state and to produce
output messages targetting the Tezos blockchain. In this
documentation, we will generally refer to the rollup under
consideration as the Layer 2 on top of the Tezos blockchain,
considered as the Layer 1.

Rollups are a permissionless scaling solution for the Tezos
blockchain.  Indeed, anyone can originate and operate one or more
rollups, allowing to increase the throughput of the Tezos blockchain,
(almost) arbitrarily.

The purpose of this documentation is to provide an overview of the terminology and basic principles of smart rollups.
In the :doc:`../shell/smart_rollup_node`, we provide a complete tour
of smart rollups related workflows and reference documentation for the development of a WASM kernel.

The integration of these rollups in the Tezos protocol is
*optimistic*: this means that when a participant publishes a claim
about the state of the rollup, this claim is *a priori*
trusted. However, a refutation mechanism allows anyone to economically
punish someone who has published an invalid claim. Therefore, thanks
to the refutation mechanism, a single honest participant is enough to
guarantee that the input messages are correctly interpreted.

In the Tezos protocol, the subsystem of smart rollups is generic with
respect to the syntax and the semantics of the input messages. More
precisely, the originator of a smart rollup provides a program named a
**kernel** (in one of the languages supported by Tezos) responsible
for interpreting input messages. During the refutation mechanism, the
execution of this kernel is handled by a **Proof-generating Virtual
Machine (PVM)** for this language, provided by the Tezos protocol,
which allows to prove that the result of applying an input message to
the rollup context is correct. The rest of the time, any VM
implementation of the chosen language can be used to run the smart
rollup kernel, provided that it is compliant with the PVM.

The smart rollup infrastructure currently supports the WebAssembly
language. A WASM rollup runs a kernel expressed in WASM. The role of the kernel is
to process input messages, to update a state, and to output messages
targeting the Layer 1 following a user-defined logic. Anyone can
develop a kernel or reuse existing kernels. A typical use case of WASM
rollups is to deploy a kernel that implements the Ethereum Virtual
Machine (EVM) and to get as a result an EVM-compatible Layer 2 running
on top of the Tezos blockchain. WASM rollups are not limited to this
use case though: they are fully programmable, hence their names, smart
optimistic rollups, as they are very close to smart contracts in terms
of expressiveness.

Overview
--------

Just like smart contracts, smart rollups are decentralized software
components. However, contrary to smart contracts that are processed
by the network validators automatically, a smart rollup requires
a dedicated *rollup node* to function.

Any user can originate, operate, and interact with a rollup. For the
sake of clarity, we will distinguish three kinds of users in this
documentation: operators, kernel developers, and end-users. An
operator deploys the rollup node to make the rollup progress. A kernel
developer writes a kernel to be executed within a rollup. An end-user
interacts with the rollup through Layer 1 operations or Layer 2 input
messages.

Address
^^^^^^^

When a smart rollup is originated on the Layer 1, a unique address is
generated to uniquely identify it. A smart rollup address starts with
the prefix ``sr1``
(see also the :ref:`kinds of address prefixes in Tezos <address_prefixes_oxford>`).

Inputs
^^^^^^

There are two channels of communication to interact with smart rollups:

#. a global **rollups inbox** allows the Layer 1 to transmit
   information to all the rollups.

#. a **reveal data channel** allows each rollup to retrieve data
   coming from data sources external to the Layer 1. Rollups request
   data through that channel to the runner of that rollup kernel
   (i.e. the smart rollup node).

Rollups inbox
"""""""""""""

A single global inbox serves all rollups and contains two kinds of messages:
*external* messages are pushed through a Layer 1 manager operation
while *internal* messages are pushed by Layer 1 smart contracts or by
the protocol itself. All messages (external and internal) pushed to
the inbox also contain the Layer 1 level of their insertion and a
counter. The counter is the index of the message and it is reset at
each Layer 1 level.

External messages
'''''''''''''''''

Anyone can push a message to the rollups inbox. This message is a mere
sequence of bytes following no particular underlying format. The
interpretation of this sequence of bytes is the responsibility of each
kernel.

There are two ways for end-users to push an external message to the
rollups inbox: first, they can inject the dedicated Layer 1 operation
using the Octez client (see command ``send smart rollup message
<messages> from <src>``); second, they can use the batcher
of a smart rollup node. More details can be found in the :ref:`sending_external_inbox_message`.

Internal messages
'''''''''''''''''

Contrary to external messages, which are submitted by the end users,
internal messages are constructed by the Layer 1.

At the beginning of every Tezos block, the Layer 1 pushes two internal
messages: “Start of level”, and “Info per level”. “Start of level”
does not have any payload associated to it, while “Info per level”
provides to the kernel the timestamp and block hash of the predecessor
of the current Tezos block. If the Tezos block is the first block of a
protocol, then the Layer 1 pushes another message “Protocol migration”
just after the “Info per level” that provides the new protocol version
(i.e. ``<proto-name>_<NNN>``).

A rollup is identified by an address and has an associated Michelson
type (defined at origination time). Any Layer 1 smart contract can
perform a transfer to this address with a payload of this type. This
transfer is realized as an internal message pushed to the rollups
inbox.

Finally, after the application of the operations of the Tezos block,
the Layer 1 pushes one final internal message “End of
level”. Similarly to “Start of level“, these internal messages does not
come with any payload.

.. _reveal_data_channel_smart_rollups:
.. _reveal_data_channel_smart_rollups_oxford:

Reveal data channel
"""""""""""""""""""

The reveal data channel is a communication interface that allows the
rollup to request data from sources that are external to the inbox and
can be unknown to the Layer 1. The rollup node has the responsibility
to answer the rollup requests.

A rollup can do the following requests through the reveal data channel:

#. **preimage requests**: The rollup can request arbitrary data of at
   most 4kBytes, provided that it knows its (blake2b) hash. The
   request is fulfilled by the rollup node, see :ref:`populating_the_reveal_channel`.

#. **metadata requests** The rollup can request information from the
   protocol, namely the address and the origination level of the
   rollup node itself. The rollup node retrieves this information
   through RPCs to answer the rollup.

Information passing through the reveal data channel does not have to
be considered by the Layer 1: for this reason, the volume of
information is not limited by the bandwidth of the Layer 1. Thus, the
reveal data channel can be used to upload large volumes of data to the
rollup.

Origination
^^^^^^^^^^^

A smart rollup is characterized by:
- the kind of Proof-generating Virtual Machine (PVM),
- the kernel written in a language that the PVM can interpret,
- the Michelson type of the entrypoint used by Layer 1 smart contracts
to send internal messages to it, and
- an optional list of addresses used as a white-list of allowed
stakers (see :ref:`private_rollups_oxford`).

All these characteristics are provided when originating a new smart
rollup.

Processing
^^^^^^^^^^
Each time a Tezos block is finalized, a rollup reacts to three kinds
of events: the beginning of the block, the input messages possibly
contained in that block, and the end of the block. A **rollup node**
implements this reactive process: it downloads the Tezos block and
interprets it according to the semantics of the PVM. This
interpretation can require updating a state, downloading data from
other sources, or performing some cryptographic verifications. The
state of the rollup contains an **outbox**, which is a sequence of
latent calls to Layer 1 contracts.

The behavior of the rollup node is deterministic and fully specified
by a reference implementation of the PVM embedded in the
protocol. Notice that the PVM implementation is meant for
verification, not performance: for this reason, a rollup node does not
normally run a PVM to process inputs but a **fast execution engine**
(e.g., based on the Wasmer runtime for the WASM PVM in the case of the
rollup node distributed with Octez). This fast execution engine
implements the exact same semantics as the PVM. The PVM is only ever
used by the rollup node when it needs to produce a proof during the
last step of the refutation mechanism.

Commitments
^^^^^^^^^^^

Starting from the rollup origination level, levels are partitioned
into **commitment periods** of 60 consecutive blocks.

A **commitment** claims that the interpretation of all inbox messages
published during a given commitment period, and applied on the state of
a parent commitment, led to a given new state by performing a given
number of execution steps of the PVM. Execution steps are called
**ticks** in Smart Rollups terminology.

A commitment must be
published on the Layer 1 any time after each commitment period, to have the rollup
progress.
A new commitment period starts right after the previous commitment period, no matter if commitments were published or not for the previous commitment period(s).
For example, if an operator rollup node stops running for one day long, when it comes back, it will be able to resume publishing commitments for the passed periods, in chronological order.
Indeed, a commitment is always based on a parent commitment (except
for the genesis commitment that is automatically published at
origination time), so publishing a commitment fails if the parent commitment has not yet been published.

Since the PVM is deterministic and the inputs are completely
determined by the Layer 1 rollups inbox and the reveal channel, there
is only one honest commitment. In other words, if two distinct
commitments are published for the same commitment period, one of them
must be wrong.

Notice that, to publish a commitment, an operator must provide a
deposit of 10,000 tez. For this reason, the operator is said to be a
**staker**. Several users can stake on the same commitment. When a
staker *S* publishes a new commitment based on a commitment that *S* is staking
on, *S* does not have to provide a new deposit: the deposit also
applies to this new commitment.

There is no need to synchronize between operators: if two honest
operators publish the same commitment for a given commitment period,
the commitment will be published with two stakes on it.

A commitment is optimistically trusted but it can be refuted until it
is said to be **cemented** (i.e., final, unchangeable). Indeed, right
after a commitment is published, a two-weeks refutation period
starts. During the refutation period, anyone noticing that a
commitment for a given commitment period is invalid can post a
concurrent commitment for the same commitment period to force the
removal of the invalid commitment. If no one posts such a concurrent
commitment during the refutation period, the commitment can be
cemented with a dedicated operation injected in Layer 1, and the
outbox messages can be executed by the Layer 1 by an explicit Layer 1
operation (see :doc:`../shell/smart_rollup_node`), typically
to transfer assets from the rollup to the Layer 1.

The outbox messages can follow three different formats. Firstly, the
Layer 1 operations contained in the outbox messages can be left
untyped, meaning only the Micheline expression is provided by the
kernel. Before executing the transaction, the Layer 1 typechecks said
expression against the expected type of the targeted entrypoint. Since
Nairobi, it is also possible for the kernel to provide its expected
type of the targeted entrypoint. This additional safety mechanism is
to avoid type confusion: namely, a kernel transferring a tuple that
the Layer 1 interprets as a ticket. Lastly, the outbox message can
contain a white-list update. This message can only be executed for a
rollup that is private since its origination (see
:ref:`private_rollups_oxford`).

Refutation
^^^^^^^^^^

Because of concurrent commitments, a rollup is generally related to a
**commitment tree** where branches correspond to different claims
about the rollup state.

By construction, only one view of the rollup state is valid (as the
PVM is deterministic). When two concurrent branches exist in the
commitment tree, the cementation process is stopped at the first fork
in the tree. To unfreeze the cementation process, a **refutation
game** must be started between *two concurrent stakers* of these
branches. Refutation games are automatically played by rollup nodes to
defend their stakes: honest participants are guaranteed to win these
games. Therefore, an honest participant should not have to worry about
refutation games. Finally, a running refutation game does not prevent
new commitments to be published on top of the disputed commitments.

A refutation game is decomposed into two main steps: a dissection
mechanism and a final conflict resolution phase. During the first
phase, the two stakers exchange hashes about intermediate states of
the rollups in a way that allows them to converge to the very first
tick on which they disagree. The exact number of hashes exchanged at a
given step is PVM-dependent. During the final phase, the stakers must
provide a proof that they correctly interpreted this conflicting tick.

The Layer 1 PVM then determines whether these proofs are valid. There
are only two possible outcomes: either one of the stakers, that we dub *S* in the sequel, has provided
a valid proof, then *S* wins the game, and is rewarded with half of the
opponent's deposit (the other half being burnt); or, both stakers have
provided an invalid proof and they both lose their deposit. In the
end, at most one stake will be kept in the commitment tree. When a
commitment has no more stake on it (because all stakers have lost the
related refutation games), it is removed from the tree. An honest
player *H* must therefore play as many refutation games as there are
stakes on the commitments in conflict with *H*'s own commitment.

Finally, notice that each player is subject to a timer similar to a
chess clock, allowing each player to play only up to one week: after
this time is elapsed, a player can be dismissed by any Layer 1 user
playing a timeout operation. Thus, the refutation game played by the
two players can last at most 2 weeks.

There is no timeout for starting a refutation game after having
published a concurrent commitment. However, assuming the existence of
an honest participant *H*, then *H* will start the refutation game with all
concurrent stakers to avoid the rollup getting stuck.

.. _private_rollups:
.. _private_rollups_oxford:

Private rollups
^^^^^^^^^^^^^^^

A **private** Smart Rollup guarantees that private data cannot be
leaked by any means, whereas in a public rollup, one can force a
rollup to leak part of the data by starting a refutation game. This is
achieved by restricting the set of allowed stakers using a
*whitelist*. With that restriction, only addresses on the whitelist
can publish commitments and therefore participate in a refutation
game.

The whitelist is optionally defined at origination. The rollup is
considered public if no white-list is defined, private otherwise. The
whitelist can be updated with a specific outbox message. This message
contains an optional list, the new list completely replaces the stored
whitelist in layer 1. If the message contains no list, then the
rollup becomes public. In turn, it is forbidden to make a public
rollup private by sending an outbox message with a non-empty
whitelist.

It is the responsibility of the kernel to maintain the white-list by
submitting outbox messages. Kernels must therefore implement their
own access control list logic to add and remove addresses.

Also, it is important to remember that because of the refutation
logic, an outbox message can only be executed when the associated
commitment has been cemented (see :doc:`../shell/smart_rollup_node`).

Glossary
--------

#. **PVM**: A Proof-generating Virtual Machine is a reference
   implementation for a device on top of which a smart rollup can be
   executed. This reference implementation is part of the Tezos
   protocol and is the unique source of truth regarding the semantics
   of rollups. The PVM is able to produce proofs enforcing this truth.
   This ability is used during the final step of refutation games.

#. **Inbox**: A sequence of messages from the Layer 1 to smart rollups.
   The contents of the inbox are determined by the consensus of the
   Tezos protocol.

#. **Outbox**: A sequence of messages from a smart rollup to the Layer 1.
   Messages are smart contract calls, potentially containing tickets.
   These calls can be triggered only when the related commitment is
   cemented (hence, at least two weeks after the actual execution of
   the operation).

#. **Commitment period**: A period of 60 blocks during which all inbox
   messages must be processed by the rollup node state to compute a
   commitment. A commitment must be published for each commitment
   period.

#. **Refutation period**: When the first commitment for a commitment period is published, a refutation
   period of two weeks starts to allow this commitment to be challenged.

#. **Staker**: An implicit account that has made a deposit on a
   commitment.

#. **Refutation game**: A process by which the Tezos protocol solves
   a conflict between two stakers.
