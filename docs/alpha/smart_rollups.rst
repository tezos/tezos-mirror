Smart Optimistic Rollups
========================

A **rollup** is a processing unit that receives, retrieves, and
interprets input messages to update its local state and to produce
output messages targetting the Tezos blockchain. In this
documentation, we will generally refer to the rollup under
consideration as the layer-2 on top of the Tezos blockchain,
considered as the layer-1.

Rollups are a permissionless scaling solution for the Tezos
blockchain.  Indeed, anyone can originate and operate one or more
rollups, allowing to (almost) arbitrarily increase the throughput of
the Tezos blockchain.

The integration of these rollups in the Tezos protocol is
*optimistic*: this means that when an operator publishes a claim about
the state of the rollup, this claim is *a priori* trusted. However, a
refutation mechanism allows anyone to economically punish a
participant that has published an invalid claim. Therefore, thanks to
the refutation mechanism, a single honest participant is enough to
guarantee that the input messages are correctly interpreted.

In the Tezos protocol, the subsystem of smart rollups is generic with
respect to the syntax and the semantics of input messages. This means
that any computational device can be used to implement a rollup as
long as this device can produce a proof justifying the correct
application of the official semantics for this device. In the Tezos
protocol, official semantics are implemented as **Proof-generating
Virtual Machines (PVM)**. Thus, a rollup behavior is characterized by
a given PVM. Any implementation of this device can be used to run the
rollup provided that it is compliant with the PVM defined by the
economic protocol.

The smart rollup infrastructure can currently be instantiated with a
WebAssembly PVM (WASM) provided by the protocol. A WASM rollup runs a
WASM program named a **kernel**. The role of the kernel is to process
input messages, to update a state, and to output messages targetting
the layer-1 following a user-defined logic. Anyone can develop a
kernel or reuse existing kernels. A typical use case of WASM rollups
is to deploy a kernel that implements the Ethereum Virtual Machine
(EVM) and to get as a result an EVM-compatible layer-2 running on top
of the Tezos blockchain. WASM rollups are not limited to this use case
though: they are fully programmable, hence their names, smart
optimistic rollups, as they are very close to smart-contracts in terms
of expressivity.

The purpose of this documentation is to give:

#. an overview of the terminology and basic principles of smart rollups ;
#. a complete tour of smart rollups related workflows ;
#. a reference documentation for the development of a WASM kernel.

Overview
--------

Just like smart-contracts, rollups are decentralized software
components. As such, any user can originate, operate, and interact
with a rollup. For the sake of clarity, we will distinguish three
kinds of users in this documentation: operators, kernel developers,
and end-users. An operator wants the rollup to make progress. A kernel
developer wants to write a kernel to be executed within a rollup. An
end-user interacts with the rollup through layer-1 operations or
layer-2 input messages.

Address
^^^^^^^

Originating a smart rollup leads to a smart rollup address that
uniquely identifies the smart rollup on layer-1. A smart rollup
address starts with the prefix ``scr1``
(see also the :ref:`kinds of address prefixes in Tezos <address_prefixes_alpha>`).

Inputs
^^^^^^

There are two channels of communication to interact with smart rollups:

#. a global **rollups inbox** allows the the layer-1 to transmit
   information to all the rollups. This unique inbox contains two
   kinds of messages: *external* messages are pushed through a layer-1
   manager operation while *internal* messages are pushed by layer-1
   smart-contract or the protocol itself. More information about these
   two kinds of messages can be found below.

#. a **reveal data channel** allows the rollup to retrieve data
   coming from data sources external to the layer-1. More information
   about this channel can be found below.


External messages
"""""""""""""""""

Anyone can push a message to the rollups inbox. This message is a mere
sequence of bytes following no particular underlying format. The
interpretation of this sequence of bytes is the responsibility of the
kernel.

There are two ways for an end-user to push an external message to the
rollups inbox: first, she can inject the dedicated layer-1 operation
using the Octez client (see command ``send sc rollup message
<messages> from <src>``) ; second, she can use the batcher
of a smart rollup node. More details can be found
:ref:`sending_external_inbox_message`.

Internal messages
"""""""""""""""""

A rollup is identified by an address and is assigned a Michelson type
so that any layer-1 smart contract can perform a transfer to this
address with a payload of this type. By contrast to external messages
that are meant for end-users, this transfer is realized as an internal
message pushed to the rollups inbox.

In addition, the Tezos protocol pushes a start-of-level internal
message (respectively, an end-of-level internal message) in the inbox,
at the beginning (respectively, at the end) of the validation of each
layer-1 block.

Reveal data channel
"""""""""""""""""""

The reveal data channel is a communication interface that allows the
rollup to request data from sources that are external to the inbox and
can be unknown to the layer-1. The rollup node has the responsibility
to answer the rollup requests: this is usually done by issuing RPC
to an octez node or by reading the contents of a local file.

A rollup can do the following requests through the reveal data channel:

#. **preimage requests** The rollup can request a page, that is 4kB of
   data, provided that it knows a (blake2b) hash of this sequence of
   bytes. The request is fulfilled by the rollup node
   :ref:`populating_the_reveal_channel`.

#. **metadata requests** The rollup can request information from the
   protocol, namely the address and the origination level of the
   rollup node itself. The rollup node retrieves this information
   through RPCs to answer the rollup.

Information passing through the reveal data channel does not have to
be considered by the layer-1: for this reason, the volume of
information is not limited by the bandwidth of the layer-1. Thus, the
reveal data channel can be used to upload large volume of data to the
rollup.

Origination
^^^^^^^^^^^
When originated, a rollup is characterized by the name of the device
it runs – the so-called Proof-generating Virtual Machine (PVM) – by
the source code of the rollup running under this device, and by the
Michelson type of the entrypoint used by layer-1 smart-contracts to
communicate with the rollup through internal messages.

Processing
^^^^^^^^^^
Each time a Tezos block is finalized, a rollup reacts to three kinds
of events: the beginning of the block, the input messages contained in
that block, and the end of the block. A **rollup node** implements this
reactive process: it downloads the Tezos block and interprets it
according to the semantics of the PVM. This interpretation can require
updating a state, downloading data from other sources, or performing
some cryptographic verifications. The state of the rollup contains
an **outbox**, that is a sequence of latent calls to layer-1 contracts.

The behavior of the rollup node is deterministic and fully specified
by a reference implementation of the PVM embedded in the
protocol. Notice that the PVM implementation is meant for
verification, not performance: for this reason, a rollup node does not
normally run a PVM to process inputs but a **fast execution engine**
(e.g., WASMER for the WASM PVM in the case of the rollup node
distributed with Octez). This fast execution engine implements the
exact same semantics as the PVM.

Commitments
^^^^^^^^^^^

Starting from the rollup origination level, levels are partitioned
into **commitment periods** of 30 consecutive blocks.

A **commitment** claims that the interpretation of all inbox messages
published during a given commitment period and applied on the state of
a parent commitment led to a given new state by performing a given
number of execution steps of the PVM. Execution steps are called
**ticks** in the smart rollups terminology. A commitment must be
published on the layer-1 after each commitment period to have the rollup
progress. A commitment is always based on a parent commitment (except
for the genesis commitment that is automatically published at
origination time).

Notice that, to publish a commitment, an operator must provide a
deposit of 10,000 tez. For this reason, the operator is said to be a
**staker**. Several users can stake on the same commitment. When a
staker publishes a new commitment based on a commitment she is staking
on, she does not have to provide a new deposit: the deposit also
applies to this new commitment.

A commitment is optimistically trusted but it can be refuted until it
is said to be **cemented** (i.e., final, unchangeable). Indeed, right
after a commitment is published, a two-weeks refutation period
starts. During the refutation period, anyone noticing that a
commitment for a given commitment period is invalid can post a
concurrent commitment for the same commitment period to force the
removal of the invalid commitment. If no one posts such a concurrent
commitment during the refutation period, the commitment can be
cemented with a dedicated operation injected in layer-1, and the
outbox messages can be executed by the layer-1 by an explicit layer-1
operation (see :ref:`triggering_execution_outbox_message`), typically
to transfer assets from the rollup to the layer-1.

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
tick on which they disagree. During the final phase, the stakers must
provide a proof that they correctly interpreted this conflicting
tick.

The layer-1 PVM then determines whether these proofs are valid. There
are only two possible outcomes: either one of the staker has provided
a valid proof, she wins the game, and is rewarded with half of the
opponent's deposit (the other half being burnt) ; or, both stakers have
provided an invalid proof and they both lose their deposit. In the
end, at most one stake will be kept in the commitment tree. When a
commitment has no more stake on it (because all stakers have lost the
related refutation games), it is removed from the tree. An honest
player must therefore play as many refutation games as there are
stakes on the commitments in conflict with her own commitment.

Finally, notice that each player is subject to a timer similar to a
chess clock, allowing each player to play only up to one week: after
this time is elapsed, a player can be dismissed by any layer-1 user
playing a timeout operation. Thus, the refutation game played by the
two players can last at most 2 weeks.

There is no timeout for starting a refutation game after having
published a concurrent commitment. However, assuming the existence of
a honest participant, she will start the refutation game with all
concurrent stakers to avoid the rollup being stuck.

Workflows
---------

Tools
^^^^^

Smart rollups come with two new executable programs: the Octez
rollup node and the Octez rollup client.

The Octez rollup node is used by a rollup operator to deploy a
rollup. The rollup node is responsible for making the rollup progress
by publishing commitments and by playing refutation games.

Just like the Octez node, the Octez rollup node provides an RPC
interface :doc:`RPC <../api/openapi>`. The services of this interface
can be called directly with HTTP requests or indirectly using the
Octez rollup client.

Prerequisites
^^^^^^^^^^^^^

To experiment with the commands described in this section, we use
the `Mondaynet <https://teztnets.xyz/mondaynet-about>`_.
In this section, we assume that ``${OPERATOR_ADDR}`` is a valid
implicit account on MondayNet owned by the reader.

Notice that you need a specific development version of Octez to
participate to MondayNet. This version is either available from
docker images or can be compiled from sources. Please refer to the
`Mondaynet <https://teztnets.xyz/mondaynet-about>`_ website
for installation details.

An Octez rollup node needs an Octez tezos node to run. We assume that
a rollup node has been launched locally, typically by issuing:

.. code:: sh

   octez-node config init --data-dir "${ONODE_DIR}" --network "${NETWORK}"
   octez-node run --data-dir "${ONODE_DIR}" --network "${NETWORK}" --rpc-addr 127.0.0.1

in a terminal where ``${NETWORK}`` is of the
form ``https://teztnets.xyz/mondaynet-YYYY-MM-DD``
and ``${ONODE_DIR}`` is a path for the Octez node store.

The commands will only work when ``proto_alpha`` is activated.
This can be checked by:

.. code:: sh

   octez-client rpc get /chains/main/blocks/head/protocols

that must return:

::

   { "protocol": "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK",
     "next_protocol": "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK" }

Finally, you need to check that your balance is greater than 10,000
tez to make sure that staking is possible. In case your balance is not
sufficient, you can get test tokens from :ref:`a faucet <faucet>`.


.. code:: sh

   octez-client get balance for "${OPERATOR_ADDR}"

Origination
^^^^^^^^^^^

Anyone can originate a smart rollup with the following invocation of
the Octez client:

.. code:: sh

    octez-client originate sc rollup from "${OPERATOR_ADDR}" \
      of kind wasm_2_0_0 \
      of type bytes \
      booting with "${KERNEL}" \
      -burn-cap 999

where ``${KERNEL}`` is a hex representation of a WebAssembly
bytecode serving as an initial program to boot on. From a
WASM bytecode file named ``kernel.wasm``, such representation
can be obtained through

.. code:: sh

     xxd -ps -c 0 <kernel.wasm>

To experiment, we propose that you use the value for ``${KERNEL}``
defined in the :download:`given file <sr_boot_kernel.sh>`.

If everything went well, the origination command results in:

::

  This sequence of operations was run:
    Manager signed operations:
      From: tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX
      Fee to the baker: ꜩ0.000357
      Expected counter: 10
      Gas limit: 1000
      Storage limit: 0 bytes
      Balance updates:
        tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX ... -ꜩ0.000357
        payload fees(the block proposer) ....... +ꜩ0.000357
      Revelation of manager public key:
        Contract: tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX
        Key: edpkukxtw4fHmffj4wtZohVKwNwUZvYm6HMog5QMe9EyYK3QwRwBjp
        This revelation was successfully applied
        Consumed gas: 1000
    Manager signed operations:
      From: tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX
      Fee to the baker: ꜩ0.004617
      Expected counter: 11
      Gas limit: 3227
      Storage limit: 10711 bytes
      Balance updates:
        tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX ... -ꜩ0.004617
        payload fees(the block proposer) ....... +ꜩ0.004617
      Smart contract rollup origination:
        Kind: wasm_2_0_0
        Parameter type: bytes
        Boot sector Blake2B hash: '789431137a40057a39867cbc5cd7f984139360559c655c0508821b9be8047a02'
        This smart contract rollup origination was successfully applied
        Consumed gas: 3126.633
        Storage size: 10691 bytes
        Address: scr1BBMMrm3Zhq1S2Qy2LpRXdu4ebtW9sBrtY
        Genesis commitment hash: scc13y58tqmHJtWhWY5Sa3BLu7W3FUjsFZrLCyvdu78VTnmf4aEVWe
        Balance updates:
          tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX ... -ꜩ2.67275
          storage fees ........................... +ꜩ2.67275

The address ``scr1BBMMrm3Zhq1S2Qy2LpRXdu4ebtW9sBrtY`` is the smart rollup address.
Let's write it ``${SOR_ADDR}`` from now on.

Deploying a rollup node
^^^^^^^^^^^^^^^^^^^^^^^

Now that the rollup is originated, anyone can make it progress by deploying a
rollup node.

First, we need to decide on a directory where the rollup node stores
its data. Let us assign ``${ROLLUP_NODE_DIR}`` with this path. The
rollup node is configured with the following command:

.. code:: sh

   octez-sc-rollup-node-alpha --base-dir "${OCLIENT_DIR}" \
                    init operator config for "${SOR_ADDR}" \
                    with operators "${OPERATOR_ADDR}" \
                    --data-dir "${ROLLUP_NODE_DIR}"

This creates a configuration file:

::

   Smart-contract rollup node configuration written in {ROLLUP_NODE_DIR}/config.json

Here is the content of the file:

::

  {
    "data-dir": "${ROLLUP_NODE_DIR}",
    "sc-rollup-address": "${SOR_ADDR}",
    "sc-rollup-node-operator": {
      "publish": "${OPERATOR_ADDR}",
      "add_messages": "${OPERATOR_ADDR}",
      "cement": "${OPERATOR_ADDR}",
      "refute": "${OPERATOR_ADDR}"
    },
    "fee-parameters": {},
    "mode": "operator"
  }

Notice that distinct layer-1 adresses could be used for the layer-1
operations issued by the rollup node simply by editing the
configuration file to set different addresses for ``publish``,
``add_messages``, ``cement``, and ``refute``.

In addition, a rollup node can run under different modes:

#. ``operator`` activates a full-fledged rollup node. This means that
   the rollup node will do everything needed to make the rollup
   progress. This includes following the layer-1 chain, reconstructing
   inboxes, updating the states, publishing and cementing commitments
   regularly, and playing the refutation games. In this mode, the
   rollup node will accept transactions in its queue and batch them on
   the layer-1.  It does not include the message batching service,
   either.

#. ``batcher`` means that the rollup node will accept transactions in
   its queue and batch them on the layer-1. In this mode, the rollup
   node follows the layer-1 chain, but it does not update its state
   and does not reconstruct inboxes. Consequently, it does not publish
   commitments nor play refutation games.


#. ``observer`` means that the rollup node follows the layer-1 chain
   to reconstruct inboxes, to update its state. However, it will
   neither publish commitments, nor play a refutation game.
   It does not include the message batching service, either.


#. ``maintenance`` is the same as the operator mode except that it does not
   include the message batching service.

The following table summarizes the operation modes, focusing on the L1
operations which are injected by the rollup node in each mode.

+-------------+--------------+----------+--------+--------+
|             | Add messages | Publish  | Cement | Refute |
+=============+==============+==========+========+========+
| Operator    | Yes          | Yes      | Yes    | Yes    |
+-------------+--------------+----------+--------+--------+
| Batcher     | Yes          | No       | No     | No     |
+-------------+--------------+----------+--------+--------+
| Observer    | No           | No       | No     | No     |
+-------------+--------------+----------+--------+--------+
| Maintenance | No           | Yes      | Yes    | Yes    |
+-------------+--------------+----------+--------+--------+

Second, the configured rollup node can be run:

.. code:: sh

   octez-sc-rollup-node-alpha" -d "${OCLIENT_DIR}" run --data-dir ${ROLLUP_NODE_DIR}

The log should show that the rollup node follows the layer-1 chain and
processes the inbox of each level.

.. _sending_external_inbox_message:

Sending an external inbox message
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _sending_internal_inbox_message:

Sending an internal inbox message
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _triggering_execution_outbox_message:

Triggering the execution of an outbox message
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _populating_the_reveal_channel:

Populating the reveal channel
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is the responsibility of rollup node operators to get the data
passed through the reveal data channel when the rollup requested it.

To answer a request for a page of hash ``H``, the rollup node tries to
read the content of a file ``H`` named
``${ROLLUP_NODE_DIR}/wasm_2_0_0``.

Notice that a page cannot exceed 4KB. Hence, larger pieces of data
must be represented with multiple pages that reference each other
through hashes. It is up to the kernel to decide how to implement
this. For instance, one can classify pages into two categories: index
pages that are hashes for other pages and leaf pages that contain
actual payloads.

Developing WASM Kernels
-----------------------

Glossary
--------

#. **PVM**: A Proof-generating Virtual Machine is a reference
   implementation for a device on top of which a smart rollup can be
   executed. This reference implementation is part of the Tezos
   protocol and is the unique source of truth regarding the semantics
   of rollups. The PVM is able to produce proofs enforcing this truth.
   This ability is used during the final step of refutation games.

#. **Inbox**: A sequence of messages from the layer-1 to smart rollups.
   The contents of the inbox is determined by the consensus of the
   Tezos protocol.

#. **Outbox**: A sequence of messages from a smart rollup to the layer-1.
   Messages are smart contract calls, potentially containing tickets.
   These calls can be triggered only when the related commitment is
   cemented (hence, at least two weeks after the actual execution of
   the operation).

#. **Commitment period**: A period of 30 blocks during which all inbox
   messages must be processed by the rollup node state to compute a
   commitment. A commitment must be published for each commitment
   period.

#. **Refutation period**: At the end of each commitment period, a
   period of two weeks starts to allow any commitment related to
   this commitment period to be published.

#. **Staker**: An implicit account that has made a deposit on a
   commitment.

#. **Refutation game**: A process by which the Tezos protocol solves
   a conflict between two stakers.
