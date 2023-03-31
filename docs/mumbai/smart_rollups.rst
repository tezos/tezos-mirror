Smart Optimistic Rollups
========================

A **rollup** is a processing unit that receives, retrieves, and
interprets input messages to update its local state and to produce
output messages targetting the Tezos blockchain. In this
documentation, we will generally refer to the rollup under
consideration as the Layer 2 on top of the Tezos blockchain,
considered as the Layer 1.

Rollups are a permissionless scaling solution for the Tezos
blockchain.  Indeed, anyone can originate and operate one or more
rollups, allowing to increase the throughput of the Tezos blockchain,
(almost) arbitrarily.

The integration of these rollups in the Tezos protocol is
*optimistic*: this means that when an operator publishes a claim about
the state of the rollup, this claim is *a priori* trusted. However, a
refutation mechanism allows anyone to economically punish a
participant who has published an invalid claim. Therefore, thanks to
the refutation mechanism, a single honest participant is enough to
guarantee that the input messages are correctly interpreted.

In the Tezos protocol, the subsystem of smart rollups is generic with
respect to the syntax and the semantics of the input messages. More
precisely, the originator of a smart rollup provides a program (in one
of the languages supported by Tezos) responsible for interpreting
input messages. During the refutation mechanism, the execution of this
program is handled by a **Proof-generating Virtual Machine (PVM)** for
this language, provided by the Tezos protocol, which allows to prove
that the result of applying an input message to the rollup context is
correct. The rest of the time, any VM implementation of the chosen
language can be used to run the smart rollup program, provided that it
is compliant with the PVM.

The smart rollup infrastructure currently supports the WebAssembly
language. A WASM rollup runs a WASM program named a **kernel**. The
role of the kernel is to process input messages, to update a state,
and to output messages targeting the Layer 1 following a user-defined
logic. Anyone can develop a kernel or reuse existing kernels. A
typical use case of WASM rollups is to deploy a kernel that implements
the Ethereum Virtual Machine (EVM) and to get as a result an
EVM-compatible Layer 2 running on top of the Tezos blockchain. WASM
rollups are not limited to this use case though: they are fully
programmable, hence their names, smart optimistic rollups, as they are
very close to smart contracts in terms of expressiveness.

The purpose of this documentation is to give:

#. an overview of the terminology and basic principles of smart rollups;
#. a complete tour of smart rollups related workflows;
#. a reference documentation for the development of a WASM kernel.

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
(see also the :ref:`kinds of address prefixes in Tezos <address_prefixes_mumbai>`).

Inputs
^^^^^^

There are two channels of communication to interact with smart rollups:

#. a global **rollups inbox** allows the Layer 1 to transmit
   information to all the rollups. This unique inbox contains two
   kinds of messages: *external* messages are pushed through a Layer 1
   manager operation while *internal* messages are pushed by Layer 1
   smart contracts or the protocol itself.

#. a **reveal data channel** allows the rollup to retrieve data
   coming from data sources external to the Layer 1.


External messages
"""""""""""""""""

Anyone can push a message to the rollups inbox. This message is a mere
sequence of bytes following no particular underlying format. The
interpretation of this sequence of bytes is the responsibility of each
kernel.

There are two ways for an end-user to push an external message to the
rollups inbox: first, she can inject the dedicated Layer 1 operation
using the Octez client (see command ``send smart rollup message
<messages> from <src>``); second, she can use the batcher
of a smart rollup node. More details can be found in :ref:`sending_external_inbox_message_mumbai`.

Internal messages
"""""""""""""""""

Contrary to external messages, which are submitted by the end users,
internal messages are constructed by the Layer 1.

At the beginning of every Tezos block, the Layer 1 pushes two internal
messages: “Start of level”, and “Info per level”. “Start of level”
does not have any payload associated to it, while “Info per level”
provides to the kernel the timestamp and block hash of the predecessor
of the current Tezos block.

A rollup is identified by an address and has an associated Michelson
type (defined at origination time). Any Layer 1 smart contract can
perform a transfer to this address with a payload of this type. This
transfer is realized as an internal message pushed to the rollups
inbox.

Finally, after the application of the operations of the Tezos block,
the Layer 1 pushes one final internal message “End of
level”. Similarly to “Start of level“, this internal messages does not
come with any payload.


Reveal data channel
"""""""""""""""""""

The reveal data channel is a communication interface that allows the
rollup to request data from sources that are external to the inbox and
can be unknown to the Layer 1. The rollup node has the responsibility
to answer the rollup requests.

A rollup can do the following requests through the reveal data channel:

#. **preimage requests**: The rollup can request arbitrary data of at
   most 4kBytes, provided that it knows its (blake2b) hash. The
   request is fulfilled by the rollup node
   :ref:`populating_the_reveal_channel_mumbai`.

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
When originated, a rollup is characterized by the name of the device
it runs – the so-called Proof-generating Virtual Machine (PVM) – by
the source code of the rollup running under this device, and by the
Michelson type of the entrypoint used by Layer 1 smart contracts to
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
an **outbox**, that is a sequence of latent calls to Layer 1 contracts.

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
published on the Layer 1 after each commitment period to have the rollup
progress. A commitment is always based on a parent commitment (except
for the genesis commitment that is automatically published at
origination time).

Since the PVM is deterministic and the inputs are completely
determined by the Layer 1 rollups inbox and the reveal channel, there
is only one honest commitment. In other words, if two distinct
commitments are published for the same commitment period, one of them
must be wrong.

Notice that, to publish a commitment, an operator must provide a
deposit of 10,000 tez. For this reason, the operator is said to be a
**staker**. Several users can stake on the same commitment. When a
staker publishes a new commitment based on a commitment she is staking
on, she does not have to provide a new deposit: the deposit also
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
operation (see :ref:`triggering_execution_outbox_message_mumbai`), typically
to transfer assets from the rollup to the Layer 1.

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
this time is elapsed, a player can be dismissed by any Layer 1 user
playing a timeout operation. Thus, the refutation game played by the
two players can last at most 2 weeks.

There is no timeout for starting a refutation game after having
published a concurrent commitment. However, assuming the existence of
an honest participant, she will start the refutation game with all
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

An Octez rollup node needs an Octez node to run. We assume that
an Octez node has been launched locally, typically by issuing:

.. code:: sh

   octez-node config init --data-dir "${ONODE_DIR}" --network "${NETWORK}"
   octez-node run --data-dir "${ONODE_DIR}" --network "${NETWORK}" --rpc-addr 127.0.0.1

The commands will only work when ``Mumbai`` is activated.
This can be checked by:

.. code:: sh

   octez-client rpc get /chains/main/blocks/head/protocols

that must return:

::

   { "protocol": "PtMumbai2TmsJHNGRkD8v8YDbtao7BLUC3wjASn1inAKLFCjaH1",
     "next_protocol": "PtMumbai2TmsJHNGRkD8v8YDbtao7BLUC3wjASn1inAKLFCjaH1" }

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

    octez-client originate smart rollup from "${OPERATOR_ADDR}" \
      of kind wasm_2_0_0 \
      of type bytes \
      with kernel "${KERNEL}" \
      --burn-cap 999

where ``${KERNEL}`` is a hex representation of a WebAssembly
bytecode serving as an initial program to boot on. From a
WASM bytecode file named ``kernel.wasm``, such representation
can be obtained through

.. code:: sh

     xxd -ps -c 0 <kernel.wasm> | tr -d '\n'

To experiment, we propose that you use the value ``${KERNEL}``
defined in the :download:`given file <sr_boot_kernel.sh>`.

.. code:: sh

     source sr_boot_kernel.sh # defines shell variable KERNEL

If everything went well, the origination command results in:

::

   This sequence of operations was run:
     Manager signed operations:
       From: tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX
       Fee to the baker: ꜩ0.000357
       Expected counter: 36
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
       Fee to the baker: ꜩ0.000956
       Expected counter: 37
       Gas limit: 2849
       Storage limit: 6572 bytes
       Balance updates:
         tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX ... -ꜩ0.000956
         payload fees(the block proposer) ....... +ꜩ0.000956
       Smart rollup origination:
         Kind: wasm_2_0_0
         Parameter type: bytes
         Kernel Blake2B hash: '24df9e3c520dd9a9c49b447766e8a604d31138c1aacb4a67532499c6a8b348cc'
         This smart rollup origination was successfully applied
         Consumed gas: 2748.269
         Storage size: 6552 bytes
         Address: sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf
         Genesis commitment hash: src13wCGc2nMVfN7rD1rgeG3g1q7oXYX2m5MJY5ZRooVhLt7JwKXwX
         Balance updates:
           tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX ... -ꜩ1.638
           storage fees ........................... +ꜩ1.638


The address ``sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf`` is the smart rollup address.
Let's write it ``${SOR_ADDR}`` from now on.

Deploying a rollup node
^^^^^^^^^^^^^^^^^^^^^^^

Now that the rollup is originated, anyone can make it progress by deploying a
rollup node.

First, we need to decide on a directory where the rollup node stores
its data. Let us assign ``${ROLLUP_NODE_DIR}`` with this path. The
rollup node is configured with the following command:

.. code:: sh

   octez-smart-rollup-node-PtMumbai --base-dir "${OCLIENT_DIR}" \
                    init operator config for "${SOR_ADDR}" \
                    with operators "${OPERATOR_ADDR}" \
                    --data-dir "${ROLLUP_NODE_DIR}"

This creates a configuration file:

::

   Smart rollup node configuration written in ${ROLLUP_NODE_DIR}/config.json

Here is the content of the file:

::

  {
    "data-dir": "${ROLLUP_NODE_DIR}",
    "smart-rollup-address": "${SOR_ADDR}",
    "smart-rollup-node-operator": {
      "publish": "${OPERATOR_ADDR}",
      "add_messages": "${OPERATOR_ADDR}",
      "cement": "${OPERATOR_ADDR}",
      "refute": "${OPERATOR_ADDR}"
    },
    "fee-parameters": {},
    "mode": "operator"
  }

Notice that distinct Layer 1 adresses could be used for the Layer 1
operations issued by the rollup node simply by editing the
configuration file to set different addresses for ``publish``,
``add_messages``, ``cement``, and ``refute``.

In addition, a rollup node can run under different modes:

#. ``operator`` activates a full-fledged rollup node. This means that
   the rollup node will do everything needed to make the rollup
   progress. This includes following the Layer 1 chain, reconstructing
   inboxes, updating the states, publishing and cementing commitments
   regularly, and playing the refutation games. In this mode, the
   rollup node will accept transactions in its queue and batch them on
   the Layer 1.

#. ``batcher`` means that the rollup node will accept transactions in
   its queue and batch them on the Layer 1. In this mode, the rollup
   node follows the Layer 1 chain, but it does not update its state
   and does not reconstruct inboxes. Consequently, it does not publish
   commitments nor play refutation games.

#. ``observer`` means that the rollup node follows the Layer 1 chain
   to reconstruct inboxes, to update its state. However, it will
   neither publish commitments, nor play a refutation game.
   It does not include the message batching service either.

#. ``maintenance`` is the same as the operator mode except that it does not
   include the message batching service.

#. ``accuser`` follows the layer1-chain and computes commitments but does not
   publish them. Only when a conflicting commitment (published by another
   staker) is detected will the "accuser node" publish a commitment and
   participate in the subsequent refutation game.

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
| Accuser     | No           | Yes [*]_ | No     | Yes    |
+-------------+--------------+----------+--------+--------+

.. [*] An accuser node will publish commitments only when it detects
       conflicts; for such cases it must make a deposit of 10,000 tez.

Second, the configured rollup node can be run:

.. code:: sh

   octez-smart-rollup-node-PtMumbai -d "${OCLIENT_DIR}" run --data-dir ${ROLLUP_NODE_DIR}

The log should show that the rollup node follows the Layer 1 chain and
processes the inbox of each level.

Rollup node in a sandbox
""""""""""""""""""""""""

The node can also be tested locally with a sandbox environment. (See :doc:`sandbox documentation <../user/sandbox>`.)

Once you initialized the "sandboxed" client data with ``./src/bin_client/octez-init-sandboxed-client.sh``, you can run a sandboxed rollup node with ``octez-smart-rollup-node-PtMumbai run``.

A temporary directory ``/tmp/tezos-smart-rollup-node.xxxxxxxx`` will be used. However, a specific data directory can be set with the environment variable ``SCORU_DATA_DIR``.

.. _sending_external_inbox_message:
.. _sending_external_inbox_message_mumbai:

Sending an external inbox message
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The Octez client can be used to send an external message into the
rollup inbox. Assuming that ``${EMESSAGE}`` is the hexadecimal
representation of the message payload, one can do:

.. code:: sh

    octez-client" -d "${OCLIENT_DIR}" -p PtMumbai \
     send smart rollup message "hex:[ \"${EMESSAGE}\" ]" \
     from "${OPERATOR_ADDR}"

to inject such an external message.
So let us focus now on producing a viable contents for ``${EMESSAGE}``.

The kernel used previously in our running example is a simple "echo"
kernel that copies its input as a new message to its outbox.
Therefore, the input must be a valid binary encoding of an outbox
message to make this work. Specifically, assuming that we have
originated a Layer 1 smart contract as follows:

.. code:: sh

   octez-client -d "${OCLIENT_DIR}" -p PtMumbai \
     originate contract go transferring 1 from "${OPERATOR_ADDR}" \
     running 'parameter string; storage string; code {CAR; NIL operation; PAIR};' \
     --init '""' --burn-cap 0.4

and that this contract is identified by a address ``${CONTRACT}``, then one
can encode an outbox transaction using the Octez rollup client as follows:

.. code:: sh

    MESSAGE='[ { \
      "destination" : "${CONTRACT}", \
      "parameters" : "\"Hello world\"", \
      "entrypoint" : "%default" } ]'


    EMESSAGE=$(octez-smart-rollup-client-PtMumbai encode outbox message "${MESSAGE}")


.. _triggering_execution_outbox_message:
.. _triggering_execution_outbox_message_mumbai:

Triggering the execution of an outbox message
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Once an outbox message has been pushed to the outbox by the kernel at
some level ``${L}``, the user needs to wait for the commitment that
includes this level to be cemented. On Dailynet, the cementation
process of a non-disputed commitment is 40 blocks long while on
Mainnet, it is 2 weeks long.

When the commitment is cemented, one can observe that the outbox is
populated as follows:

.. code:: sh

   octez-smart-rollup-client-PtMumbai rpc get \
     /global/block/cemented/outbox/${L}/messages

Here is the output for this command:

.. code::

   [ { "outbox_level": ${L}, "message_index": "0",
    "message":
      { "transactions":
          [ { "parameters": { "string": "Hello world" },
              "destination": "${CONTRACT}",
              "entrypoint": "%default" } ] } } ]


At this point, the actual execution of a given outbox message can be
triggered. This requires precomputing a proof that this outbox message
is indeed in the outbox. In the case of our running example, this
proof is retrieved as follows:

.. code:: sh

   PROOF=$(octez-smart-rollup-client-PtMumbai get proof for message 0 \
     of outbox at level "${L}" \
     transferring "${MESSAGE}")

Finally, the execution of the outbox message is done as follows:

.. code:: sh

   "${TEZOS_PATH}/octez-client" -d "${OCLIENT_DIR}" -p PtMumbai \
           execute outbox message of smart rollup "${SOR_ADDR}" \
           from "${OPERATOR_ADDR}" for commitment hash "${LCC}" \
           and output proof "${PROOF}"

where ``${LCC}`` is the hash of the latest cemented commitment.
Notice that anyone can trigger the execution of an outbox message
(not only an operator as in this example).

One can check in the receipt that the contract has indeed been called
with the parameter ``"Hello world"`` through an internal
operation. More complex parameters, typically containing assets
represented as tickets, can be used as long as they match the type of
the entrypoint of the destination smart contract.

.. _sending_internal_inbox_message:
.. _sending_internal_inbox_message_mumbai:

Sending an internal inbox message
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A smart contract can push an internal message in the rollup inbox
using the Michelson ``TRANSFER_TOKENS`` instruction targetting a
specific rollup address. The parameter of this transfer must be a
value of the Michelson type declared at the origination of this
rollup.

Remember that our running example rollup has been originated with:

.. code:: sh

    octez-client originate smart rollup from "${OPERATOR_ADDR}" \
      of kind wasm_2_0_0 \
      of type bytes \
      booting with "${KERNEL}" \
      -burn-cap 999

The fragment ``of type bytes`` of this command declares that the
rollup is expecting values of type ``bytes``. (Notice any Michelson type
could have been used instead. To transfer tickets to a rollup, this
type must mention tickets.)

Here is an example of a Michelson script that sends an internal
message to the rollup of our running example. The payload of the
internal message is the value passed as parameter of type ``bytes``
to the rollup.

::

        parameter bytes;
        storage unit;
        code
          {
            UNPAIR;
            PUSH address "${SOR_ADDR}";
            CONTRACT bytes;
            IF_NONE { PUSH string "Invalid address"; FAILWITH } {};
            PUSH mutez 0;
            DIG 2;
            TRANSFER_TOKENS;
            NIL operation;
            SWAP;
            CONS;
            PAIR;
          }

.. _populating_the_reveal_channel:
.. _populating_the_reveal_channel_mumbai:

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

.. _configure_fast_exec:
.. _configure_fast_exec_mumbai:

Configure WebAssembly fast execution
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When the rollup node advances its internal rollup state under normal
operation, it does so in a mode called "Fast Execution".

This mode uses Wasmer when running WebAssembly code at the moment
which allows you to configure the compiler it will use to deal with
the WebAssembly code. It can be done using the
``OCTEZ_WASMER_COMPILER`` environment variable which will be picked
up by the smart rollup node.

The choice of compiler primarily affects the performance of the
WebAssembly execution. Some compilers offer additional security
guarantees which might be attractive to you.

There are these options:

.. list-table:: Wasmer compiler options
   :widths: 25 25 50
   :header-rows: 1

   * - Compiler
     - ``OCTEZ_WASMER_COMPILER`` value
     - Description
   * - Singlepass
     - ``singlepass``
     - `When to use Singlepass <https://github.com/wasmerio/wasmer/tree/master/lib/compiler-singlepass#when-to-use-singlepass>`_
   * - Cranelift
     - ``cranelift``
     - `When to use Cranelift <https://github.com/wasmerio/wasmer/tree/master/lib/compiler-cranelift#when-to-use-cranelift>`_

Developing WASM Kernels
-----------------------

A rollup is primarily characterized by the semantics it gives to the
input messages it processes. This semantics is provided at origination
time as a WASM program (in the case of the ``wasm_2_0_0`` kind) called
a *kernel*. More concretely, the kernel is a WASM module encoded in the
binary format defined by the WASM standard.

Except for necessary restrictions to ensure determinism (a key requirement for any web3 technology), we support the full WASM language.
More precisely, determinism is ensured by the following restrictions:

#. Instructions and types related to floating-point arithmetic are not
   supported. This is because IEEE floats are not deterministic, as
   the standard includes undefined behavior operations.
#. The length of the call stack of the WASM kernel is restricted to 300.

Modulo the limitations above, a valid kernel is a WASM module that satisfies the following
constraints:

#. It exports a function ``kernel_run`` that takes no argument and
   returns nothing.
#. It declares and exports exactly one memory.
#. It only imports the host functions exported by the (virtual)
   module ``smart_rollup_core``.

For instance, the mandatory example of a ``hello, world!`` kernel is
the following WASM program in text format.

.. code::

    (module
      (import "smart_rollup_core" "write_debug"
         (func $write_debug (param i32 i32) (result i32)))
      (memory 1)
      (export "mem" (memory 0))
      (data (i32.const 100) "hello, world!")
      (func (export "kernel_run")
        (local $hello_address i32)
        (local $hello_length i32)
        (local.set $hello_address (i32.const 100))
        (local.set $hello_length (i32.const 13))
        (drop (call $write_debug (local.get $hello_address)
                                 (local.get $hello_length)))))

This program can be compiled to the WASM binary format with
general-purpose tool like
`WABT <https://github.com/WebAssembly/wabt>`_.

::

   wat2wasm hello.wat -o hello.wasm

The contents of the resulting ``hello.wasm`` file is a valid WASM
kernel, though its relevance as a decentralized application is
debatable.

One of the benefits of choosing WASM as the programming language for
smart rollups is that WASM has gradually become a ubiquitous
compilation target over the years. Its popularity has grown to the point where mainstream,
industrial languages like Go or Rust now natively compile to
WASM. Thus, ``cargo`` —the official Rust package manager— provides an
official target to compile Rust to ``.wasm`` binary files, which are
valid WASM kernels. This means that, for this particular example, one
can build a WASM kernel while enjoying the strengths and convenience
of the Rust language and the Rust ecosystem.

The rest of the section proceeds as follows.

#. First, we explain the execution environment of a WASM kernel: when
   it is parsed, executed, etc.
#. Then, we explain in more details the API at the disposal of WASM
   kernel developers.
#. Finally, we demonstrate how Rust in particular can be used to
   implement a WASM kernel.

Though Rust has become the primary language whose WASM backend has
been tested in the context of smart rollups, the WASM VM has not been
modified in any way to favor this language. We fully expect that other
mainstream languages such as Go are also good candidates for
implementing WASM kernels.

Execution Environment
^^^^^^^^^^^^^^^^^^^^^
In a nutshell, the life cycle of a smart rollup is a never-ending
loop of fetching inputs from the Layer 1, and executing the
``kernel_run`` function exposed by the WASM kernel.

State
"""""

The smart rollup carries two states:

#. A transient state, that is reset after each call to the
   ``kernel_run`` function and is akin to RAM.
#. A persistent state, that is preserved across ``kernel_run`` calls.
   The persistent state consists in an *inbox* that is regularly
   populated with the inputs coming from the Layer 1, the *outbox*
   which the kernel can populate with contract calls targeting smart
   contracts in the Layer 1, and a durable storage which is akin to a
   file system.

The durable storage is a persistent tree, whose contents is addressed
by path-like keys. A path in the storage may contain: a value (also
called file) consisting of a sequence of raw bytes, and/or any number
of subtrees (also called directories), that is, the paths in the
storage prefixed by the current path. Thus, unlike most file systems,
a path in the durable storage may be at the same time a file and a
directory (a set of sub-paths).

The WASM kernel can write and read the raw bytes stored under a given
path (the file), but can also interact (delete, copy, move, etc.) with
subtrees (directories).

The values and subtrees under the key ``/readonly`` are not writable
by a kernel, but can be used by the PVM to give information to the
kernel.

Control Flow
""""""""""""

When a new block is published on Tezos, the inbox exposed to the smart
rollup is populated with all the inputs published on Tezos in this
block. It is important to keep in mind that all the smart rollups
which are originated on Tezos share the same inbox. As a consequence,
a WASM kernel has to filter the inputs that are relevant for its
purpose from the ones it does not need to process.

Once the inbox has been populated with the inputs of the Tezos block,
the ``kernel_run`` function is called, from a clean “transient”
state. More precisely, the WASM kernel is re-initialized,
then ``kernel_run`` is called.

By default, the WASM kernel yields when ``kernel_run`` returns. In
this case, the WASM kernel execution is put on hold while the inputs of
the next inbox are being loaded. The inputs that were not consumed by
``kernel_run`` are dropped. ``kernel_run`` can prevent the WASM
kernel from yielding by writing arbitrary data under the path
``/kernel/env/reboot`` in its durable storage. In such a case (known
as reboot), ``kernel_run`` is called again, without dropping unread
inputs. The value at ``/kernel/env/reboot`` is removed between each call of ``kernel_run``,
and the ``kernel_run`` function can postpone yielding at most 1,000
reboots for each Tezos level.

A call to ``kernel_run`` cannot take an arbitrary amount of time to
complete, because diverging computations are not compatible with the
optimistic rollup infrastructure of Tezos.
To dodge the halting
problem, the reference interpreter of WASM (used during the refutation game)
enforces a bound on the number of ticks used in a call to
``kernel_run``. Once the maximum number of ticks is reached, the
execution of ``kernel_run`` is trapped (*i.e.*, interrupted with an
error).
In turn, the fast execution engine does not enforce this time limit. Hence,
it is the responsibility of the kernel developer to implement a ``kernel_run`` which does not exceed its tick budget.


The current bound is set to 11,000,000,000 ticks.
``octez-smart-rollup-wasm-debugger`` is probably the best tool available to
verify the ``kernel_run`` function does not take more ticks than authorized.

The direct consequence of this setup is that it might be necessary for
a WASM kernel to span a long computation across several calls to
``kernel_run``, and therefore to serialize any data it needs in the
durable storage to avoid loosing them.

Finally, the kernel can verify if the previous ``kernel_run``
invocation was trapped by verifying if some data are stored under the
path ``/kernel/env/stuck``.

Host Functions
^^^^^^^^^^^^^^

At its core, the WASM machine defined in the WASM standard is just a
very evolved arithmetic machine. It needs to be enriched with
so-called host functions in order to be used for greater purposes. The
host functions provide an API to the WASM program to interact with an
“outer world”.

As for smart rollups, the host functions exposed to a WASM kernel
allow it to interact with the components of persistent state:

``read_input``
  Loads the oldest input still present in the inbox of the smart
  rollup in the transient memory of the WASM kernel. This means that
  the input is lost at the next invocation of ``kernel_run`` if it is
  not written in the durable storage.

``write_output``
  Writes an in-memory buffer to the outbox of the smart rollup. If the
  content of the buffer follows the expected encoding, it can be
  interpreted in the Layer 1 as a smart contract call, once a
  commitment acknowledging the call to this host function is cemented.

``write_debug``
  Can be used by the WASM kernel to log
  events which can potentially be interpreted by an instrumented
  rollup node.

``store_has``
  Returns the kind of data (if any) stored in the durable storage under a given
  path: a directory, a file, neither or both.

``store_delete``
  Cuts the subtree under a given path out of the durable storage.

``store_copy``
  Copies the subtree under a given path to another key.

``store_move``
  Behaves as ``store_copy``, but also cuts the original subtree out of
  the tree.

``store_read``
  Loads at most 4,096 bytes from a file of the durable storage to a buffer
  in the memory of the WASM kernel.

``store_write``
  Writes at most 4,096 bytes from a buffer in the memory of the WASM
  kernel to a file of the durable storage, increasing its size if
  necessary. Note that files in the durable storage cannot exceed
  :math:`2^{31} - 1` bytes (i.e. 2GB - 1).

``store_value_size``
  Returns the size (in bytes) of a file under a given key in the
  durable storage.

``store_list_size``
  Returns the number of child objects (either directories or files)
  under a given key.

``store_get_nth_key``
  Loads in memory at a given location the durable storage key to
  access the nth child under a given key, and returns the number of
  bytes loaded in memory. Children include both subtrees and the
  value, if any. Note that the result is not stable w.r.t. key
  additions and removals. This function can be used by the WASM kernel
  to iterate over the contents under the input key. This function will
  return :math:`0` for the index of the value (if any) under the input
  key.

``reveal_preimage``
  Loads in memory the preimage of a hash. The size of the hash in
  bytes must be specified as an input to the function.

``reveal_metadata``
  Loads in memory the address of the smart rollup (20 bytes), and the
  Tezos level of its origination (4 bytes).

These host functions use a "C-like" API. In particular, most of them
return a signed 32bit integer, where negative values are reserved for
conveying errors, as shown in the next table.

======= =======================================================================================================
 Code    Description
------- -------------------------------------------------------------------------------------------------------
  -1     Input is too large to be a valid key of the durable storage
  -2     Input cannot be parsed as a valid key of the durable storage
  -3     There is no file under the requested key
  -4     The host functions tried to read or write an invalid section (determined by an offset and a length) of the value stored under a given key
  -5     Cannot write a value beyond the 2GB size limit
  -6     Invalid memory access (segmentation fault)
  -7     Tried to read from the inbox or write to the outbox more than 4,096 bytes
  -8     Unknown error due to an invalid access
  -9     Attempt to modify a readonly value
  -10    Key has no tree in the storage
  -11    Outbox is full, no new message can be appended
  -12    No subkey at provided index. The index is out of bounds
======= =======================================================================================================

Implementing a WASM Kernel in Rust
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Though WASM is a good fit for efficiently executing computation-intensive, arbitrary
programs, it is a low-level, stack-based, memory unsafe language.
Fortunately, it was designed to be a compilation target, not a
language in which developers would directly write their programs.

Rust has several advantages that makes it a good candidate for writing
the kernel of a smart rollup. Not only does the Rust compiler treat
WASM as a first class citizen when it comes to compilation targets,
but its approach to memory safety eliminates large classes of bugs and
vulnerabilities that arbitrary WASM programs may suffer from.

Setting-up Rust
"""""""""""""""

`rustup <https://rustup.rs>`_ is the standard way to get Rust. Once
``rustup`` is installed, enabling WASM as a compilation target is as
simple as running the following command.

::

   rustup target add wasm32-unknown-unknown

Rust also proposes the ``wasm64-unknown-unknown`` compilation
target. This target is **not** compatible with Tezos smart rollups,
which only provides a 32bit address space.

.. note::

   This document is not a tutorial about Rust, and familiarity with
   the language and its ecosystem (*e.g.*, how Rust crates are
   structured in particular) is assumed.

The simplest kernel one can implement in Rust (the one that returns
directly after being called, without doing anything particular) is the
following Rust file (by convention named ``lib.rs`` in Rust).

.. code:: rust

   #[no_mangle]
   pub extern "C" fn kernel_run() {
   }

This code can be easily computed with ``cargo`` with the following
``Cargo.toml``.

::

   [package]
   name = 'noop'
   version = '0.1.0'
   edition = '2021'

   [lib]
   crate-type = ["cdylib"]

The key line to spot is the ``crate-type`` definition to
``cdylib``. As a side note, when writing a library that will
eventually be consumed by a Kernel WASM crate, this line must be
modified to

.. code:: toml

   crate-type = ["cdylib", "rlib"]

Compiling our “noop” kernel is done by calling ``cargo`` with the
correct argument.

::

   cargo build --target wasm32-unknown-unknown

It is also possible to use the ``--release`` CLI flag to tell
``cargo`` to optimize the kernel.

To make the use of the ``target`` optional, it is possible to create
a ``.cargo/config.toml`` file, containing the following line.

::

   [build]
   target = "wasm32-unknown-unknown"

   [rust]
   lld = true%

The resulting project looks as follows.

::

   .
   ├── .cargo
   │   └── config.toml
   ├── Cargo.toml
   └── src
       └── lib.rs

and the kernel can be found in the ``target/`` directory, *e.g.*,
``./target/wasm32-unknown-unknown/release/noop.wasm``.

By default, Rust binaries (including WASM binaries) contain a lot of
debugging information and possibly unused code that we do not want to
deploy in our rollup. For instance, our “noop” kernel weighs
1.7MBytes. We can use `wasm-strip
<https://github.com/WebAssembly/wabt>`__ to reduce the size of the
kernel (down to 115 bytes in our case).

Host Functions in Rust
""""""""""""""""""""""

The host functions exported by the WASM runtime to Rust programs
are exposed by the following API. The ``link`` pragma is used to specify the
module that exports them (in our case, ``smart_rollup_core``).

.. code:: rust

   #[repr(C)]
   pub struct ReadInputMessageInfo {
       pub level: i32,
       pub id: i32,
   }

   #[link(wasm_import_module = "smart_rollup_core")]
   extern "C" {
       /// Returns the number of bytes written to `dst`, or an error code.
       pub fn read_input(
           message_info: *mut ReadInputMessageInfo,
           dst: *mut u8,
           max_bytes: usize,
       ) -> i32;

       /// Returns 0 in case of success, or an error code.
       pub fn write_output(src: *const u8, num_bytes: usize) -> i32;

       /// Does nothing. Does not check the correctness of its argument.
       pub fn write_debug(src: *const u8, num_bytes: usize);

       /// Returns
       /// - 0 the key is missing
       /// - 1 only a file is stored under the path
       /// - 2 only directories under the path
       /// - 3 both a file and directories
       pub fn store_has(path: *const u8, path_len: usize) -> i32;

       /// Returns 0 in case of success, or an error code
       pub fn store_delete(path: *const u8, path_len: usize) -> i32;

       /// Returns the number of children (file and directories) under a
       /// given key.
       pub fn store_list_size(path: *const u8, path_len: usize) -> i64;

       /// Returns the size of the key loaded in memory at `dst`, or an
       /// error code.
       pub fn store_get_nth_key(
           path: *const u8,
           path_len: usize,
           index: i64,
           dst: *mut u8,
           max_size: usize,
       ) -> i32;

       /// Returns 0 in case of success, or an error code.
       pub fn store_copy(
           src_path: *const u8,
           scr_path_len: usize,
           dst_path: *const u8,
           dst_path_len: usize,
       ) -> i32;

       /// Returns 0 in case of success, or an error code.
       pub fn store_move(
           src_path: *const u8,
           scr_path_len: usize,
           dst_path: *const u8,
           dst_path_len: usize,
       ) -> i32;

       /// Returns the number of bytes written to the durable storage
       /// (should be equal to `num_bytes`, or an error code.
       pub fn store_read(
           path: *const u8,
           path_len: usize,
           offset: usize,
           dst: *mut u8,
           num_bytes: usize,
       ) -> i32;

       /// Returns 0 in case of success, or an error code.
       pub fn store_write(
           path: *const u8,
           path_len: usize,
           offset: usize,
           src: *const u8,
           num_bytes: usize,
       ) -> i32;

       /// Returns the number of bytes written at `dst`, or an error
       /// code.
       pub fn reveal_metadata(
           dst: *mut u8,
           max_bytes: usize,
       ) -> i32;

       /// Returns the number of bytes written at `dst`, or an error
       /// code.
       pub fn reveal_preimage(
           hash_addr: *const u8,
           hash_size: u8,
           dst: *mut u8,
           max_bytes: usize,
       ) -> i32;
   }

These functions are marked as ``unsafe`` for Rust. It is possible to
provide a safe API on top of them. For instance, the ``read_input`` host
function can be used to declare a safe function which allocates a
fresh Rust Vector to receive the input.

.. code:: rust

   // Assuming the host functions are defined in a module `host`.

   pub const MAX_MESSAGE_SIZE: u32 = 4096u32;

   pub struct Input {
       pub level: u32,
       pub id: u32,
       pub payload: Vec<u8>,
   }

   pub fn next_input() -> Option<Input> {
       let mut payload = Vec::with_capacity(MAX_MESSAGE_SIZE as usize);

       // Placeholder values
       let mut message_info = ReadInputMessageInfo { level: 0, id: 0 };

       let size = unsafe {
            host::read_input(
               &mut message_info,
               payload.as_mut_ptr(),
               MAX_MESSAGE_SIZE,
           )
       };

       if 0 < payload.len() {
           unsafe { payload.set_len(size as usize) };
           Some(Input {
               level: message_info.level as u32,
               id: message_info.id as u32,
               payload,
           })
       } else {
           None
       }
   }

Coupling ``Vec::with_capacity`` along with the ``set_len`` unsafe
function is a good approach to avoid initializing the 4,096 bytes of
memory every time you want to load data of arbitrary size into the
WASM memory.

Testing your Kernel
"""""""""""""""""""

.. note::

   ``octez-smart-rollup-wasm-debugger`` is available in the Octez
   distribution starting with :doc:`/releases/version-16`.

Testing a kernel without having to start a rollup node on a test
network is very convenient. We provide a debugger as a means to
evaluate the WASM PVM without relying on any node and network:
``octez-smart-rollup-wasm-debugger``.

.. code:: sh

  octez-smart-rollup-wasm-debugger ${WASM_FILE} --inputs ${JSON_INPUTS} --rollup ${ROLLUP_ADDRESS}

``octez-smart-rollup-wasm-debugger`` takes as its argument the WASM kernel to be debugged, either a a ``.wasm`` file (the binary
representation of WebAssembly modules) or as a ``.wast`` file (its textual
representation), and actually parses and typechecks the kernel before
giving it to the PVM.

Beside the kernel file, the debugger can optionally take an input file containing inboxes and a
rollup address. The expected contents of the inboxes is a JSON value,
with the following schema:

.. code:: javascript

  [
    [ { "payload" : <Michelson data>,
        "sender" : <Contract hash of the originated contract for the rollup, optional>,
        "source" : <Implicit account sending the message, optional>
        "destination" : <Smart rollup address> }
      ..
      // or
      { "external" : <hexadecimal payload> }
      ..
    ]
  ]

The contents of the input file is a JSON array of arrays of inputs,
which encodes a sequence of inboxes, where an inbox is a set of
messages. These inboxes are read in the same order as they appear in
the JSON file. For example, here is a valid input file that defines
two inboxes: the first array encodes an inbox containing only an
external message, while the second array encodes an inbox containing
two messages:

.. code:: javascript

  [
    [
      {
        "external":
        "0000000023030b01d1a37c088a1221b636bb5fccb35e05181038ba7c000000000764656661756c74"
      }
    ],
    [
      {
        "payload" : "0",
        "sender" : "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq",
        "source" : "tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w",
        "destination" : "sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf"
      },
      { "payload" : "Pair Unit False" }
    ]
  ]

Note that the ``sender``, ``source`` and ``destination`` fields are optional
and will be given default values by the debugger, respectively
``KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT``,
``tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU`` and
``sr163Lv22CdE8QagCwf48PWDTquk6isQwv57``. If no input file is given, the
inbox will be assumed empty. If the option ``--rollup`` is given, it
replaces the default value for the rollup address.

``octez-smart-rollup-wasm-debugger`` is a debugger, as such it waits for user
inputs to continue its execution. Its initial state is exactly the same as right
after its origination. Its current state can be inspected with the command
``show status``:

.. code::

  > show status
  Status: Waiting for inputs
  Internal state: Collect

When started, the kernel is in collection mode internally. This means that it is
not executing any WASM code, and is waiting for inputs in order to
proceed. The command
``load inputs`` will load the first inbox from the file given with the
option ``--inputs``, putting ``Start_of_level`` and ``Info_per_level`` before
these inputs and ``End_of_level`` after the inputs.

.. code::

  > load inputs
  Loaded 3 inputs at level 0

  > show status
  Status: Evaluating
  Internal state: Snapshot

At this point, the internal input buffer can be inspected with the
command ``show inbox``.

.. code::

  > show inbox
  Inbox has 3 messages:
  { raw_level: 0;
    counter: 0
    payload: Start_of_level }
  { raw_level: 0;
    counter: 1
    payload: 0000000023030b01d1a37c088a1221b636bb5fccb35e05181038ba7c000000000764656661756c74 }
  { raw_level: 0;
    counter: 2
    payload: End_of_level }

The first input of an inbox at the beginning of a level is
``Start_of_level``, and is represented by the message ``\000\001`` on
the kernel side. We can now start a ``kernel_run`` evaluation:

.. code::

  > step kernel_run
  Evaluation took 11000000000 ticks so far
  Status: Waiting for inputs
  Internal state: Collect


The memory of the interpreter is flushed between two ``kernel_run``
calls (at the ``Snapshot`` and ``Collect`` internal states), however the
durable storage can be used as a persistent memory. Let's assume this
kernel wrote data at key ``/store/key``:

.. code::

  > show key /store/key
  `<hexadecimal value of the key>`

Since the representation of values is decided by the kernel, the debugger can
only return its raw value. Please note that the command ``show keys <path>``
will return the keys under the given path. This can help navigate in the durable
storage.

.. code::

   > show keys /store
   /key
   /another_key
   ...

It is also possible to inspect the memory by stopping the PVM before its
snapshot internal state, with ``step result``, and inspect the memory at pointer
``n`` and length ``l``, and finally evaluate until the next ``kernel_run``:

.. code::

  > step result
  Evaluation took 2500 ticks so far
  Status: Evaluating
  Internal state: Evaluation succeeded

  > show memory at p for l bytes
  `<hexadecimal value>`

  > step kernel_run
  Evaluation took 7500 ticks so far
  Status: Evaluating
  Internal state: Snapshot

Once again, note that values from the memory are output as is,
since the representation is internal to WASM.

Finally, it is possible to evaluate the whole inbox with ``step inbox``. It will
take care of the possible reboots asked by the kernel (through the usage of the
``/kernel/env/reboot_flag`` flag) and stop at the next collection phase.

.. code::

  > step inbox
  Evaluation took 44000000000 ticks
  Status: Waiting for inputs
  Internal state: Collect

It is also possible to show the outbox for any given level (``show
outbox at level 0``)

.. code::

  > show outbox at level 0
  Outbox has N messages:
  { unparsed_parameters: ..;
    destination: ..;
    entrypoint: ..; }
  ..

The reveal channel described previously is available in the
debugger, either automatically or through specific commands. The
debugger can fill automatically preimages from files in a specific
directory on the disk, by default in the ``preimage`` subdirectory of the
working directory. It can be configured with the option
``--preimage-dir <directory>``. In case there is no corresponding file
found for the requested preimage, the debugger will ask for the
hexadecimal value of the preimage:

.. code::

  > step inbox
  Preimage for hash 0000[..] not found.
  > 48656c6c6f207468657265210a
  Hello there!
  ...

Metadata are automatically filled with level ``0`` as origination level
and the configured smart rollup address (or the default one).

Note that when stepping tick by tick (using the ``step tick`` command), it is
possible to end up in a situation were the evaluation stops on ``Waiting for
reveal``. If the expected value is a metadata, the command ``reveal metadata``
will give the default metadata to the kernel. If the value expected is the
preimage of a given hash, there are two possible solutions:

* ``reveal preimage`` to read the value from the disk. In that case, the
  debugger will look for a file of the same name as the expected hash in the
  ``preimage`` subdirectory.
* ``reveal preimage of <hex encoded value>`` can be used to feed a custom
  preimage hash.

Glossary
--------

#. **PVM**: A Proof-generating Virtual Machine is a reference
   implementation for a device on top of which a smart rollup can be
   executed. This reference implementation is part of the Tezos
   protocol and is the unique source of truth regarding the semantics
   of rollups. The PVM is able to produce proofs enforcing this truth.
   This ability is used during the final step of refutation games.

#. **Inbox**: A sequence of messages from the Layer 1 to smart rollups.
   The contents of the inbox is determined by the consensus of the
   Tezos protocol.

#. **Outbox**: A sequence of messages from a smart rollup to the Layer 1.
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
   this commitment period to be challenged.

#. **Staker**: An implicit account that has made a deposit on a
   commitment.

#. **Refutation game**: A process by which the Tezos protocol solves
   a conflict between two stakers.
