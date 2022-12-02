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
   smart-contracts or the protocol itself. More information about
   these two kinds of messages can be found below.

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
using the Octez client (see command ``send smart rollup message
<messages> from <src>``) ; second, she can use the batcher
of a smart rollup node. More details can be found in :ref:`sending_external_inbox_message_alpha`.

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
   :ref:`populating_the_reveal_channel_alpha`.

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
operation (see :ref:`triggering_execution_outbox_message_alpha`), typically
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

     xxd -ps -c 0 <kernel.wasm>

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

   octez-smart-rollup-node-alpha --base-dir "${OCLIENT_DIR}" \
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

   octez-smart-rollup-node-alpha" -d "${OCLIENT_DIR}" run --data-dir ${ROLLUP_NODE_DIR}

The log should show that the rollup node follows the layer-1 chain and
processes the inbox of each level.

.. _sending_external_inbox_message_alpha:

Sending an external inbox message
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


.. _sending_internal_inbox_message_alpha:

Sending an internal inbox message
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _triggering_execution_outbox_message_alpha:

Triggering the execution of an outbox message
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _populating_the_reveal_channel_alpha:

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

.. _configure_fast_exec_alpha:

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
a *kernel*. More precisely, a *kernel* is a WASM module encoded in the
binary format defined by the WASM standard.

Though compliance with the WASM standard was a key requirement for
smart rollups, there is a caveat to this claim. This is due to the
particular constraints web3 developers are very familiar with, namely
programs must be fully deterministic. As a consequence,

#. Instructions and types related to floating-point arithmetic are not
   supported. This is because IEEE floats are not deterministic, as
   the standard includes undefined behaviors operations.
#. The call stack of the WASM kernel is restricted to 300.

A valid kernel is a WASM module that satisfies the following
constraints:

#. It exports a function ``kernel_run`` that takes no argument and
   returns nothing.
#. It declares and exports exactly one memory.
#. It only imports the host functions, exported by the (virtual)
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

One of the benefit of choosing WASM as the programming language for
smart rollups is that WASM has gradually become a ubiquitous
compilation target over the years. To the point where mainstream,
industrial languages like Go or Rust now natively compile to
WASM. Thus, ``cargo`` —the official Rust package manager— provides an
official target to compile Rust to ``.wasm`` binary files that are
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
modified in anyway to favor this language. We fully expect that other
mainstream languages like Go for instance are also good candidate to
implement WASM kernels.

Execution Environment
^^^^^^^^^^^^^^^^^^^^^
In a nutshell, the life cycle of a smart rollup is a never-ending
interleaving of fetching inputs from the layer-1, and executing the
``kernel_run`` function exposed by the WASM kernel.

State
"""""

The smart rollup carries two states:

#. A transient state, that is reset after each call to the
   ``kernel_run`` function and is akin to RAM.
#. A persistent state, that is preserved across ``kernel_run`` calls.
   The persistent state consists in an *inbox* that is regularly
   populated with the inputs coming from the layer-1, the *outbox*
   which the kernel can populate with contract calls targeting smart
   contracts in the layer-1, and a durable storage which is akin to a
   file system.

The durable storage is a persistent tree, whose contents is addressed
by path-like keys. The WASM kernel can write and read raw bytes stored
under a given path (files), but can also interact (delete, copy, move,
etc.) with subtrees (directories). The value and subtrees at key
``/readonly`` are not writable by a kernel, but can be used by the PVM
to give information to the kernel.

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
state. More precisely, the WASM kernel is parsed, linked, initialized,
then ``kernel_run`` is called.

By default, the WASM kernel yields when ``kernel_run`` returns. In
this case, the WASM kernel execution is put on hold while the input of
the next inbox are being loaded. The inputs that were not consumed by
``kernel_run`` are dropped. ``kernel_run`` can prevent the WASM
kernel from yielding by writing arbitrary data under the path
``/kernel/env/reboot`` in its durable storage. In such a case (known
as reboot), ``kernel_run`` is called again, without dropping unread
inputs. This value is removed between each call of ``kernel_run``,
and the ``kernel_run`` function can postpone yielding at most 1,000
reboots for each Tezos level.

A call to ``kernel_run`` cannot take an arbitrary amount of time to
complete, because diverging computations are not compatible with the
optimistic rollup infrastructure of Tezos. To dodge the halting
problem, the reference interpreter of WASM used during the rejection
enforces a bound on the number of ticks used in a call to
``kernel_run``. Once the maximum number of ticks is reached, the
execution of ``kernel_run`` is trapped (*i.e.*, interrupted with an
error).

The current bound is set to 11,000,000,000 ticks. ``octez-wasm-repl``
is probably the best tool available to verify the ``kernel_run``
function does not take more ticks than authorized.

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
host functions provides an API to the WASM program to interact with an
“outer world.”  In a browser, this API typically allows the WASM
program to interact with the `DOM
<https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model>`_
of the webpage.

As for smart rollups, the host functions exposed to a WASM kernel
allows it to interact with the components of persistent state.

``read_input``
  Loads the oldest input still present in the inbox of the smart
  rollup in the transient memory of the WASM kernel. This means that
  the input is lost at the next invocation of ``kernel_run`` if it is
  not written in the durable storage.

``write_output``
  Writes an in-memory buffer to the outbox of the smart rollup. If the
  content of the buffer follows the expected encoding, it can be
  interpreted in the layer-1 as a smart contract call, once a
  commitment acknowledging the call to this host function is cemented.

``write_debug``
  Is considered as a no-op, but can be used by the WASM kernel to log
  events which can potentially be interpreted by an instrumented
  rollup node.

``store_has``
  Reports the kind of data stored under a given path in the durable
  storage: a directory, a file, neither or both.

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
  2,147,483,647 bytes (:math:`2^31 - 1`, around 2GB).

``store_value_size``
  Returns the size (in bytes) of a file under a given key in the
  durable storage.

``store_list_size``
  Returns the number of child objects (either directories or files)
  under a given key.

``store_get_nth_key``
  Loads in memory at a given location the durable storage key to
  access the nth child under a given key. Note that the result is not
  stable w.r.t. key additions and removals. Returns the number of
  bytes loaded in memory. If :math:`0` is loaded, it means there
  exists a value under the key given as argument (which can be
  manipulated with ``store_read`` and ``store_write``).

``reveal_preimage``
  Loads in memory the preimage of a 32-byte Blake2B hash.

``reveal_metadata``
  Loads in memory the address of the smart rollup (20 bytes), and the
  Tezos level of its origination (4 bytes).

These host functions use a "C-like" API. In particular, most of them
return a signed 32bit integer, where negative values are reserved for
conveying errors.

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
======= =======================================================================================================

Implementing a WASM Kernel in Rust
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Though WASM is a good fit for writing computation intensive, arbitrary
programs, it remains a low-level, stack-based, memory unsafe language.
Fortunately, it was designed to be a compilation target, not a
language whose program would be written directly by developers.

Rust has several advantages that makes it a good candidate to write
the kernel of a smart rollup. Not only does the Rust compiler treat
WASM as a first class citizen when it comes to compilation targets,
but its approach to memory safety eliminates large classes of bugs and
vulnerabilities that arbitrary WASM programs may suffer from.

Setting-up Rust
"""""""""""""""

```rustup`` <https://rustup.rs>`_ is the standard to get Rust. Once
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
following.

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

Compiling our ``noop`` kernel is done by calling ``cargo`` with the
correct argument.

::

   cargo build --target wasm32-unknown-unknown

To make the use of the ``target`` optional, it is possible to create
a ``.cargo/config.toml`` file, containing the following line.

::

   [build]
   target = "wasm32-unknown-unknown"

   [rust]
   lld = true%

Host Functions in Rust
""""""""""""""""""""""

Exposing host functions exported by the WASM runtime to a Rust program
is actually really straightforward. The ``link`` pragma is used to specify the
module that exports them (in our case, ``smart_rollup_core``).

.. code:: rust

   #[link(wasm_import_module = "smart_rollup_core")]
   extern "C" {
       /// Returns the number of bytes written to `dst`, or an error code.
       pub fn read_input(
           level: *mut i32,
           id: *mut i32,
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
           dst: *mut u8,
           max_bytes: usize,
       ) -> i32;
   }

These functions are marked as ``unsafe`` for Rust. It is possible to
provide safe API on top of them. For instance, the ``read_input`` host
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
       let mut level = 0i32;
       let mut id = 0i32;

       let size = unsafe {
            host::read_input(
               &mut level,
               &mut id,
               payload.as_mut_ptr(),
               MAX_MESSAGE_SIZE,
           )
       };

       if 0 < payload.len() {
           unsafe { payload.set_len(size as usize) };
           Some(Input {
               level: level as u32,
               id: id as u32,
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

.. warning::

   The ``octez-wasm-repl`` tool that is described in this section is
   still under active development. A preliminary version can be found
   in `the Octez repository <https://gitlab.com/tezos/tezos>`_.

   To get ``octez-wasm-repl``, the easiest way is to build Octez from
   source. See the `usual instructions
   <https://tezos.gitlab.io/introduction/howtoget.html#setting-up-the-development-environment-from-scratch>`_.

   For now, ``octez-wasm-repl`` is **not** part of Octez, and is only
   provided for developers interested in testing Tezos smart rollup
   infrastructure before its release on mainnet.

Testing kernels can be useful during its development, without relying
on starting a rollup on a test network. We provide a
*read-eval-print-loop* (REPL) as a mean to evaluate the WASM PVM
without relying on any node and network: ``octez-wasm-repl``.

.. code:: sh

  octez-wasm-repl ${WASM_FILE} --inputs ${JSON_INPUTS} --rollup ${ROLLUP_ADDRESS}

``octez-wasm-repl`` can take either a `.wasm` file (the binary
representation of WebAssembly modules) or a `.wast` file (its textual
representation), and actually parses and typechecks the kernel before
giving it to the PVM. It can take a file containing inboxes and a
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

The contents of the input file is a JSON array of array of inputs,
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
        "destination" : "scr1HLXM32GacPNDrhHDLAssZG88eWqCUbyLF"
      },
      { "payload" : "Pair Unit False" }
    ]
  ]

Note that the `sender`, `source` and `destination` fields are optional
and will be given default values by the REPL (which are the *zero*
adresses). If no input file is given it will be assumed empty. If no
rollup address is given, it will use a default address which is the
*zero* address: ``scr1AFyXAWFS3c6S2D617o8NGRvazoMJPEw6s``.

``octez-wasm-repl`` is a REPL, as such it waits for user inputs to
continue its execution. Its initial state is exactly the same as right
after its origination. Its current state can be inspected with the
command ``show status``:

.. code::

  > show status
  Status: Waiting for inputs
  Internal state: Snapshot

At start, internally the kernel is in snapshot mode. It means it is
not executing any WASM code, and initially it is waiting for inputs to
proceed. It needs some inputs to continue its execution. The command
``load inputs`` will load the first inbox from the file given with the
option `--input`, putting `Start_of_level` (and `End_of_level`) before
(resp. after) these inputs.

.. code::

  > load inputs
  Loaded 3 inputs at level 0

  > show status
  Status: Evaluating
  Internal state: Decode

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
`Start_of_level`, and is represented by the message ``\000\001`` on
the kernel side. We can now start a `kernel_run` evaluation:

.. code::

  > step kernel_run
  Evaluation took 10000 ticks so far
  Status: Evaluating
  Internal state: Snapshot


The memory of the interpreter is flushed between two `kernel_run`
call (at the `Snapshot` internal state), however the ``durable
storage`` can be used as a persistent memory. Let's assume this kernel
wrote data at key `/store/key`:

.. code::

  > show key /store/key
  `<hexadecimal value of the key>`

Since the representation of values is decided by the kernel, the REPL can only
return its raw value. It is possible however to inspect the memory by stopping
the PVM before its snapshot internal state, with ``step result``, and
inspect the memory at pointer `n` and length `l`, and finaly evaluate until the
next `kernel_run`:

.. code::

  > step result
  Evaluation took 2500 ticks so far
  Status: Evaluating
  Internal state: Eval (Result)

  > show memory at p for l bytes
  `<hexadecimal value>`

  > step kernel_run
  Evaluation took 7500 ticks so far
  Status: Evaluating
  Internal state: Snapshot

Once again, note that values from the memory are outputted as is,
since the representation is internal to WASM.

Finally, it is possible to evaluate the whole inbox with ``step inbox``:

.. code::

  > step inbox
  Evaluation took 30000 ticks
  Status: Waiting for inputs
  Internal state: Snapshot

It is also possible to show the outbox for any given level (``show outbox at level 0``)

.. code::

  > show outbox
  Outbox has N messages:
  { unparsed_parameters: ..;
    destination: ..;
    entrypoint: ..; }
  ..


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
   this commitment period to be challenged.

#. **Staker**: An implicit account that has made a deposit on a
   commitment.

#. **Refutation game**: A process by which the Tezos protocol solves
   a conflict between two stakers.
