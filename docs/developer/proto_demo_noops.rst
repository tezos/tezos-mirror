======================================
How to write a Tezos protocol - Part 1
======================================

In this tutorial we will see how to write, compile, register, activate and use
an extremely simple protocol. By doing so, we will also start to explore
the interface between the protocol and the node (more specifically the
*shell* component of the node). In the second tutorial, we will
work our way to a protocol maintaining and updating some state.

In what follows, we suppose you have built Octez from sources,
as described in :doc:`../introduction/howtobuild`. You
should be already familiar with the :doc:`../user/sandbox`. All paths are
relative to the root of the Octez repository. All shell commands are to be
executed in sandbox mode.

That is, in your current terminal, do::

    eval `./src/bin_client/octez-init-sandboxed-client.sh 1`

And in another terminal, do::

    ./src/bin_node/octez-sandboxed-node.sh 1 --connections 0

and leave the sandboxed node running.

Protocol Registration
=====================

A node can contain several economics protocols (they are said to be
*registered*), but only one is *activated* at any given time.

We can query a node to know the registered protocols. Protocols are
identified by a b58check hash. On the ``master`` branch, hashes are
arbitrary values and do not depend on the actual code, but on production
branches, they are hashes of the source code of the protocol.

::

   $ octez-admin-client list protocols

   ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK
   ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT
   ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp
   ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im
   ...

The node in this example contains many protocols that were statically
linked (*embedded*) to the node at compile time. ``genesis`` is the
protocol activated at start-up. ``alpha`` is the main Tezos protocol.
``demo_noops`` is a simple protocol without operations (hence the name
no-ops) that we will use as our main example in this article.

Protocols can also be registered dynamically at run-time via an RPC
(a.k.a. *protocol injection*). As an example, let us inject the test
protocol :src:`src/bin_client/test/proto_test_injection` available as a
test case in the Tezos code base.

::

   $ octez-admin-client inject protocol src/bin_client/test/proto_test_injection

   Injected protocol Ps7nS9ZwNxMvSRQpaFSmu1uTjDvvJhvbdu8fGGDgzninPygNbZz successfully

Under the hood, the protocol is compiled and sent to the node using the
POST RPC ``/injection/protocol``.

We can check that the protocol was successfully injected

::

   $ octez-admin-client list protocols

   ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK
   ...
   Ps7nS9ZwNxMvSRQpaFSmu1uTjDvvJhvbdu8fGGDgzninPygNbZz
   ...

Lastly the node can also fetch a protocol over the network, for example
before starting the test chain or activating a new amendment. Like in
the previous case, once the code is downloaded, it will be compiled and
dynamically linked.

Protocol Activation
===================

Generally, a node starts its execution with the ``genesis`` protocol.
``genesis`` provides an operation to upgrade to a new protocol.
Interestingly, upgradability is a feature of the protocol, not of the
shell (though the shell can also force a protocol upgrade). Protocols
may or may not be upgradable. The *raison d’être* of ``genesis`` is
upgradability, ``alpha`` is upgradable by voting, while ``demo_noops``
is not upgradable.

The client command ``activate protocol`` is a shorthand to craft the
activation operation offered by ``genesis`` to upgrade to a new
protocol.

::

   $ echo "{ }" > /tmp/protocol_parameters.json
   $ octez-client activate protocol \
     ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp \
     with fitness 5 and key activator and parameters /tmp/protocol_parameters.json

   Injected BMX1tHrYDdWC

This command injects a so-called "activation block" to the blockchain
(the command returns the prefix of the hash of this block). This block
is the only one using the ``genesis`` protocol. It is a block that
contains only one operation: the operation that activates the next
protocol (in our case, ``demo_noops``). The next block in the blockchain
will be the first block using the activated protocol. Let us detail the
parameters of this command:

- ``ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp`` is the hash of
  the protocol to be activated. The protocol must be registered.
- ``activator`` is an alias for an activation *secret* key. In this
  example, the corresponding public key has been passed as a parameter
  to ``octez-node`` at startup (using the ``--sandbox`` argument). The
  alias is known to the client because it is added by default in sandbox
  mode.
- ``5`` is the fitness of the activation block (more details below). It
  can be any number.
- ``protocol_parameters.json`` is a file that contains protocol-specific
  initialization parameters. There are no parameters for ``demo_noops``,
  so this file contains an empty json object (i.e. ``{ }``).

Run the following two commands to inspect the
first two blocks of the blockchain (in particular the values of the
``"protocol"``, ``"hash"``, ``"predecessor"``, ``"level"``, and
``"fitness"`` keys).

::

   $ curl localhost:18731/chains/main/blocks/head | jq
   $ curl localhost:18731/chains/main/blocks/head~1 | jq

Protocol Structure and Compilation
==================================

Currently, the embedded protocols live in the tezos repository besides
the rest of the code. They follow the naming convention ``proto_*``. The
code of ``proto_demo_noops`` is organized as shown below:

::

   $ ls -R src/proto_demo_noops

   lib_protocol/

   src/proto_demo_noops/lib_protocol:
   TEZOS_PROTOCOL  dune            main.ml         main.mli

The protocol code resides in the ``lib_protocol`` directory. A protocol
must define a ``TEZOS_PROTOCOL`` json file that contains the hash of the
protocol and the list of OCaml modules.

::

   $ cat src/proto_demo_noops/lib_protocol/TEZOS_PROTOCOL

   {
       "expected_env_version": 17,
       "hash": "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp",
       "modules": ["Main"]
   }

Besides the ``TEZOS_PROTOCOL`` and ``main.ml[i]`` files, the ``dune`` file
in ``lib_protocol`` is used for compiling the protocol, checking that
it respects the restrictions explained below, and for linking it with
the other components of the node.

Currently, protocols are compiled differently depending on whether they
are embedded or injected. Injected protocols are compiled by the Tezos
compiler embedded in ``tezos-node``. Embedded protocols are compiled as
OPAM libraries using the standard toolchain.

Protocol Interface
==================

The economic protocol is a *sandboxed* component restricted in the
following two ways.

- It can only access modules defined by the *protocol environment*.

- It must define a module ``Main`` which implements the interface
  ``Updater.PROTOCOL`` from
  :src:`src/lib_protocol_environment/sigs/v17/updater.mli`.
  The shell interacts with the protocol through this interface.

In addition, just like any other node component, the protocol can define
RPC services to interact with a client. We will address RPCs in the second
tutorial but there is no difficulty here apart from getting accustomed with
the Tezos RPC library.

Environment
-----------

The environment of the protocol is a fixed set of OCaml modules (their
signatures are declared in :src:`src/lib_protocol_environment/sigs/v17/`),
consisting in a carefully chosen subset of the OCaml standard library,
plus specialized utility modules. This form of sandboxing the protocol
ensures that the protocol code does not use unsafe functions. :doc:`../shell/protocol_environment`
explains in more detail the restrictions the environment imposes.

Any datatype used by the protocol is defined in this environment (in
particular, all modules or types mentioned below). The following is the
list of Tezos-specific modules defined by the environment that we will
be mentioning throughout this tutorial: ``Block_header``, ``Context``,
``Operation``, and ``Updater``.

Updater.PROTOCOL
----------------

At a very high-level, a protocol must:

1. implement protocol-specific types, such as the type of operations or
   protocol-specific block header data (in addition to the shell generic
   header),

2. define under which conditions a block is a valid extension of the
   current blockchain, and define an ordering on blocks to arbitrate
   between concurrent extensions.

For instance, in a bitcoin-like protocol, the supported operations are
transactions, and the block header data contains a *proof of work (PoW)*
stamp. A block is valid if its operations are supported by enough funds
and the PoW stamp is correct.

For the second point, *at a conceptual level*, the protocol defines the
function ``apply: context -> block -> (context * fitness) option`` which
is called whenever the node processes a block. *context* represents the
*protocol state* and *fitness* is an integer used to compare blocks. The
context is therefore protocol-specific, it may contain, for instance, a
list of accounts and their balance. It must contain enough information
to determine the validity of a new block. The fitness defines a total
ordering between blocks (and therefore between chains). The ``option``
type is used here to represent block validity: the function returns
``None`` when the block is not valid, while if it is valid, it returns
the block’s fitness and the updated protocol state, obtained after
applying (the operations contained in) the block.

The `signature
PROTOCOL <http://tezos.gitlab.io/api/odoc/_html/tezos-protocol-environment-sigs/Tezos_protocol_environment_sigs/V0/module-type-T/Updater/index.html>`__
in module ``Updater`` captures these general ideas (explained in more
detail in the `Tezos white
paper <https://tezos.com/whitepaper.pdf>`__),
but is slightly more complex, mostly for efficiency reasons. In this
tutorial, we will cover only some aspects of the interface and we
will cover it more fully in second tutorial.

*Concretely*, a context (represented by the type ``Context.t``) is a
disk-based immutable key-value store, namely, a map from ``string list``
to ``MBytes.t``. Such a loosely structured datatype should accommodate
most protocols. A fitness (represented by the type ``Fitness.t``) is a
list of byte arrays. A total order on blocks is obtained by comparing
their fitness first by length and then lexicographically.

A Tezos *block* is composed of a *block header* (of type
``Block_header.t``) and a list of operations. A block header has two
parts, a protocol-independent :ref:`shell header <shell_header>`
and a protocol-specific header, which is a byte array (with type
``MBytes.t``). Similarly, *operations* (of type ``Operation.t``) have a
protocol-independent shell header, and a protocol-specific header. For
instance, ``Block_header.t`` is defined as follows.

.. code:: ocaml

     type t = {
       shell: shell_header ;
       protocol_data: MBytes.t ;
     }

As part of implementing the ``PROTOCOL`` signature, the protocol must in
particular provide concrete types for the protocol-specific block header
(``type block_header_data``) and operations (``type operation_data``).
These types are private to the protocol. The only functions exported to
the shell are encoders/decoders. This allows the shell to serialize
these types, either in binary format or in json. Typically, the binary
format is used for P2P communications, and json is used for
human-readable RPCs. Here is an excerpt from the ``PROTOCOL`` signature
where these types are declared:

.. code:: ocaml

     (** The version specific type of blocks. *)
     type block_header_data

     (** Encoding for version specific part of block headers.  *)
     val block_header_data_encoding: block_header_data Data_encoding.t

     (** A fully parsed block header. *)
     type block_header = {
       shell: Block_header.shell_header ;
       protocol_data: block_header_data ;
     }

Note the analogy between ``Block_header.t`` (the shell’s view of the
block header) and ``block_header`` (the protocol’s view of the block
header).

Several functions declared in the ``PROTOCOL`` signature realize
together the ``apply`` functionality: ``begin_application``,
``begin_partial_application``, ``begin_construction``,
``apply_operation,`` and ``finalize_block``. A typical ``apply`` is
represented by a call to ``begin_(application|construction)``, followed
by a sequence of calls to ``apply_operation``, one for each operation in
the block, and finally a call to ``finalize_block``. These functions use
values with types ``validation_result`` and ``validation_state``.
Defined by the ``PROTOCOL`` signature, the type ``validation_result``
represents the result of a block application, and it is a record type
that contains most notably a context and a fitness. ``validation_state``
is a protocol-defined datatype used as intermediary state between
applications of operations. To understand the usage of these two types,
it may be useful to consider the following simplification of the types
of the five functions mentioned:

.. code:: ocaml

   begin_application: Context.t -> block_header -> validation_state
   begin_partial_application: Context.t -> block_header -> validation_state
   begin_construction: Context.t -> ?protocol_data: block_header_data -> validation_state
   apply_operation: validation_state -> operation -> validation_state
   finalize_block: validation_state -> validation_result

We briefly describe the role of these five functions:

- ``begin_application`` is used when validating a block received from
  the network.

- ``begin_partial_application`` is used when the shell receives a block
  more than one level ahead of the current head (this happens, for
  instance, when synchronizing a node). This function should run
  quickly, as its main role is to reject invalid blocks from the chain
  as early as possible.

- ``begin_construction`` is used by the shell when instructed to build a
  block and for validating operations as they are gossiped on the
  network. This two cases are distinguished by the optional
  ``protocol_data`` argument: when only validating operations the
  argument is missing, as there is no block header. In both of these
  cases, the operations are not (yet) part of a block which is why the
  function does not expect a shell block header.

- ``apply_operation`` is called after ``begin_application`` or
  ``begin_construction``, and before ``finalize_block``, for each
  operation in the block or in the mempool, respectively. Its role is to
  validate the operation and to update the (intermediary) state
  accordingly.

- ``finalize_block`` represents the last step in a block validation
  sequence. It produces the context that will be used as input for the
  validation of the block’s successor candidates.

Another important function in the ``PROTOCOL`` interface is ``init``,
which is called when the protocol is activated. It takes as parameters a
context and the shell header of the last block of the previous protocol.
The context is the context corresponding to this last block, which
includes the protocol parameters given at activation time. It returns a
``validation_result``, which contains a context that is prepared for the
new protocol. Note that the new context may change the key-value
structure of the store compared with the previous protocol. ``init`` is
therefore responsible for making the migration of the context from the
previous protocol to the current protocol.

Finally, let us emphasize that the protocol is a *stateless* component.
Rather than maintaining a mutable state, it implements pure functions
that that take a state as a parameter and return a new state. The shell
is responsible to store this state between function calls.

Protocol ``demo_noops``
=======================

The ``demo_noops`` protocol is very simple:

- It has no operations (hence no-ops).
- It does not update its state, ``context`` is never modified.
- The fitness of a block is the block’s level (i.e. its height in the
  blockchain).

We now go through the types and functions which do not have a trivial
definition. First, we simply choose to have a string as the block
header. Therefore we define in ``main.ml``:

.. code:: ocaml

     type block_header_data = string

     let block_header_data_encoding =
       Data_encoding.(obj1 (req "block_header_data" string))

For the encoding of the (protocol-specific) block header we rely on the
``data_encoding`` library, see :doc:`data_encoding`.

As there are no operations, the type of an operation header is just
``unit``. Similarly, as we do not use the other helper datatypes like
``block_header_metadata`` and ``operation_receipt``, we simply set
these types to ``unit``.

Next, we need to define a ``validation_state``. We define it as record
datatype that contains a context and a fitness, because these need to be
passed to the ``validation_result`` returned by ``finalize_block``.

.. code:: ocaml

     type validation_state = {
       context : Context.t ;
       fitness : Fitness.t ;
     }

Concerning the fitness, we assume that the protocol is instantiated from
``genesis``. Note that this may not be the case in general.
``demo_noops`` could very well be instantiated from a previous protocol
with a totally different format for the fitness. The protocol should be
able to adjust to different fitness models. Here, however, we use the
same fitness model as ``genesis`` (and ``alpha``), where the fitness has
the form ``xx:xxxxxxxxxxxxxxxx``. That is, the fitness is a list of two
byte arrays, the first one (``xx``, of length 1), representing the
protocol version, and the second one encoding an ``int64`` number (thus
of length 8). Recall that there is only one block using the ``genesis``
protocol. For this block, the fitness’ first element is ``00`` and its
second element encodes the integer given as the fitness parameter when
activating the next protocol. In ``demo_noops``, the first element is
``01`` and the second element represents the level.

The helper functions needed to implement the fitness are as follows:

.. code:: ocaml

     let version_number = "\001"

     let int64_to_bytes i =
       let b = MBytes.create 8 in
       MBytes.set_int64 b 0 i;
       b

     let fitness_from_level level =
       [ MBytes.of_string version_number ;
         int64_to_bytes level ]

The fitness of a new block is actually set in ``begin_construction``,
which has the following very simple implementation:

.. code:: ocaml

     let begin_construction
         ~chain_id:_
         ~predecessor_context:context
         ~predecessor_timestamp:_
         ~predecessor_level
         ~predecessor_fitness:_
         ~predecessor:_
         ~timestamp:_
         ?protocol_data:_ ()
       =
       let fitness = fitness_from_level Int64.(succ (of_int32 predecessor_level)) in
       ... (* output a log message *)
       return { context ; fitness }

The implementation of the other main functions is trivial:
``begin_application`` just builds the validation state from the
predecessor context and the fitness from (the shell part of) the block
header. ``begin_partial_application`` behaves like
``begin_application``. ``apply_operation`` returns an error (however, it
is never called), and ``finalize_block`` builds a validation result from
the validation state by copying the context and the fitness, and setting
default values for the other fields. Most functions also record a log
message which allows one to see when these functions are called during
the node’s execution. They also show how the fitness is updated.

Finally, this protocol does not define any RPC.

.. code:: ocaml

   let rpc_services = RPC_directory.empty

Baking a block
==============

We can build a rudimentary baker simply using the RPCs provided by the
node. The RPC to inject a block is ``/injection/block``. However, we
need to provide an hexadecimal binary encoding of the block header. To
obtain it we use the following RPC:
``/chains/main/blocks/head/helpers/forge_block_header``. This RPC
expects as argument a json representation of the block header. The json
representation of the shell header (the protocol-independent part of the
header) can be obtained with the following RPC:
``/chains/main/blocks/head/helpers/forge_block_header``.

We will thus use the following RPCs to bake a block:

1. ``/chains/main/blocks/head/helpers/preapply/block``
2. ``/chains/main/blocks/head/helpers/forge_block_header``
3. ``/injection/block``

We call the first RPC with the protocol hash, the protocol block header,
and the (empty) list of operations. The RPC service calls
``begin_construction`` and ``finalize_block`` of the ``demo_noops``
protocol and returns the built (but not injected) block in json format.
Notice the json representation of the protocol block header data
``"block_header_data": "hello world"``) is the one we defined in our
implementation of ``demo_noops``.

::

   $ octez-client -p ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im \
     rpc post /chains/main/blocks/head/helpers/preapply/block with \
     '{"protocol_data":
         {"protocol": "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp",
          "block_header_data": "hello world"},
          "operations": []}'

   { "shell_header":
       { "level": 2, "proto": 1,
         "predecessor": "BLCJ5s7SGvMzmJd7Y7jbpuNiTN5c8yz9L4Q2GtBpLaJTAHKMEoz",
         "timestamp": "2019-06-21T15:35:37Z", "validation_pass": 0,
         "operations_hash":
           "LLoZS2LW3rEi7KYU4ouBQtorua37aWWCtpDmv1n2x3xoKi6sVXLWp",
         "fitness": [ "01", "0000000000000002" ],
         "context": "CoV3MLpgMM91DbHGuqGz7uwgmMYjnh7EQSsqt1CxPqvxQpU9pczA" },
     "operations": [] }

``octez-client`` can use protocol-specific extensions. By default,
``octez-client`` tries to use the extension corresponding to the node’s
protocol. In our case no such extension has been given, therefore we
need to specify an extension using the ``-p XXX`` option, where ``XXX``
is a protocol hash.

Now we use the second RPC to obtain the binary encoding of the protocol
block header (mind replacing the fields "predecessor", "timestamp", "operations_hash", "fitness", and "context" with the values output by the previous command):

::

   $ octez-client -p ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im \
       rpc post /chains/main/blocks/head/helpers/forge_block_header \
       with '{"level": 2, "proto": 1,
              "predecessor": "BLCJ5s7SGvMzmJd7Y7jbpuNiTN5c8yz9L4Q2GtBpLaJTAHKMEoz",
              "timestamp": "2019-06-21T15:35:37Z", "validation_pass": 0,
              "operations_hash": "LLoZS2LW3rEi7KYU4ouBQtorua37aWWCtpDmv1n2x3xoKi6sVXLWp",
              "fitness": ["01", "0000000000000002"],
              "context": "CoV3MLpgMM91DbHGuqGz7uwgmMYjnh7EQSsqt1CxPqvxQpU9pczA",
              "protocol_data": "0000000b68656c6c6f20776f726c64"}'

   { "block": "0000000201b478f20b61340c9e8290d7b45edf057fd180891d0e0b290abc..." }

Notice the last field ``protocol_data``. It must contain the
binary-encoded block header data. Remember that we specified this
encoding in the protocol with

.. code:: ocaml

   let block_header_data_encoding =
     Data_encoding.(obj1 (req "block_header_data" string))

We can compute the binary encoding on the client side, for instance
using the ``Data_encoding`` library, or by writing the encoder in a
different language using the public specification of the
``Data_encoding`` library.

For this example, ``"0000000b68656c6c6f20776f726c64"`` is the binary
encoding of ``"hello world"``.

Finally, the last RPC injects the block (mind replacing the field "data" with the value output by the previous command in field "block"). After the block is validated by
the protocol (the RPC service calls ``begin_application`` and
``finalize_block``), the RPC returns its hash.

::

   $ octez-client -p ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im \
     rpc post injection/block with \
      '{"data": "0000000201b478f20b61340c9e8290d7b45edf057fd180891d0e0b290abc...",
        "operations": []}'

   "BM6qcDPhm57sXHv1js25qcy9WESah1C3qcpKn9y8bRZzpf8s7g8"

We can look at the newly created block:

::

   $ octez-client -p ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im \
     rpc get /chains/main/blocks/head/

   { "protocol": "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp",
     "chain_id": "NetXdQprcVkpaWU",
     "hash": "BMKeY5PDbm3acKDPUt7XnARkFBj5JoUDfMbBqYTYvWrGFHPt89a",
     "header":
       { "level": 2, "proto": 1,
         "predecessor": "BLCJ5s7SGvMzmJd7Y7jbpuNiTN5c8yz9L4Q2GtBpLaJTAHKMEoz",
         "timestamp": "2019-06-21T15:35:37Z", "validation_pass": 0,
         "operations_hash":
           "LLoZS2LW3rEi7KYU4ouBQtorua37aWWCtpDmv1n2x3xoKi6sVXLWp",
         "fitness": [ "01", "0000000000000002" ],
         "context": "CoV3MLpgMM91DbHGuqGz7uwgmMYjnh7EQSsqt1CxPqvxQpU9pczA",
         "block_header_data": "hello world" },
     "metadata":
       { "protocol": "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp",
         "next_protocol": "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp",
         "test_chain_status": { "status": "not_running" },
         "max_operations_ttl": 0, "max_operation_data_length": 0,
         "max_block_header_length": 100, "max_operation_list_length": [] },
     "operations": [] }

For completeness, we show below the node’s trace::

   Generating a new identity... (level: 0.00)
   Stored the new identity (idrwY8MRTyKLQ45f8Hx1VP7pLvEU19) into '/var/folders/wq/63lgkbp565dgl109gqkxwh000000gn/T/tezos-node.XXXXXXXX.5LIf6DdGdT/identity.json'.
   Nov 24 13:56:37.284 NOTICE │ the node configuration has been successfully
   Nov 24 13:56:37.284 NOTICE │   validated.
   Nov 24 13:56:37.285 NOTICE │ read identity file
   Nov 24 13:56:37.285 NOTICE │ starting the Octez node 21.0~rc3+dev (e1573177)
   Nov 24 13:56:37.383 NOTICE │ disabled local peer discovery
   Nov 24 13:56:37.384 NOTICE │ p2p initialization: bootstrapping
   Nov 24 13:56:37.397 NOTICE │ p2p initialization: p2p_maintenance_started
   Nov 24 13:56:37.809 NOTICE │ external validator initialized
   Nov 24 13:56:37.809 NOTICE │ initializing irmin context at
   Nov 24 13:56:37.809 NOTICE │   /var/folders/wq/63lgkbp565dgl109gqkxwh000000gn/T/tezos-node[...]
   Nov 24 13:56:38.770 NOTICE │ activate chain NetXdQprcVkpaWU
   Nov 24 13:56:38.770 NOTICE │ chain is bootstrapped
   Nov 24 13:56:38.770 NOTICE │ synchronisation status: synced
   Nov 24 13:56:38.771 NOTICE │ starting local RPC server on
   Nov 24 13:56:38.771 NOTICE │   ::ffff:127.0.0.1:18731 (acl = AllowAll)
   Nov 24 13:56:38.771 NOTICE │ the Tezos node is now running

After the protocol activation command, we further see the following output::

   Nov 24 13:57:37.232 NOTICE │ initializing protocol ProtoDemoNoo...
   Nov 24 13:57:37.233 NOTICE │ block at level 1 successfully pre-applied in 748us
   Nov 24 13:57:37.237 NOTICE │ the protocol table was updated: protocol
   Nov 24 13:57:37.237 NOTICE │   ProtoDemoNoo (level 1) was activated on block
   Nov 24 13:57:37.237 NOTICE │   BLrgJzwN4M68uCCGrkYXRcKUqxpDewMWM1gxUwyZ7J8sgYZCuvZ
   Nov 24 13:57:37.237 NOTICE │   (level 1)
   Nov 24 13:57:37.237 NOTICE │ head is now
   Nov 24 13:57:37.237 NOTICE │   BLrgJzwN4M68uCCGrkYXRcKUqxpDewMWM1gxUwyZ7J8sgYZCuvZ
   Nov 24 13:57:37.237 NOTICE │   (1)
   Nov 24 13:58:45.304 NOTICE │ block at level 2 successfully pre-applied in 568us
   Nov 24 14:00:35.405 NOTICE │ head is now
   Nov 24 14:00:35.405 NOTICE │   BKzSsYtHSxgPWgyw7YktHTrddzgGQgKr3bLAm3NjxwMCyrFR7qH
   Nov 24 14:00:35.405 NOTICE │   (2)

Protocol tests
--------------

The tests we covered are also implemented in the testsuite of the demo protocol Noop, and can be executed with::

    dune exec tezt/tests/main.exe -- --title demo_noops

The test is contained in module ``Demo_noops`` within file :src:`tezt/tests/demo_protocols.ml`.

The test launches a node in sandbox, activates the ``demo_noops``
protocol, and bakes a block.

Conclusion
==========

We saw how to write, compile, register, activate and use a simple
protocol. In the next tutorial, we’ll make this protocol more realistic
by adding operations and block validation. We’ll also improve the client
interface by defining RPCs in the protocol, as well as extending the
``octez-client`` command-line interface with protocol-specific commands.
