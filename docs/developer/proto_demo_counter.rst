======================================
How to write a Tezos protocol - Part 2
======================================

This is the second part of a couple of tutorials on how to implement a Tezos
protocol.

In the :doc:`first part <proto_demo_noops>`,
we saw how to write, compile, register, activate and use an extremely
simple protocol. We also looked at the interface between the protocol
and the *shell*.

In this tutorial, we consider a new protocol called ``demo_counter`` which
extends ``demo_noops`` from the first part in several ways.

- Blocks can contain simple operations, whose effects update the
  blockchain state.
- It is parameterized by protocol parameters passed at activation time.
- It defines REST services (a.k.a. RPCs), in addition to the generic
  ones already available from the shell.
- It defines a client library, extending ``octez-client`` with
  protocol-specific commands.

A large part of this tutorial is devoted to the client library. While this
library is not part of the protocol per se, it is needed if we want to
communicate with the node in any meaningful way.

This protocol and the client library also make use of additional
libraries, such as command-line parsing tools, error monads, RPCs…
Describing them in detail would be beyond the scope of this tutorial, but we
try to provide enough information to keep the tutorial self-contained.

``demo_counter`` can be found in the Octez repository in ``src/proto_demo_counter/``.

We refer to the first tutorial for compilation instructions. In most cases,
it should be enough to run

::

   make build-deps
   make

Protocol ``demo_counter``
=========================

The protocol is referred to by the hash
``ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT``. We can check it
is indeed known by the node.

::

   # octez-admin-client list protocols
   ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK
   ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT
   ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp
   ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im
   ...

It is defined by several modules:

::

   ls src/proto_demo_counter/lib_protocol/*.mli
   lib_protocol/apply.mli           lib_protocol/main.mli            lib_protocol/receipt.mli
   lib_protocol/error.mli           lib_protocol/proto_operation.mli lib_protocol/services.mli
   lib_protocol/header.mli          lib_protocol/proto_params.mli    lib_protocol/state.mli

Most protocol-specific types required in ``Main`` are now defined in
separate modules.

- ``block_header_data`` is defined as ``Header.t``,
- ``operation_receipt`` is defined as ``Receipt.t``,
- ``operation_data`` is defined as ``Proto_operation.t``.

As for ``demo_noops``, ``block_header_data`` is still a string, and
fitness is defined as the height of the chain. However, ``demo_counter``
uses a different fitness format than ``demo_noops``: it uses version
number ``\255`` (``ff`` in hex) and encodes the level as a 64-bit
integer, with a 5-element fitness of the form
``ff::00::00::00::LEVEL``. This higher version number allows testing
migration from ``alpha`` to ``demo_counter``.

More interesting are the protocol operations and the operation receipt.

The protocol defines three operations in ``Proto_operation.t``, which
act on a state ``State.t`` stored in the protocol *context*. As seen in
the first part, in the Tezos model each operation is applied to a
context and can produce a new context, the context is a map that can be
seen as the current state of the blockchain. For ``proto_counter``, the
context maps a key ``"state"`` to a serialized form of ``State.t``.

``State`` is simply a couple of nonnegative counters (which we can also
view as the balances of two accounts).

::

   type t = { a : int32; b : int32 }

Operations are defined in ``Proto_operation`` as

::

   type t =
     | IncrA
     | IncrB
     | Transfer of Int32.t (* transfer from A to B, possibly a negative amount *)

The module ``Apply`` defines a function

::

   val apply : State.t -> Proto_operation.t -> State.t option

that applies the operation as expected. Some operations may be invalid
(and in this case ``apply`` returns ``None``). For instance, transfer
requires that both counters stay nonnegative, and increment operations
require that counters don’t overflow.

Operation application is defined by the function
``Main.apply_operation``.

.. code:: ocaml

   val apply_operation :
     application_state ->
     Operation_hash.t ->
     operation ->
     (application_state * operation_receipt) tzresult Lwt.t

The implementation uses a helper function ``apply_operation_aux``
that is shared with ``validate_operation``:

.. code:: ocaml

   let apply_operation_aux application_state operation =
     let open Lwt_result_syntax in
     let {context; fitness} = application_state in
     let*! state = State.get_state context in
     match Apply.apply state operation.protocol_data with
     | None -> Error_monad.tzfail Error.Invalid_operation
     | Some state ->
       let*! context = State.update_state context state in
       return {context; fitness}

   let apply_operation application_state _oph operation =
     let open Lwt_result_syntax in
     Logging.log Notice "apply_operation" ;
     let* application_state = apply_operation_aux application_state operation in
     let receipt = Receipt.create "operation applied successfully" in
     return (application_state, receipt)

This is quite straightforward. If the application succeeds, fitness is
left unchanged and the resulting context contains the updated state.
This function also returns a *receipt* that describes the effect of the
operation. In this protocol, the receipt is simply a string, but it
could be more descriptive. If the application fails, an error is
returned via an :doc:`error monad <error_monad>`. All
protocol errors are registered in ``Error``.

Protocol parameters
-------------------

We saw in the first part that when a protocol is activated, we can pass
to it initialization parameters through a JSON value. This value is
provided by a user through a file argument, e.g.,
``protocol_parameters.json``, to the activation command. The
``demo_noops`` protocol did not take advantage of this feature, but
``demo_counter`` uses a JSON value of the form
``{"init_a": A, "init_b": B}``, where ``A`` and ``B`` are the initial
values of the counters.

The type of the protocol parameters and their encoding are defined in
``Proto_params``.

By convention, the protocol parameters are stored in the context under
the key ``"protocol_parameters"``. The activation operation of the
``genesis`` protocol sets the parameters under this key, and they are
retrieved by ``demo_counter`` in ``Main.init``.

RPC Services
------------

The protocol implements two services.

- ``/chains/main/blocks/head/counter/a`` returns the value of counter
  ``a``
- ``/chains/main/blocks/head/counter/b`` returns the value of counter
  ``b``

Services rely on the ``RPC_*`` modules accessible through the
:doc:`protocol environment <../shell/protocol_environment>`.
Ultimately, these modules are implemented by the ``octez-libs.rpc`` library.

Services are registered by the function ``Services.rpc_services``, which
is called by ``Main.rpc_services`` at protocol activation.

::

   val rpc_services : Updater.rpc_context RPC_directory.t

   let rpc_services = Services.rpc_services

Compilation
-----------

Two libraries are compiled from the protocol code.

- ``tezos-protocol-demo-counter`` is linked to the client library (see
  below),
- ``tezos-embedded-protocol-demo-counter`` is linked to the node (see
  :src:`src/bin_node/dune`).

Recall from the first part that the protocol hash and modules are given
in ``TEZOS_PROTOCOL``.

::

   > cat src/proto_demo_counter/lib_protocol/TEZOS_PROTOCOL
   {
       "expected_env_version": 17,
       "hash": "ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT",
       "modules": ["Error", "Proto_params", "Header", "State",
                   "Proto_operation", "Receipt", "Apply", "Services", "Main"]
   }

The compilation steps are given in a generic ``dune`` file common to all
protocols, and a protocol-specific file ``dune.inc`` that can be
generated from the ``TEZOS_PROTOCOL`` file.

Client library
==============

The client and the node interact using RPCs. In theory, we could write a
client for ``demo_counter`` from scratch in any language, but it is
convenient to simply extend ``octez-client`` with a protocol-specific
client library. Hence, we can keep using generic features of
``octez-client``, such as wallet management, and simply add new commands
specific to the new protocol. Moreover, we can use well-tested OCaml
libraries to conveniently call shell RPCs, and we can access to the
protocol code from the client, for instance to access some of its
datatypes or services.

For ``demo_counter``, the client library is
``tezos-client-demo-counter`` and it is defined in
:src:`src/proto_demo_counter/lib_client/`. It is linked to ``octez-client``
(see :src:`src/bin_client/dune`). It is composed of four modules
``Client_proto_args``, ``Client_proto_commands``, ``Client_proto_main``,
``Protocol_client_context`` which we will describe as we go.

We can check that ``demo_counter`` is indeed known to the client.

::

   # octez-admin-client list understood protocols | grep -i demo
   ProtoDemoCou

Note that ``Proto_demo_noops`` isn’t in the list since it doesn’t have a
client library.

User interface
--------------

``demo_counter`` adds a few new commands.

::

   # octez-client -p ProtoDemoCou man
   ...
   Commands for protocol Demo_counter:
     bake <message>
       Bake a block
       <message>: message in block header
     increment a
       Increment A
     increment b
       Increment B
     transfer <amount>
       Transfer from A to B
       <amount>: amount taken from A and given to B (possibly negative)
     get a
       Get A counter
     get b
       Get B counter
   ...

Client/Node interaction
-----------------------

Typically, the client library interacts with the node in two ways.

- using shell services (through the ``octez-shell-libs.shell-services`` library),
- using protocol services (through the protocol library, i.e.
  ``tezos-protocol-demo-counter``).

Let us see how to use these libraries in practice.

Library ``tezos-protocol-demo-counter``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The library ``tezos-protocol-demo-counter`` contains the protocol code
and its environment. The modules of the protocol are grouped in a module
``Protocol``. The environment is accessible from module
``Protocol.Environment``.

Typically, we want to use the datatypes defined in the protocol. For
instance, to build blocks, we need access to the type of operations and
block header data. We also want to access protocol-defined services. In
our case, we can get the counter values using the *client stub*
``Services.get_counter``.

Remark that although it is feasible, the client code should not use
protocol functions who read or write the protocol context.

Library ``octez-shell-libs.shell-services``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This library defines client stubs to call shell RPC services. The
``demo_counter`` client library uses *injection services* and *block
services*.

As an example, consider the (slightly simplified) function
``Shell_services.Injection.block`` defined in module
``Injection_services`` in :src:`src/lib_shell_services/`.

::

   val block :
     #simple ->
     ?async:bool ->
     ?force:bool ->
     ?chain:Chain_services.chain ->
     Bytes.t ->
     Operation.t list list ->
     Block_hash.t tzresult Lwt.t

- ``#simple`` is the *RPC context*, which identifies the
  server, deals with the networking aspects of the call, and serializes
  the transmitted values.
- ``Bytes.t`` contains the encoded block header.
- ``Operation.t`` is the generic shell operation type.
- ``Block_hash.t`` is the hash of the injected block.

This function calls the service ``/injection/block`` and serializes the
parameters and the returned value as expected.

The client library uses two injection services.

- ``Shell_services.Injection.block`` to call ``/injection/block``,
- ``Shell_services.Injection.operation`` to call
  ``/injection/operation``.

The other family of services, block services, is defined as a functor
parameterized by a protocol type ``Block_services.PROTO`` defined in
``lib_shell_services``. This allows the stubs to deserialize the JSON
values returned by the services, and to return the actual protocol types
to the client.

In ``client_proto_commands.ml``, we instantiate the functor
``Block_services.Make`` with the ``Protocol`` module (there are two
occurrences because we need to provide a module for both the current and
the next protocol).

::

   module Demo_block_services = Block_services.Make(Protocol)(Protocol)

The client library uses the following stubs:

- ``Demo_block_services.hash`` to call ``/chains/main/blocks/head/hash``
- ``Demo_block_services.Mempool.pending_operations`` to call
  ``/chains/main/mempool/pending_operations``
- ``Demo_block_services.Helpers.Preapply.block`` to call
  ``/chains/main/blocks/head/helpers/preapply/block``
- ``Demo_block_services.Helpers.Preapply.operations`` to call
  ``/chains/main/blocks/head/helpers/preapply/operations/``

Commands implementation
-----------------------

Commands are implemented with the help of several libraries.

- ``octez-libs.clic`` is a command-line parsing library.
- ``octez-shell-libs.client-commands`` provides the registration function for new
  commands.
- ``octez-shell-libs.client-base`` defines notably the ``Client_context.full`` class,
  which contains the client context (e.g. wallet, printing facilities,
  RPC context…).

The commands’ syntax is defined in the modules ``Client_proto_main`` and
``Client_proto_args``. Commands are registered using the function
``Client_command.register``.

Commands behavior is implemented in module ``Client_proto_commands``. A
command implementation may use a value of type ``Client_context.full``,
which is provided by the registration function. Through this object,
commands can access the client and RPC contexts.

More precisely, our command implementations use a
``Protocol_client_context.full`` object, which is a specialized version
of ``Client_context.full``, defined in ``Protocol_client_context``.

Let us have a closer look to the block baking command
``Client_proto_commands.bake``.

::

   let bake (cctxt : Protocol_client_context.full) message : unit tzresult Lwt.t =
     let open Lwt_result_syntax in
     let* {validated; _} =
       Demo_block_services.Mempool.pending_operations cctxt ()
     in
     let operations = List.map snd validated in
     let block_header_data = Header.create message in
     let* shell, preapply_result =
       Demo_block_services.Helpers.Preapply.block
         cctxt
         [operations]
         ~protocol_data:block_header_data
     in
     let block_header_data_encoded =
       Data_encoding.Binary.to_bytes_exn Header.encoding block_header_data
     in
     let header : Block_header.t =
       {shell; protocol_data = block_header_data_encoded}
     in
     let header_encoded =
       Data_encoding.Binary.to_bytes_exn Block_header.encoding header
     in
     let preapply_result =
       WithExceptions.Option.get ~loc:__LOC__ @@ List.hd preapply_result
     in
     let operations = [List.map snd preapply_result.applied] in
     let* block_hash =
       Shell_services.Injection.block cctxt header_encoded operations
     in
     let*! () =
       cctxt#message "Injected block %a" Block_hash.pp_short block_hash
     in
     return_unit

First, it retrieves the applied operations from the mempool using
``Demo_block_services.Mempool.pending_operations``. It then uses the
pre-apply service and ask the node to build a block based on the
proposed operations and block header data. The block is then encoded and
sent to the node through the injection service.

Sample execution of the protocol
================================

First we activate the protocol using ``protocol_parameters.json``
defined as

::

   {"init_a": 100, "init_b": 100}

::

   # octez-client -block genesis activate protocol ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT with fitness 1 and key activator and parameters protocol_parameters.json --timestamp 2019-07-05T14:30:35Z
   Injected BLf2cXRZKsby

This bakes block of level ``1``, running protocol ``genesis``, with
``demo_counter`` scheduled for the next block.

::

   # octez-client rpc get /chains/main/blocks/head/metadata
   { "protocol": "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im",
     "next_protocol": "ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT",
     "test_chain_status": { "status": "not_running" }, "max_operations_ttl": 0,
     "max_operation_data_length": 100, "max_block_header_length": 100,
     "max_operation_list_length": [ { "max_size": 1000 } ] }

Although the head is a ``genesis`` block, ``demo_counter`` has already
been activated and we can bake an empty block using the ``bake`` command
from ``demo_counter`` client library.

::

   # octez-client bake '"This is block 2"'
   Injected block BLrQqbn13Vrb

We can check that the block was baked properly, in particular the block
header data has been set as expected. We can also see the protocol state
``State.t`` in the block metadata encoded as specified by
``State.encoding``.

::

   # octez-client rpc get /chains/main/blocks/head/
   { "protocol": "ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT",
     "chain_id": "NetXdQprcVkpaWU",
     "hash": "BLrQqbn13VrbzUprxQypzAg6fc7YmsHaZvGwGrHJk8a4eG6e11B",
     "header":
       { "level": 2, "proto": 1,
         "predecessor": "BLf2cXRZKsbygWJdtf1PBbrSg8yHkNK39bgoApvdbYBd1EX9ung",
         "timestamp": "2019-07-05T14:30:36Z", "validation_pass": 1,
         "operations_hash":
           "LLoaGLRPRx3Zf8kB4ACtgku8F4feeBiskeb41J1ciwfcXB3KzHKXc",
         "fitness": [ "01", "0000000000000002" ],
         "context": "CoVpDgKDiWZ9xcodUFng1C8oGvfXEqBCD5XxQjB8Jrwptkx3vHUB",
         "demo_block_header_data": "This is block 2" },
     "metadata":
       { "protocol": "ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT",
         "next_protocol": "ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT",
         "test_chain_status": { "status": "not_running" },
         "max_operations_ttl": 0, "max_operation_data_length": 100,
         "max_block_header_length": 100,
         "max_operation_list_length": [ { "max_size": 1000 } ], "demo_a": 100,
         "demo_b": 100 }, "operations": [ [] ] }

We now inject three operations using client commands.

::

   octez-client increment a
   Operation receipt: operation applied successfully
   Injected: op5gBsE7EMi7

::

   # octez-client increment b
   Operation receipt: operation applied successfully
   Injected: oo2YhBbAY8Vr

::

   # octez-client transfer 10
   Operation receipt: operation applied successfully
   Injected: opJFLuHR98tf

The operations are known to the node, they appear as *applied* in the
node mempool.

::

   # octez-client rpc get /chains/main/mempool/pending_operations
   { "applied":
       [ { "hash": "op45sL79jASRf41kpL5NDDbAUnQeTfwgZpVnZi1sXy4Cj5x18m9",
           "branch": "BLa7SnHxjHqPTsGSE2fi8sHBm39u9g6Psd9qPZm4rJCqhzHdkSp",
           "IncrA": {} },
         { "hash": "opV6ZMR2z2ZZUSjetzPTknPisjN6x5eFCQnAWhuKNic6GsiRLW7",
           "branch": "BLa7SnHxjHqPTsGSE2fi8sHBm39u9g6Psd9qPZm4rJCqhzHdkSp",
           "IncrB": {} },
         { "hash": "oo1a3gwKnXFqaHuhpgMb5x69wm3mwbid4hn8Ry1iX8jbvXzQQs7",
           "branch": "BLa7SnHxjHqPTsGSE2fi8sHBm39u9g6Psd9qPZm4rJCqhzHdkSp",
           "Transfer": 10 } ], "refused": [], "branch_refused": [],
     "branch_delayed": [], "unprocessed": [] }

We bake the third block.

::

   octez-client bake '"This is block 3"'
   Injected block BLz4SrcTnBQU

We can see now that the three operations appear in the ``operations``
section of the block, encoded as specified by
``Proto_operation.encoding``. The receipt for each operation also
appears in this section.

::

   octez-client rpc get /chains/main/blocks/head/
   { "protocol": "ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT",
     "chain_id": "NetXdQprcVkpaWU",
     "hash": "BLz4SrcTnBQUiXXks1FTzGR9d5MsX6F1mhZ2g23bHTcaQwJbk3S",
     "header":
       { "level": 3, "proto": 1,
         "predecessor": "BLrQqbn13VrbzUprxQypzAg6fc7YmsHaZvGwGrHJk8a4eG6e11B",
         "timestamp": "2019-07-05T14:30:38Z", "validation_pass": 1,
         "operations_hash":
           "LLoZctr62cmk2pvVu2dqX5nv8rA7PHi3xRNGe6mbqmtAQPKtwHuKK",
         "fitness": [ "01", "0000000000000003" ],
         "context": "CoVXzytYqZcw4RQknAZJpK3RAFeLrzcZG2zDMdzuacpDPjX7YSor",
         "demo_block_header_data": "This is block 3" },
     "metadata":
       { "protocol": "ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT",
         "next_protocol": "ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT",
         "test_chain_status": { "status": "not_running" },
         "max_operations_ttl": 0, "max_operation_data_length": 100,
         "max_block_header_length": 100,
         "max_operation_list_length": [ { "max_size": 1000 } ], "demo_a": 91,
         "demo_b": 111 },
     "operations":
       [ [ { "protocol": "ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT",
             "chain_id": "NetXdQprcVkpaWU",
             "hash": "op5gBsE7EMi7gsR3xtSMMQms9XN8Pka5N1pT8XGuN1iP2siizkx",
             "branch": "BLrQqbn13VrbzUprxQypzAg6fc7YmsHaZvGwGrHJk8a4eG6e11B",
             "data": { "IncrA": {} },
             "receipt":
               { "demo_operation_receipt": "operation applied successfully" } },
           { "protocol": "ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT",
             "chain_id": "NetXdQprcVkpaWU",
             "hash": "oo2YhBbAY8Vr2ASXj5k3PggXoxDVYWAQGvK1Sm22GhPFcXdvjQq",
             "branch": "BLrQqbn13VrbzUprxQypzAg6fc7YmsHaZvGwGrHJk8a4eG6e11B",
             "data": { "IncrB": {} },
             "receipt":
               { "demo_operation_receipt": "operation applied successfully" } },
           { "protocol": "ProtoDemoCounterDemoCounterDemoCounterDemoCou4LSpdT",
             "chain_id": "NetXdQprcVkpaWU",
             "hash": "opJFLuHR98tfTTXGhsnMMjg6KxgVW8qx7LZ5q2nrhoySXySqZSS",
             "branch": "BLrQqbn13VrbzUprxQypzAg6fc7YmsHaZvGwGrHJk8a4eG6e11B",
             "data": { "Transfer": 10 },
             "receipt":
               { "demo_operation_receipt": "operation applied successfully" } } ] ] }

We can finally test our two RPCs to query the counter values.

::

   octez-client rpc get /chains/main/blocks/head/counter/a
   91

::

   octez-client rpc get /chains/main/blocks/head/counter/b
   111

The node’s trace is similar to the one presented in the previous
part. What we see in addition are three chunks of output of the form:

::

   Nov 24 10:19:15.108 NOTICE │ demo-counter: begin_validation (partial_construction mode): pred_fitness = ff::00::00::00::0000000000000002  constructed fitness = ff::00::00::00::0000000000000003
   Nov 24 10:19:15.108 NOTICE │ demo-counter: begin_application (partial_construction mode): pred_fitness = ff::00::00::00::0000000000000002  constructed fitness = ff::00::00::00::0000000000000003
   Nov 24 10:19:15.108 NOTICE │ demo-counter: apply_operation
   Nov 24 10:19:15.109 NOTICE │ operation
   Nov 24 10:19:15.109 NOTICE │   opBuK7iL6HTasNSgWB6gaTax5et1FyAAjY3piLqeDy4UfvkKdH9
   Nov 24 10:19:15.109 NOTICE │   injected

   Nov 24 10:20:39.792 NOTICE │ demo-counter: begin_validation (construction mode): pred_fitness = ff::00::00::00::0000000000000002  constructed fitness = ff::00::00::00::0000000000000003
   Nov 24 10:20:39.792 NOTICE │ block at level 3 successfully pre-applied in
   Nov 24 10:20:39.792 NOTICE │   371us

The first 6 lines correspond to the call
``Demo_block_services.Helpers.Preapply.operations cctxt [op]`` and the
next 3 lines to the call ``Shell_services.Injection.operation`` (both
calls triggered by ``Client_proto_commands.inject_op``). Finally, when
the last block is created, we see the following output:

::

   Nov 24 10:20:39.792 NOTICE │ demo-counter: begin_application (construction mode): pred_fitness = ff::00::00::00::0000000000000002  constructed fitness = ff::00::00::00::0000000000000003
   Nov 24 10:20:39.792 NOTICE │ demo-counter: validate_operation
   Nov 24 10:20:39.792 NOTICE │ demo-counter: apply_operation
   Nov 24 10:20:39.792 NOTICE │ demo-counter: validate_operation
   Nov 24 10:20:39.792 NOTICE │ demo-counter: apply_operation
   Nov 24 10:20:39.792 NOTICE │ demo-counter: validate_operation
   Nov 24 10:20:39.792 NOTICE │ demo-counter: apply_operation
   Nov 24 10:20:39.792 NOTICE │ demo-counter: finalize_validation: fitness = ff::00::00::00::0000000000000003
   Nov 24 10:20:39.792 NOTICE │ demo-counter: finalize_application: fitness = ff::00::00::00::0000000000000003

   Nov 24 10:20:39.793 NOTICE │ demo-counter: validate_operation
   Nov 24 10:20:39.793 NOTICE │ demo-counter: begin_validation (application mode): pred_fitness = ff::00::00::00::0000000000000002  block_fitness = ff::00::00::00::0000000000000003
   Nov 24 10:20:39.793 NOTICE │ demo-counter: validate_operation
   Nov 24 10:20:39.793 NOTICE │ demo-counter: validate_operation
   Nov 24 10:20:39.793 NOTICE │ demo-counter: finalize_validation: fitness = ff::00::00::00::0000000000000003
   Nov 24 10:20:39.795 NOTICE │ demo-counter: Mempool.init: head fitness = ff::00::00::00::0000000000000003
   Nov 24 10:20:39.795 NOTICE │ head is now
   Nov 24 10:20:39.795 NOTICE │   BLcidYLQJfMVqrp2x7GdxVwDdrhzy9reKo5FjNk6jNiMHPMTjSp
   Nov 24 10:20:39.795 NOTICE │   (3)

Here again, the chunks can be seen as being composed of two parts, one
for ``Demo_block_services.Helpers.Preapply.block`` and one for
``Shell_services.Injection.block``. In each of the parts
``apply_operation`` is called three times, once for each operation
included in the block.

This scenario can be reproduced using the following Tezt test.
It launches a node, and runs the client commands to activate the
protocol and interact with it.

::

   dune exec tezt/tests/main.exe -- --title demo_counter

If you want to have more detailed traces of the node, you should indicate a data directory when running the sandboxed node, like this::

   DATA_DIR='/tmp/tz-data' ./src/bin_node/octez-sandboxed-node.sh 1 --connections 0


Then the node trace appears in ``/tmp/tz-data/daily_logs/daily-20251124.log``.

Mempool module
==============

Since environment V7 (protocol Lima, 2022), the ``PROTOCOL`` signature
requires protocols to implement a ``Mempool`` module. This module
provides the shell with a protocol-level API for managing the mempool —
the set of pending operations that have been received but not yet
included in a block. See :doc:`../shell/prevalidation` for more details
on how the shell uses this module.

For ``demo_counter``, the mempool state type ``t`` is simply
``State.t`` — the current counter values. When an operation is added to
the mempool via ``add_operation``, it is applied to this state using
``Apply.apply`` to verify validity. This means the mempool can reject
invalid operations (e.g. a transfer that would make a counter negative)
before they reach block construction.

Key functions:

- ``Mempool.init`` initializes the mempool from the current context by
  reading the state via ``State.get_state``.
- ``Mempool.add_operation`` validates an operation against the current
  mempool state and adds it if valid.
- ``Mempool.add_valid_operation`` adds a previously validated operation
  to the mempool (a synchronous variant).

For ``demo_noops``, which has no operations, the ``Mempool`` module is a
trivial implementation where all types are ``unit`` and all functions
are no-ops or return errors.

Note that ``remove_operation``, ``merge``, and ``operations`` are not
used by the demo protocols and are stubbed out. A production protocol
would need full implementations.

Conclusion
==========

We presented a simple protocol ``demo_counter`` which explores further
the interface between the shell and the protocol, and uses more features
available to the protocol developer such as RPC services, the mempool
module, and the split validation/application model. Besides, this
protocol comes with a library that extends ``octez-client`` with new
commands to interact with the protocol.
