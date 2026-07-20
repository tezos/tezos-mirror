(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Requirement:  make -f etherlink.mk build
                 make octez-node octez-client octez-smart-rollup-node octez-evm-node
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file faults.ml
   Subject:      Exercise the kernel's behaviour when an operation
                 deliberately triggers an unrecoverable WASM trap, submitted
                 directly or through the delayed inbox, and the operator's
                 recovery from a delayed one.
 *)

open Test_helpers
open Rpc.Syntax

type fault = Panic | Stack_overflow

let string_of_fault = function
  | Panic -> "panic"
  | Stack_overflow -> "stack_overflow"

(** [craft_tx ~fault ~sequencer ~nonce] builds a signed transaction from the
    first bootstrap account that calls the [fault] method of the debug
    precompile. *)
let craft_tx ~fault ~sequencer ~nonce =
  let*@ chain_id = Rpc.get_chain_id sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  Cast.craft_tx
    ~source_private_key:sender.private_key
    ~chain_id
    ~nonce
    ~value:Wei.zero
    ~gas:1_000_000
    ~gas_price:1_000_000_000
    ~address:Solidity_contracts.Precompile.panic
    ~signature:(sf "%s()" (string_of_fault fault))
    ()

module Self_tests = struct
  (* When the [enable_debug_precompiles] flag is left unset, the precompile is
   not registered: its address is a plain, code-less account. Calling
   [panic()] is therefore an inert no-op that is included successfully,
   instead of trapping. *)
  let test_panic_precompile_absent () =
    register_sandbox
      ~__FILE__
      ~tags:["evm"; "precompile"; "panic"; "fault"; "debug"]
      ~title:"Panic debug precompile is inert when the flag is unset"
    @@ fun sequencer ->
    let* raw_tx = craft_tx ~sequencer ~nonce:0 ~fault:Panic in
    let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
    let*@ _ = Rpc.produce_block sequencer in
    let* receipt =
      wait_for_transaction_receipt
        ~evm_node:sequencer
        ~transaction_hash:tx_hash
        ()
    in
    Check.is_true
      receipt.status
      ~error_msg:
        "Expected the panic() call to be included as a plain no-op, but the \
         transaction failed" ;
    unit

  (* With [enable_debug_precompiles] set, the precompile is live. The transaction
   queue only validates transactions (it does not execute the callee), so the
   [panic()] transaction is accepted without ever being simulated. Executing it
   at block production reaches the [unreachable] trap, which is unrecoverable
   and takes the whole sequencer process down. *)
  let test_panic_precompile_crashes_sequencer () =
    register_sandbox
      ~__FILE__
      ~enable_debug_precompiles:true
        (* The private RPC to produce blocks will *not* terminate the node,
           this only happens when the sequencer loop is receiving the error
           from the block producer. *)
      ~time_between_blocks:6.0
      ~tags:["evm"; "precompile"; "panic"; "fault"; "debug"]
      ~title:"Panic debug precompile crashes the sequencer at block production"
    @@ fun sequencer ->
    let* raw_tx = craft_tx ~sequencer ~nonce:0 ~fault:Panic in
    (* Wait for the transaction to reach the queue before producing the block,
     so the block that triggers the crash actually contains it. *)
    let added =
      Evm_node.wait_for_tx_queue_add_transaction ~timeout:30. sequencer
    in
    let terminated = Evm_node.wait_termination sequencer in
    let* _ = Rpc.send_raw_transaction ~raw_tx sequencer in
    let* (_ : string) = added in
    (* Producing the block runs [panic()] and crashes the kernel. The RPC call
     fails as the sequencer's connection drops, which we tolerate. *)
    Evm_node.resolve_or_timeout
      ~timeout:60.
      sequencer
      ~name:"sequencer crash on panic precompile"
      terminated

  let register () =
    test_panic_precompile_absent () ;
    test_panic_precompile_crashes_sequencer ()
end

(* Wrap [raw_tx] and post it to the L1 delayed transaction bridge, then bake an
   L1 level so the rollup node -- and thus the sequencer's follower -- pick it
   up. *)
let send_raw_tx_to_delayed_inbox ~sc_rollup_node ~client ~l1_contracts
    ~sc_rollup_address raw_tx =
  let* () =
    Client.transfer
      ~arg:(sf "Pair %S 0x%s" sc_rollup_address raw_tx)
      ~amount:Tez.one
      ~giver:Constant.bootstrap2.public_key_hash
      ~receiver:l1_contracts.Setup.delayed_transaction_bridge
      ~burn_cap:Tez.one
      client
  in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  unit

(* A faulty transaction reaching the block producer through the delayed inbox
   takes the sequencer down, exactly like one submitted directly. This
   exercises the operator's recovery path: manually flushing the delayed inbox
   with [--force] publishes a blueprint the kernel drops on the trap,
   unblocking the rollup node so the sequencer can be restarted and able to
   produce blocks again.

   The [Stack_overflow] variant additionally pins the call-depth guard injected
   by [smart-rollup-instrument]: its recursion spills only a scalar per frame,
   so the host stack — not the kernel's 1 MiB shadow stack — is what runs out
   first. Without the guard that is not a WASM trap, the PVM has nothing to
   absorb, and it goes Stuck: the flush never drains the delayed inbox and this
   test times out. See [stack_overflow] in the debug precompile. *)
let test_recover_crashing_delayed_transaction fault =
  Setup.register_test
    ~__FILE__
    ~time_between_blocks:Nothing
    ~kernel:Kernel.Latest
    ~enable_dal:false
    ~enable_debug_precompiles:true
    ~da_fee:Wei.zero
    ~tags:
      [
        "evm";
        "precompile";
        string_of_fault fault;
        "fault";
        "debug";
        "delayed_inbox";
      ]
    ~title:
      (sf
         "Recover from a delayed %s() transaction with a forced flush"
         (string_of_fault fault))
  @@
  fun {sequencer; sc_rollup_node; client; l1_contracts; sc_rollup_address; _}
      _protocol
    ->
  (* Submit the faulty transaction through the delayed inbox. *)
  let* raw_tx = craft_tx ~fault ~sequencer ~nonce:0 in
  let* () =
    send_raw_tx_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_tx
  in
  (* Advance the layer 1 to make sure the sequencer fetches the delayed item. *)
  let* () =
    repeat 3 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  (* This should fail *)
  let*@? _ = Rpc.produce_block ~with_delayed_transactions:true sequencer in
  (* While the sequencer is down, flush the delayed inbox with [--force]: the
     blueprint is published even though applying it traps. *)
  let* () = Evm_node.terminate sequencer in
  let*! () =
    Evm_node.flush_delayed_inbox
      ~wallet_dir:(Client.base_dir client)
      ~force:true
      sequencer
  in
  (* Bake until the rollup node has processed the forced blueprint and its
     delayed inbox is empty again. *)
  let* () =
    bake_until
      ~timeout:120.
      ~timeout_in_blocks:20
      ~bake:(fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
      ~result_f:(fun () ->
        let* size = Delayed_inbox.size (Sc_rollup_node sc_rollup_node) in
        if size = 0 then return (Some ()) else return None)
      ()
  in

  (* Restart the sequencer to create a block *)
  let* () = Evm_node.run sequencer in
  (* Advance the layer 1 to make sure the sequencer fetches the dropped
     transaction event. *)
  let* l = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let wait_for =
    Evm_node.wait_for_processed_l1_level ~level:(l + 2) sequencer
  in
  let* () =
    repeat 4 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* _ = wait_for in

  let*@ _ = Rpc.produce_block sequencer in

  bake_until_sync ~sc_rollup_node ~sequencer ~client ()

(** [craft_tezlink_call ~sc_rollup_node ~client ~sequencer ~source ~counter
    ?dest ?amount ?entrypoint ?arg_data ()] returns a Tezos X (Michelson
    runtime) manager operation from [source] at [counter], holding a single
    transaction that calls [dest] with [amount], [entrypoint] and the Michelson
    argument [arg_data]. Fee, gas and storage limits are fixed; the operation is
    branched on the sequencer's latest Tezos X block. *)
let craft_tezlink_call ~sc_rollup_node ~client ~sequencer ~source ~counter ?dest
    ?amount ?entrypoint ?arg_data () =
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let tezlink_endpoint = tezlink_foreign_endpoint sequencer in
  let* branch = RPC_core.call tezlink_endpoint @@ RPC.get_chain_block_hash () in
  let* arg =
    match arg_data with
    | None -> Lwt.return_none
    | Some data ->
        Client.convert_data_to_json ~data client |> Lwt.map Option.some
  in
  Operation.Manager.(
    operation
      ~branch
      [
        make
          ~fee:1_000_000
          ~counter
          ~gas_limit:660_000
          ~storage_limit:1000
          ~source
          (call ?dest ?amount ?entrypoint ?arg ());
      ])
    client

(** [test_crashing_michelson_at_sequencer ~name ~tags ~poison_code]
    registers a Tezt test that originates the [poison_code] contract,
    submits a call to it directly to the sequencer, and checks that
    producing the block fails. *)
let test_crashing_michelson_at_sequencer ~name ~tags ~poison_code =
  let title = sf "%s traps the sequencer (Michelson runtime)" name in
  Setup.register_test
    ~__FILE__
    ~rpc_server:Evm_node.Resto
    ~time_between_blocks:Nothing
    ~kernel:Kernel.Latest
    ~with_runtimes:[Tezosx_runtime.Tezos]
    ~enable_dal:false
    ~da_fee:Wei.zero
    ~tags:(["evm"; "michelson"; "fault"; "sequencer"; "oom"] @ tags)
    ~title
  @@ fun {sequencer; sc_rollup_node; client; _} _protocol ->
  let source = Constant.bootstrap5 in
  (* Originate the poison contract (origination only typechecks the code). *)
  let* tez_client = tezlink_client sequencer in
  let* dest =
    Client.originate_contract
      ~alias:"poison"
      ~amount:Tez.zero
      ~src:source.alias
      ~prg:poison_code
      ~init:"Unit"
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sequencer in
  let* call_op =
    craft_tezlink_call
      ~sc_rollup_node
      ~client
      ~sequencer
      ~source
      ~counter:2
      ~dest
      ~arg_data:"Unit"
      ()
  in
  (* The queue only validates transactions (it does not execute the callee), so
     the call is accepted without ever being simulated. *)
  let added =
    Evm_node.wait_for_tx_queue_add_transaction ~timeout:30. sequencer
  in
  let* (`OpHash _) = Operation.inject ~dont_wait:true call_op tez_client in
  let* (_ : string) = added in
  let*@? _ = Rpc.produce_block sequencer in
  unit

(** [test_recover_crashing_delayed_michelson ~name ~tags ~poison_code]
    registers a Tezt test that originates the [poison_code] contract,
    submits a call to it through the delayed inbox, checks that
    force-including that call fails block production, then flushes the
    delayed inbox and checks the sequencer produces blocks again. *)
let test_recover_crashing_delayed_michelson ~name ~tags ~poison_code =
  let title = sf "Recover from a delayed %s (Michelson runtime)" name in
  Setup.register_test
    ~__FILE__
    ~rpc_server:Evm_node.Resto
    ~time_between_blocks:Nothing
    ~kernel:Kernel.Latest
    ~with_runtimes:[Tezosx_runtime.Tezos]
    ~enable_dal:false
    ~da_fee:Wei.zero
    ~tags:(["evm"; "michelson"; "fault"; "delayed_inbox"; "oom"] @ tags)
    ~title
  @@
  fun {sequencer; sc_rollup_node; client; l1_contracts; sc_rollup_address; _}
      _protocol
    ->
  let source = Constant.bootstrap5 in
  (* Originate the poison contract (origination only typechecks the code). *)
  let* tez_client = tezlink_client sequencer in
  let* dest =
    Client.originate_contract
      ~alias:"poison"
      ~amount:Tez.zero
      ~src:source.alias
      ~prg:poison_code
      ~init:"Unit"
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sequencer in
  (* Submit a call to the poison contract through the delayed inbox. *)
  let* call_op =
    craft_tezlink_call
      ~sc_rollup_node
      ~client
      ~sequencer
      ~source
      ~counter:2
      ~dest
      ~arg_data:"Unit"
      ()
  in
  let* _hash =
    Delayed_inbox.send_tezos_operation_to_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~tezosx_format:true
      call_op
  in
  (* Advance the layer 1 to make sure the sequencer fetches the delayed item. *)
  let* () =
    repeat 3 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  (* This should fail *)
  let*@? _ = Rpc.produce_block ~with_delayed_transactions:true sequencer in
  (* While the sequencer is down, flush the delayed inbox with [--force]: the
     blueprint is published even though applying it traps. *)
  let* () = Evm_node.terminate sequencer in
  let*! () =
    Evm_node.flush_delayed_inbox
      ~wallet_dir:(Client.base_dir client)
      ~force:true
      sequencer
  in
  (* Bake until the rollup node has processed the forced blueprint and its
     delayed inbox is empty again. *)
  let* () =
    bake_until
      ~timeout:120.
      ~timeout_in_blocks:20
      ~bake:(fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
      ~result_f:(fun () ->
        let* size = Delayed_inbox.size (Sc_rollup_node sc_rollup_node) in
        if size = 0 then return (Some ()) else return None)
      ()
  in
  (* Restart the sequencer to create a block *)
  let* () = Evm_node.run sequencer in
  (* Advance the layer 1 to make sure the sequencer fetches the dropped
     transaction event. *)
  let* l = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let wait_for =
    Evm_node.wait_for_processed_l1_level ~level:(l + 2) sequencer
  in
  let* () =
    repeat 4 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* _ = wait_for in
  let*@ _ = Rpc.produce_block sequencer in
  bake_until_sync ~sc_rollup_node ~sequencer ~client ()

(** [test_crashing_oversized_allocation] registers the sequencer-crash and
    delayed-recovery tests for a contract that builds a value larger than the
    maximum size allocatable on the kernel's wasm32 target (2^31 - 1 bytes). *)
let test_crashing_oversized_allocation protos =
  let name = "oversized allocation" in
  let tags = ["concat"] in
  let poison_code =
    {|
parameter unit ;
storage unit ;
code {
       DROP ;
       # Build a 2^20-byte chunk (a 1-byte seed doubled 20 times).
       PUSH bytes 0x00 ; PUSH int 20 ; DUP ; GT ; LOOP { PUSH int 1 ; SWAP ; SUB ; DIP { DUP ; CONCAT } ; DUP ; GT } ; DROP ;
       # Build a list of 2048 copies of the chunk.
       NIL bytes ; PUSH int 2048 ; DUP ; GT ; LOOP { PUSH int 1 ; SWAP ; SUB ; DIP { DUP 2 ; CONS } ; DUP ; GT } ; DROP ;
       DIP { DROP } ;
       # CONCAT materialises 2048 * 2^20 = 2^31 > isize::MAX -> capacity-overflow panic.
       CONCAT ;
       DROP ; UNIT ; NIL operation ; PAIR
     }
|}
  in
  test_recover_crashing_delayed_michelson ~name ~tags ~poison_code protos ;
  test_crashing_michelson_at_sequencer ~name ~tags ~poison_code protos

(** [test_crashing_heap_exhaustion] registers the sequencer-crash and
    delayed-recovery tests for a contract that allocates more memory than the
    kernel's wasm32 memory can hold (4 GiB heap). *)
let test_crashing_heap_exhaustion protos =
  let name = "heap-exhausting operation" in
  let tags = ["and"] in
  let poison_code =
    {|
parameter unit ;
storage unit ;
code {
       DROP ;
       # Build a 2^20-byte chunk (a 1-byte seed doubled 20 times).
       PUSH bytes 0x00 ; PUSH int 20 ; DUP ; GT ; LOOP { PUSH int 1 ; SWAP ; SUB ; DIP { DUP ; CONCAT } ; DUP ; GT } ; DROP ;
       # Concatenate 1536 copies of the chunk into a ~1.5 GiB value.
       NIL bytes ; PUSH int 1536 ; DUP ; GT ; LOOP { PUSH int 1 ; SWAP ; SUB ; DIP { DUP 2 ; CONS } ; DUP ; GT } ; DROP ; CONCAT ;
       DIP { DROP } ;
       # AND clones both operands of the value DUP'd twice: 3 * 1.5 GiB > 4 GiB -> OOM.
       DUP ; DUP ;
       AND ;
       DROP ; DROP ; UNIT ; NIL operation ; PAIR
     }
|}
  in
  test_recover_crashing_delayed_michelson ~name ~tags ~poison_code protos ;
  test_crashing_michelson_at_sequencer ~name ~tags ~poison_code protos

let () =
  Self_tests.register () ;
  List.iter
    (fun fault -> test_recover_crashing_delayed_transaction fault [Alpha])
    [Panic; Stack_overflow] ;
  test_crashing_oversized_allocation [Alpha] ;
  test_crashing_heap_exhaustion [Alpha]
