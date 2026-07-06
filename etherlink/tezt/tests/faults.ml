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
   Subject:      Exercise the kernel's behaviour when the "panic" debug
                 precompile deliberately triggers unrecoverable WASM traps.
 *)

open Test_helpers
open Rpc.Syntax

(** [craft_panic_tx ~sequencer ~nonce] builds a signed transaction from the
    first bootstrap account that calls [panic()] on the debug precompile. *)
let craft_panic_tx ~sequencer ~nonce =
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
    ~signature:"panic()"
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
    let* raw_tx = craft_panic_tx ~sequencer ~nonce:0 in
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
    let* raw_tx = craft_panic_tx ~sequencer ~nonce:0 in
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

(* A panic() transaction reaching the block producer through the delayed inbox
   takes the sequencer down, exactly like one submitted directly. This
   exercises the operator's recovery path: manually flushing the delayed inbox
   with [--force] publishes a blueprint the kernel drops on the trap, unblocking
   the rollup node. *)
let test_recover_crashing_delayed_transaction =
  Setup.register_test
    ~__FILE__
    ~time_between_blocks:Nothing
    ~kernel:Kernel.Latest
    ~enable_dal:false
    ~enable_debug_precompiles:true
    ~da_fee:Wei.zero
    ~tags:["evm"; "precompile"; "panic"; "fault"; "debug"; "delayed_inbox"]
    ~title:"Recover from a delayed panic() transaction with a forced flush"
  @@
  fun {sequencer; sc_rollup_node; client; l1_contracts; sc_rollup_address; _}
      _protocol
    ->
  (* Submit a panic() transaction through the delayed inbox. *)
  let* raw_tx = craft_panic_tx ~sequencer ~nonce:0 in
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

  unit

let () =
  Self_tests.register () ;
  test_recover_crashing_delayed_transaction [Alpha]
