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

let () = Self_tests.register ()
