(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Sc_rollup_helpers

let setup_sequencer ?(bootstrap_accounts = Eth_account.bootstrap_accounts)
    protocol =
  let* node, client = setup_l1 protocol in
  let sc_rollup_node =
    Sc_rollup_node.create Observer node ~base_dir:(Client.base_dir client)
  in
  let preimages_dir = Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0" in
  let config =
    Configuration.make_config ~bootstrap_accounts ~sequencer:true ()
  in
  let* {output; _} =
    prepare_installer_kernel
      ~base_installee:"./"
      ~preimages_dir
      ?config
      "evm_kernel"
  in
  let* sc_rollup_address =
    originate_sc_rollup
      ~kind:"wasm_2_0_0"
      ~boot_sector:("file:" ^ output)
      ~parameters_ty:"unit"
      client
  in
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup_address ["--log-kernel-debug"]
  in
  let mode =
    Evm_node.Sequencer {kernel = output; preimage_dir = preimages_dir}
  in
  Evm_node.init ~mode (Sc_rollup_node.endpoint sc_rollup_node)

let test_persistent_state =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"]
    ~title:"Sequencer state is persistent across runs"
  @@ fun protocol ->
  let* evm_node = setup_sequencer protocol in
  (* Sleep to let the sequencer produce some blocks. *)
  let* () = Lwt_unix.sleep 20. in
  (* Ask for the current block. *)
  let* block_number = Rpc.block_number evm_node in
  Check.is_true
    ~__LOC__
    (block_number > 0l)
    ~error_msg:"The sequencer should have produced a block" ;
  (* Terminate the sequencer. *)
  let* () = Evm_node.terminate evm_node in
  (* Restart it. *)
  let* () = Evm_node.run evm_node in
  (* Assert the block number is at least [block_number]. Asserting
     that the block number is exactly the same as {!block_number} can
     be flaky if a block is produced between the restart and the
     RPC. *)
  let* new_block_number = Rpc.block_number evm_node in
  Check.is_true
    ~__LOC__
    (new_block_number >= block_number)
    ~error_msg:"The sequencer should have produced a block" ;
  unit

let register ~protocols = test_persistent_state protocols
