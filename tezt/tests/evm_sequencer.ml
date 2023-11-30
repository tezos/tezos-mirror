(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Sc_rollup_helpers

let setup_sequencer ?(bootstrap_accounts = Eth_account.bootstrap_accounts) () =
  let preimages_dir = Temp.dir "preimages" in
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
  let mode =
    Evm_node.Sequencer {kernel = output; preimage_dir = preimages_dir}
  in
  Evm_node.init ~mode ~devmode:false "0.0.0.0:0"

let test_persistent_state =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"]
    ~title:"Sequencer state is persistent across runs"
  @@ fun _protocol ->
  let* evm_node = setup_sequencer () in
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
