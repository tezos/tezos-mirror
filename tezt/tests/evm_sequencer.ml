(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Sc_rollup_helpers

(** Renaming the helper to avoid confusion on its behavior. *)
let next_rollup_node_level = Helpers.next_evm_level

type setup = {
  node : Node.t;
  client : Client.t;
  sc_rollup_node : Sc_rollup_node.t;
  evm_node : Evm_node.t;
}

let setup_sequencer ?(bootstrap_accounts = Eth_account.bootstrap_accounts)
    protocol =
  let* node, client = setup_l1 protocol in
  let sc_rollup_node =
    Sc_rollup_node.create
      ~default_operator:Constant.bootstrap1.public_key_hash
      Batcher
      node
      ~base_dir:(Client.base_dir client)
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
    Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
  in
  let mode =
    Evm_node.Sequencer {kernel = output; preimage_dir = preimages_dir}
  in
  let* evm_node =
    Evm_node.init ~mode (Sc_rollup_node.endpoint sc_rollup_node)
  in
  return {evm_node; node; client; sc_rollup_node}

let test_persistent_state =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"]
    ~title:"Sequencer state is persistent across runs"
  @@ fun protocol ->
  let* {evm_node; _} = setup_sequencer protocol in
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

let test_publish_blueprints =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "data"]
    ~title:"Sequencer publishes the blueprints to L1"
  @@ fun protocol ->
  let* {evm_node; node; client; sc_rollup_node} = setup_sequencer protocol in
  (* Sleep to let the sequencer produce some blocks. *)
  let* () = Lwt_unix.sleep 20. in
  (* Ask for the current block. *)
  let* sequencer_head = Rpc.block_number evm_node in
  (* Stop the EVM node. *)
  let* () = Evm_node.terminate evm_node in

  (* At this point, the evm node should called the batcher endpoint to publish
     all the blueprints. Stopping the node is then not a problem. *)
  let* () =
    repeat 5 (fun () ->
        let* _ = next_rollup_node_level ~node ~client ~sc_rollup_node in
        unit)
  in

  (* Open an EVM node in proxy mode to fetch the rollup node storage. *)
  let proxy_mode = Evm_node.Proxy {devmode = true} in
  let* proxy_evm =
    Evm_node.init ~mode:proxy_mode (Sc_rollup_node.endpoint sc_rollup_node)
  in
  let* rollup_head = Rpc.block_number proxy_evm in
  Check.((sequencer_head = rollup_head) int32)
    ~error_msg:"Expected the same head on the rollup node and the sequencer" ;
  unit

let register ~protocols =
  test_persistent_state protocols ;
  test_publish_blueprints protocols
