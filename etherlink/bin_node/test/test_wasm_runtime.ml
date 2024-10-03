(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    EVM Node’s WASM Runtime
   Invocation:   dune exec -- etherlink/bin_node/test/main.exe wasm_runtime
*)

open Evm_node_lib_dev

let register ?(tags = []) =
  Test.register
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
    ~__FILE__
    ~tags:("wasm_runtime" :: tags)

let context_hash_typ = Check.(convert Context_hash.to_b58check string)

let empty_tree = Irmin_context.PVMState.empty ()

let test_wasm_runtime_id () =
  register ~title:"test wasm_runtime_run" @@ fun () ->
  let hash = Irmin_context.Tree.hash empty_tree in
  let* empty_tree_id = Wasm_runtime.run empty_tree in
  let hash_id = Irmin_context.Tree.hash empty_tree_id in
  Check.((hash = hash_id) context_hash_typ ~error_msg:"Hash should be the same") ;
  unit

let () = test_wasm_runtime_id ()
