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

let z_typ = Check.(convert Z.to_int64 int64)

let empty_tree = Irmin_context.PVMState.empty ()

let expect_ok msg = function Ok x -> x | Error _err -> Test.fail msg

let test_wasm_runtime_id () =
  register ~title:"test wasm_runtime_run" @@ fun () ->
  let* tree =
    Evm_state.init
      ~kernel:
        "etherlink/kernel_evm/kernel/tests/resources/mainnet_evm_kernel.wasm"
  in
  let tree = expect_ok "Should be able to create an initial state" tree in
  let* tree =
    Wasm_runtime.run
      ~preimages_dir:Temp.(dir "wasm_2_0_0")
      ~entrypoint:"kernel_run"
      tree
      Tezos_crypto.Hashed.Smart_rollup_address.zero
      []
  in
  let* quantity =
    Durable_storage.current_block_number (fun key ->
        let* candidate = Evm_state.inspect tree key in
        return (Ok candidate))
  in
  let (Qty number) =
    expect_ok "Should be able to read the current number" quantity
  in
  Check.(
    (number = Z.zero) z_typ ~error_msg:"Block number should be %R, but got %L") ;
  unit

let () = test_wasm_runtime_id ()
