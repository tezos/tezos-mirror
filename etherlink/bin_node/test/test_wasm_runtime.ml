(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    EVM Node’s WASM Runtime
   Invocation:   cd etherlink/bin_node/test/ ; dune exec -- ./test_wasm_runtime.exe
*)

open Evm_node_lib_dev

let register ?(tags = []) =
  Test.register
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
    ~__FILE__
    ~tags:("wasm_runtime" :: tags)

let z_typ = Check.(convert Z.to_int64 int64)

let expect_ok msg = function Ok x -> x | Error _err -> Test.fail msg

let pool = Lwt_domain.setup_pool 1

let test_wasm_runtime_id () =
  register ~title:"test wasm_runtime_run" @@ fun () ->
  Log.info "Initialize the tree" ;
  let* tree =
    Evm_state.init
      ~kernel:
        (On_disk
           "../../kernel_latest/kernel/tests/resources/mainnet_kernel.wasm")
  in
  let tree = expect_ok "Should be able to create an initial state" tree in
  let tree = Pvm.Wasm_internal.to_irmin_exn tree in
  Log.info "Run kernel_run" ;
  let* tree =
    Wasm_runtime.run
      ~pool
      ~preimages_dir:Temp.(dir "wasm_2_0_0")
      ~entrypoint:"kernel_run"
      ~native_execution:true
      tree
      Tezos_crypto.Hashed.Smart_rollup_address.zero
      (`Inbox [])
  in
  let tree = Pvm.Wasm_internal.of_irmin tree in
  Log.info "Check state consistency" ;
  let* quantity =
    Etherlink_durable_storage.current_block_number (fun key ->
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

let () = Test.run ()
