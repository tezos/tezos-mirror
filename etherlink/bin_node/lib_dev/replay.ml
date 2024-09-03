(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let patch_kernel ~kernel_path evm_state =
  let open Lwt_result_syntax in
  let* content, binary = Wasm_debugger.read_kernel_from_file kernel_path in
  let*! kernel =
    if binary then Lwt.return content else Wasm_utils.wat2wasm content
  in
  let*! evm_state =
    Evm_state.modify ~key:"/kernel/boot.wasm" ~value:kernel evm_state
  in
  return evm_state

let main ?profile ?kernel_path ~data_dir ~preimages ~preimages_endpoint number =
  let open Lwt_result_syntax in
  let* ro_ctxt =
    Evm_ro_context.load ~data_dir ~preimages ?preimages_endpoint ()
  in
  let alter_evm_state =
    match kernel_path with
    | None -> None
    | Some kernel_path -> Some (patch_kernel ~kernel_path)
  in
  let* apply_result =
    Evm_ro_context.replay ro_ctxt ?profile ?alter_evm_state number
  in
  match apply_result with
  | Apply_success {block; _} ->
      Format.printf
        "Replaying blueprint %a led to block %a\n%!"
        Ethereum_types.pp_quantity
        number
        Ethereum_types.pp_block_hash
        block.hash ;
      return_unit
  | Apply_failure ->
      failwith "Could not replay blueprint %a" Ethereum_types.pp_quantity number
