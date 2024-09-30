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

let level_verbosity = function
  | Events.Debug -> 3
  | Info -> 2
  | Error -> 1
  | Fatal -> 0

let patch_verbosity ~kernel_verbosity evm_state =
  let open Lwt_result_syntax in
  let value = Bytes.make 1 '\000' in
  Bytes.set_uint8 value 0 (level_verbosity kernel_verbosity) ;
  let*! evm_state =
    Evm_state.modify
      ~key:"/evm/logging_verbosity"
      ~value:(Bytes.to_string value)
      evm_state
  in
  return evm_state

let alter_evm_state ~kernel_path ~kernel_verbosity evm_state =
  let open Lwt_result_syntax in
  let* evm_state =
    match kernel_path with
    | None -> return evm_state
    | Some kernel_path -> patch_kernel ~kernel_path evm_state
  in
  match kernel_verbosity with
  | None -> return evm_state
  | Some kernel_verbosity -> patch_verbosity ~kernel_verbosity evm_state

let main ?profile ?kernel_path ?kernel_verbosity ~data_dir ~preimages
    ~preimages_endpoint number =
  let open Lwt_result_syntax in
  let* ro_ctxt =
    Evm_ro_context.load
      ~data_dir
      ~preimages
      ?preimages_endpoint
      ~finalized_view:
        (* The block parameter won’t be used so this parameter is effectively
           ignored during a replay *)
        false
      ()
  in
  let alter_evm_state = alter_evm_state ~kernel_path ~kernel_verbosity in
  let* apply_result =
    Evm_ro_context.replay ro_ctxt ?profile ~alter_evm_state number
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
