(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let read_kernel_from_file kernel_path =
  let open Lwt_result_syntax in
  if Filename.(check_suffix kernel_path ".hex") then
    let*! content = Repl_helpers.read_file kernel_path in
    let*? content =
      match Hex.to_string (`Hex content) with
      | Some content -> Ok (content, true)
      | None -> error_with "%S is not a valid hexadecimal file" kernel_path
    in
    return content
  else
    let*! content = Repl_helpers.read_file kernel_path in
    let*? binary =
      if Filename.check_suffix kernel_path ".wasm" then Ok true
      else if Filename.check_suffix kernel_path ".wast" then Ok false
      else error_with "Kernels should have .wasm or .wast file extension"
    in
    return (content, binary)

let patch_kernel ~kernel_path evm_state =
  let open Lwt_result_syntax in
  let* content, binary = read_kernel_from_file kernel_path in
  let*! kernel =
    if binary then Lwt.return content else Wasm_utils.wat2wasm content
  in
  let*! evm_state =
    Evm_state.modify ~key:"/kernel/boot.wasm" ~value:kernel evm_state
  in
  return evm_state

let main ?profile ?kernel_path ~data_dir ~preimages ~preimages_endpoint
    ?smart_rollup_address number =
  let open Lwt_result_syntax in
  let* _init =
    Evm_context.start
      ~data_dir
      ~preimages
      ~preimages_endpoint
      ~fail_on_missing_blueprint:false
      ?smart_rollup_address
      ~sqlite_journal_mode:`Identity
      ~store_perm:`Read_only
      ()
  in
  let alter_evm_state =
    match kernel_path with
    | None -> None
    | Some kernel_path -> Some (patch_kernel ~kernel_path)
  in
  let* apply_result = Evm_context.replay ?profile ?alter_evm_state number in
  match apply_result with
  | Apply_success {block_hash = hash; _} ->
      Format.printf
        "Replaying blueprint %a led to block %a\n%!"
        Ethereum_types.pp_quantity
        number
        Ethereum_types.pp_block_hash
        hash ;
      return_unit
  | Apply_failure ->
      failwith "Could not replay blueprint %a" Ethereum_types.pp_quantity number

let rpc block_number =
  let open Lwt_result_syntax in
  let* apply_result = Evm_context.replay block_number in
  match apply_result with
  | Apply_failure ->
      failwith
        "Could not replay block %a"
        Ethereum_types.pp_quantity
        block_number
  | Apply_success {block_hash; evm_state; _} -> (
      let path = Durable_storage_path.Block.by_hash block_hash in
      let*! bytes = Evm_state.inspect evm_state path in
      match bytes with
      | None -> failwith "Replayed block not found in the storage"
      | Some b -> return @@ Ethereum_types.block_from_rlp b)
