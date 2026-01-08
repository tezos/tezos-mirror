(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let enable ~keep_alive ~timeout ?evm_node_endpoint store =
  let get_block_from_store n =
    Evm_store.(
      use store @@ fun conn ->
      Blocks.find_with_level ~full_transaction_object:false conn n)
  in
  let get_block_from_endpoint evm_node_endpoint n =
    Batch.call
      (module Rpc_encodings.Get_block_by_number)
      ~keep_alive
      ~timeout
      ~evm_node_endpoint
      (Ethereum_types.Block_parameter.(Number n), false)
  in
  let get_block (n : Ethereum_types.quantity) =
    let (Qty nz) = n in
    let open Lwt_result_syntax in
    let* from_store =
      trace
        (error_of_fmt
           "Failed when trying to fetch block %a from the store"
           Z.pp_print
           nz)
      @@ get_block_from_store n
    in
    match (from_store, evm_node_endpoint) with
    | Some block, _ ->
        (* We do not need the transactions, and the block returned from the
           store and the EVM node endpoint are not compatible. *)
        return {block with transactions = TxHash []}
    | None, Some evm_node_endpoint ->
        let+ block =
          trace
            (error_of_fmt
               "Failed when trying to fetch block %a from the upstream endpoint"
               Z.pp_print
               nz)
          @@ get_block_from_endpoint evm_node_endpoint n
        in
        (* We do not need the transactions, and the block returned from the
           store and the EVM node endpoint are not compatible. *)
        {block with transactions = TxHash []}
    | None, None ->
        failwith
          "Block %a was missing from the context and upstream endpoint"
          Z.pp_print
          nz
  in
  let get_block_exn n =
    let open Lwt_syntax in
    let* maybe_block = get_block n in
    match maybe_block with
    | Ok block -> return block
    | Error trace ->
        Stdlib.failwith Format.(asprintf "%a" Error_monad.pp_print_trace trace)
  in
  let tmp_path = "/tmp" in
  let world_state_path = "/evm/world_state" in
  let tmp_world_state_path = tmp_path ^ world_state_path in
  let store_get_hash tree key =
    let open Lwt_syntax in
    let evm_node_state = Pvm.Wasm_internal.of_irmin tree in
    let enable_fallback =
      match
        Lwt_domain.run_in_main @@ fun () ->
        Evm_state.storage_version evm_node_state
      with
      | Ok storage_version ->
          Storage_version.kernel_has_txs_in_storage ~storage_version
      | Error _ ->
          (* This should not be possible. Etherlink kernel is designed such
             that it always write its storage version in the durable storage.
             We default to [false] (default behavior) because we cannot make
             any assumption on the running kernel. *)
          false
    in
    if enable_fallback && key = tmp_world_state_path ^ "/transactions_receipts"
    then
      Lwt_domain.run_in_main @@ fun () ->
      let* pred =
        Evm_state.current_block_height
          ~root:Durable_storage_path.etherlink_root
          evm_node_state
      in
      let n = Ethereum_types.Qty.next pred in
      let+ block = get_block_exn n in
      let s = Ethereum_types.hash_to_bytes block.receiptRoot in
      Ok (Bytes.unsafe_of_string s)
    else if
      enable_fallback && key = tmp_world_state_path ^ "/transactions_objects"
    then
      Lwt_domain.run_in_main @@ fun () ->
      let* pred =
        Evm_state.current_block_height
          ~root:Durable_storage_path.etherlink_root
          evm_node_state
      in
      let n = Ethereum_types.Qty.next pred in
      let+ block = get_block_exn n in
      let s = Ethereum_types.hash_to_bytes block.transactionRoot in
      Ok (Bytes.unsafe_of_string s)
    else Wasm_runtime_callbacks.store_get_hash tree key
  in
  Callback.register "layer2_store__store_get_hash" store_get_hash
