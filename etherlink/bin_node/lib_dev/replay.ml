(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let patch_da_fees evm_state =
  Evm_state.modify
    ~key:"/evm/world_state/fees/da_fee_per_byte"
    ~value:(Int.to_string 0)
    evm_state

let patch_kernel ~kernel evm_state =
  let open Lwt_result_syntax in
  let* content, binary = Pvm.Kernel.read_kernel kernel in
  let*! kernel =
    if binary then Lwt.return content else Wasm_utils_functor.wat2wasm content
  in
  let*! evm_state =
    Evm_state.modify ~key:"/kernel/boot.wasm" ~value:kernel evm_state
  in
  return evm_state

let patch_verbosity ~kernel_verbosity evm_state =
  let open Lwt_result_syntax in
  let*! evm_state =
    Evm_state.modify
      ~key:Durable_storage_path.kernel_verbosity
      ~value:(Events.string_from_kernel_log_level kernel_verbosity)
      evm_state
  in
  return evm_state

let alter_evm_state ~disable_da_fees ~kernel ~kernel_verbosity evm_state =
  let open Lwt_result_syntax in
  let*! evm_state =
    if disable_da_fees then patch_da_fees evm_state else Lwt.return evm_state
  in
  let* evm_state =
    match kernel with
    | None -> return evm_state
    | Some kernel -> patch_kernel ~kernel evm_state
  in
  match kernel_verbosity with
  | None -> return evm_state
  | Some kernel_verbosity -> patch_verbosity ~kernel_verbosity evm_state

let make_executor ro_ctxt ~disable_da_fees ~kernel ~kernel_verbosity ?profile ()
    =
  let base_alter = alter_evm_state ~disable_da_fees ~kernel ~kernel_verbosity in
  (module struct
    let replay ?log_file ?profile:p ?alter_evm_state number =
      let open Lwt_result_syntax in
      let profile = match p with Some _ -> p | None -> profile in
      let composed evm_state =
        let* evm_state = base_alter evm_state in
        match alter_evm_state with
        | None -> return evm_state
        | Some f -> f evm_state
      in
      let+ result =
        Evm_ro_context.replay
          ro_ctxt
          ?log_file
          ?profile
          ~alter_evm_state:composed
          Blueprint
          number
      in
      match result with
      | Replay_success {block; evm_state; tezos_block; _} ->
          Evm_state.Apply_success {block; evm_state; tezos_block}
      | Replay_failure -> Apply_failure

    let execute ?alter_evm_state:_ _ _ =
      failwith "execute not supported in trace mode"
  end : Evm_execution.S)

let trace_block ~tracer_kind ~disable_da_fees ?kernel ?kernel_verbosity ~number
    ?profile config =
  let open Lwt_result_syntax in
  let pool = Lwt_domain.setup_pool 1 in
  let* ro_ctxt = Evm_ro_context.load ~pool config in
  Block_storage_setup.enable
    ~keep_alive:config.keep_alive
    ~timeout:config.rpc_timeout
    ro_ctxt.store ;
  let executor =
    make_executor ro_ctxt ~disable_da_fees ~kernel ~kernel_verbosity ?profile ()
  in
  let module Storage : Block_storage_sig.S = struct
    let current_block_number () =
      failwith "current_block_number not supported in trace mode"

    let nth_block ~full_transaction_object level =
      let open Lwt_result_syntax in
      Evm_store.use ro_ctxt.store @@ fun conn ->
      let* block_opt =
        Evm_store.Blocks.find_with_level
          ~full_transaction_object
          conn
          (Qty level)
      in
      match block_opt with
      | None -> failwith "Block %a not found" Z.pp_print level
      | Some block -> return block

    let block_by_hash ~full_transaction_object:_ _ =
      failwith "block_by_hash not supported in trace mode"

    let block_receipts _ = failwith "block_receipts not supported in trace mode"

    let block_range_receipts ?mask:_ _ _ =
      failwith "block_range_receipts not supported in trace mode"

    let transaction_receipt hash =
      Evm_store.use ro_ctxt.store @@ fun conn ->
      Evm_store.Transactions.find_receipt conn hash

    let transaction_object _ =
      failwith "transaction_object not supported in trace mode"
  end in
  let tracer_config = Tracer_types.default_config in
  let config = {tracer_config with tracer = tracer_kind} in
  let* output =
    Tracer.trace_block executor (module Storage) ~block_number:number ~config
  in
  let json =
    Data_encoding.Json.construct Tracer_types.block_output_encoding output
  in
  Format.printf "%a@." Data_encoding.Json.pp json ;
  return_unit

let trace_transaction ~tracer_kind ~disable_da_fees ?kernel ?kernel_verbosity
    ~tx_hash ?profile config =
  let open Lwt_result_syntax in
  let pool = Lwt_domain.setup_pool 1 in
  let* ro_ctxt = Evm_ro_context.load ~pool config in
  Block_storage_setup.enable
    ~keep_alive:config.keep_alive
    ~timeout:config.rpc_timeout
    ro_ctxt.store ;
  let* receipt =
    Evm_store.use ro_ctxt.store @@ fun conn ->
    Evm_store.Transactions.find_receipt conn tx_hash
  in
  let* block_number =
    match receipt with
    | None ->
        let (Ethereum_types.Hash (Hex h)) = tx_hash in
        failwith "Transaction %s not found" h
    | Some Transaction_receipt.{blockNumber; _} -> return blockNumber
  in
  let executor =
    make_executor ro_ctxt ~disable_da_fees ~kernel ~kernel_verbosity ?profile ()
  in
  let tracer_config = Tracer_types.default_config in
  let config = {tracer_config with tracer = tracer_kind} in
  let* output =
    Tracer.trace_transaction
      executor
      ~block_number
      ~transaction_hash:tx_hash
      ~config
  in
  let json = Data_encoding.Json.construct Tracer_types.output_encoding output in
  Format.printf "%a@." Data_encoding.Json.pp json ;
  return_unit

let replay_blueprint ~strategy ~disable_da_fees ?kernel ?kernel_verbosity
    ~number ?profile ?upto config =
  let open Lwt_result_syntax in
  let pool = Lwt_domain.setup_pool 1 in
  let* up_to_level =
    match upto with
    | None -> return number
    | Some v ->
        if v < number then
          failwith
            "'upto' must be a level succeeding the initial replayed level"
        else return v
  in
  let* ro_ctxt = Evm_ro_context.load ~pool config in
  Block_storage_setup.enable
    ~keep_alive:config.keep_alive
    ~timeout:config.rpc_timeout
    ro_ctxt.store ;
  let alter_evm_state =
    alter_evm_state ~disable_da_fees ~kernel ~kernel_verbosity
  in
  let rec replay_upto current =
    if current > up_to_level then return_unit
    else
      let* apply_result =
        Evm_ro_context.replay ro_ctxt ?profile ~alter_evm_state strategy current
      in
      match apply_result with
      | Replay_success {diverged; process_time; execution_gas; _} ->
          let*! () =
            Blueprint_events.blueprint_replayed
              ~execution_gas
              ~process_time
              ~diverged
              current
          in
          replay_upto Ethereum_types.Qty.(next current)
      | Replay_failure ->
          failwith
            "Could not replay blueprint %a"
            Ethereum_types.pp_quantity
            current
  in
  replay_upto number
