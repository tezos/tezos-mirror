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

let main ~strategy ~disable_da_fees ?kernel ?kernel_verbosity ~number ?profile
    ?upto config =
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
