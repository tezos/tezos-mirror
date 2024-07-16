(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori  <contact@functori.com>                       *)
(*                                                                           *)
(*****************************************************************************)

module type SimulationBackend = sig
  type simulation_state

  val simulation_state :
    ?block:Ethereum_types.Block_parameter.extended ->
    unit ->
    simulation_state tzresult Lwt.t

  val simulate_and_read :
    simulation_state ->
    input:Simulation.Encodings.simulate_input ->
    bytes tzresult Lwt.t

  val read : simulation_state -> path:string -> bytes option tzresult Lwt.t
end

(* This value is a hard maximum used by estimateGas. Set at Int64.max_int / 2 *)
let max_gas_limit = Z.of_int64 0x3FFFFFFFFFFFFFFFL

module Make (SimulationBackend : SimulationBackend) = struct
  let get_kernel_version simulation_state =
    let open Lwt_result_syntax in
    let* bytes =
      SimulationBackend.read
        simulation_state
        ~path:Durable_storage_path.kernel_version
    in
    let result =
      match bytes with
      | Some bytes -> Bytes.to_string bytes
      | None -> "KERNEL_VERSION_NOT_INITIALISED"
    in
    return result

  let get_storage_version simulation_state =
    let open Lwt_result_syntax in
    let+ bytes =
      SimulationBackend.read
        simulation_state
        ~path:Durable_storage_path.storage_version
    in
    match bytes with
    | Some bytes -> Z.of_bits (Bytes.unsafe_to_string bytes) |> Z.to_int
    | None -> 0

  let call_simulation ~log_file ~input_encoder ~input simulation_state =
    let open Lwt_result_syntax in
    let*? messages = input_encoder input in
    let insight_requests =
      [Simulation.Encodings.Durable_storage_key ["evm"; "simulation_result"]]
    in
    SimulationBackend.simulate_and_read
      simulation_state
      ~input:
        {
          messages;
          reveal_pages = None;
          insight_requests;
          log_kernel_debug_file = Some log_file;
        }

  (* The feature DA fees has been added later in the kernel. The node must
     decide whether it wants to activate it or not.

     As the simulation can be performed on past states, including past kernels,
     we cannot consider it enabled even if it's supported by all latest
     kernels on ghostnet and mainnet.
  *)
  let enabled_with_da_fees simulation_state =
    let open Lwt_result_syntax in
    let* storage_version = get_storage_version simulation_state in
    if storage_version < 12 then return_false
    else if storage_version > 12 then return_true
    else
      (* We are in the unknown, some kernels with STORAGE_VERSION = 12 have
         the features, some do not. *)
      let* kernel_version = get_kernel_version simulation_state in
      (* This is supposed to be the only version where STORAGE_VERSION is 12,
         but with_da_fees isn't enabled. *)
      if kernel_version = "ec7c3b349624896b269e179384d0a45cf39e1145" then
        return_false
      else return_true

  let simulate_call call block_param =
    let open Lwt_result_syntax in
    let* simulation_state =
      SimulationBackend.simulation_state ~block:block_param ()
    in
    let* enabled_with_da_fees = enabled_with_da_fees simulation_state in
    let* bytes =
      call_simulation
        simulation_state
        ~log_file:"simulate_call"
        ~input_encoder:Simulation.encode
        ~input:
          {
            call;
            with_da_fees = (if enabled_with_da_fees then Some true else None);
          }
    in
    Lwt.return (Simulation.simulation_result bytes)

  let call_estimate_gas call simulation_state =
    let open Lwt_result_syntax in
    let* bytes =
      call_simulation
        ~log_file:"estimate_gas"
        ~input_encoder:Simulation.encode
        ~input:call
        simulation_state
    in
    Lwt.return (Simulation.gas_estimation bytes)

  let rec confirm_gas ~enabled_with_da_fees (call : Ethereum_types.call) gas
      simulation_state =
    let open Ethereum_types in
    let open Lwt_result_syntax in
    let double (Qty z) = Qty Z.(mul (of_int 2) z) in
    let reached_max (Qty z) = z >= max_gas_limit in
    let new_call = {call with gas = Some gas} in
    let* result =
      call_estimate_gas
        {
          call = new_call;
          with_da_fees = (if enabled_with_da_fees then Some false else None);
        }
        simulation_state
    in
    match result with
    | Error _ | Ok (Error _) ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/6984
           All errors should not be treated the same *)
        let new_gas = double gas in
        if reached_max new_gas then
          failwith "Gas estimate reached max gas limit."
        else confirm_gas ~enabled_with_da_fees call new_gas simulation_state
    | Ok (Ok _) -> (
        (* Since the gas computation related to execution is fine. We can
           call the estimation with the da fees taken into account. *)
        let* res =
          call_estimate_gas
            {
              call;
              with_da_fees = (if enabled_with_da_fees then Some true else None);
            }
            simulation_state
        in
        match res with
        | Ok (Ok {gas_used = Some gas; _}) -> return gas
        | _ -> failwith "The gas estimation simulation with DA fees failed.")

  let estimate_gas call =
    let open Lwt_result_syntax in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7376

       Gas estimation currently ignores the block parameter. When this is fixed,
       we need to give the block parameter to the call to
       {!enabled_with_da_fees}. *)
    let* simulation_state = SimulationBackend.simulation_state () in
    let* enabled_with_da_fees = enabled_with_da_fees simulation_state in
    let* res =
      call_estimate_gas
        {
          call;
          with_da_fees = (if enabled_with_da_fees then Some false else None);
        }
        simulation_state
    in
    match res with
    | Ok (Ok {gas_used = Some gas; value}) ->
        let+ gas_used =
          confirm_gas ~enabled_with_da_fees call gas simulation_state
        in
        Ok (Ok {Simulation.gas_used = Some gas_used; value})
    | _ -> return res

  let is_tx_valid tx_raw =
    let open Lwt_result_syntax in
    let* simulation_state = SimulationBackend.simulation_state () in
    let* bytes =
      call_simulation
        ~log_file:"tx_validity"
        ~input_encoder:Simulation.encode_tx
        ~input:tx_raw
        simulation_state
    in
    Lwt.return (Simulation.is_tx_valid bytes)
end
