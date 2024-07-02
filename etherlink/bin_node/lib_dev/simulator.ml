(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori  <contact@functori.com>                       *)
(*                                                                           *)
(*****************************************************************************)

module type SimulationBackend = sig
  val simulate_and_read :
    ?block:Ethereum_types.Block_parameter.extended ->
    input:Simulation.Encodings.simulate_input ->
    unit ->
    bytes tzresult Lwt.t
end

(* This value is a hard maximum used by estimateGas. Set at Int64.max_int / 2 *)
let max_gas_limit = Z.of_int64 0x3FFFFFFFFFFFFFFFL

module Make
    (Reader : Durable_storage.READER)
    (SimulationBackend : SimulationBackend) =
struct
  let get_kernel_version () =
    let open Lwt_result_syntax in
    let* bytes = Reader.read Durable_storage_path.kernel_version in
    let result =
      match bytes with
      | Some bytes -> Bytes.to_string bytes
      | None -> "KERNEL_VERSION_NOT_INITIALISED"
    in
    return result

  let call_simulation ?block ~log_file ~input_encoder ~input () =
    let open Lwt_result_syntax in
    let*? messages = input_encoder input in
    let insight_requests =
      [Simulation.Encodings.Durable_storage_key ["evm"; "simulation_result"]]
    in
    SimulationBackend.simulate_and_read
      ?block
      ~input:
        {
          messages;
          reveal_pages = None;
          insight_requests;
          log_kernel_debug_file = Some log_file;
        }
      ()

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7328
     The few following lines should be removed as soon as possible when
     the next kernel is released on production. *)
  let enabled_with_da_fees () =
    let open Lwt_result_syntax in
    let+ kernel_version = get_kernel_version () in
    match kernel_version with
    | "b9f6c9138719220db83086f0548e49c5c4c8421f"
      (* Mainnet before security ugprade *)
    | "4d98081699595b57512ffeff35bca320f281c806" (* Mainnet *)
    | "ec7c3b349624896b269e179384d0a45cf39e1145" (* Ghostnet *)
    | "KERNEL_VERSION_NOT_INITIALISED" ->
        false
    | _ -> true

  let simulate_call call block_param =
    let open Lwt_result_syntax in
    let* enabled_with_da_fees = enabled_with_da_fees () in
    let* bytes =
      call_simulation
        ~block:block_param
        ~log_file:"simulate_call"
        ~input_encoder:Simulation.encode
        ~input:
          {
            call;
            with_da_fees = (if enabled_with_da_fees then Some true else None);
          }
        ()
    in
    Lwt.return (Simulation.simulation_result bytes)

  let call_estimate_gas call =
    let open Lwt_result_syntax in
    let* bytes =
      call_simulation
        ~log_file:"estimate_gas"
        ~input_encoder:Simulation.encode
        ~input:call
        ()
    in
    Lwt.return (Simulation.gas_estimation bytes)

  let rec confirm_gas ~enabled_with_da_fees (call : Ethereum_types.call) gas =
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
    in
    match result with
    | Error _ | Ok (Error _) ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/6984
           All errors should not be treated the same *)
        let new_gas = double gas in
        if reached_max new_gas then
          failwith "Gas estimate reached max gas limit."
        else confirm_gas ~enabled_with_da_fees call new_gas
    | Ok (Ok _) -> (
        (* Since the gas computation related to execution is fine. We can
           call the estimation with the da fees taken into account. *)
        let* res =
          call_estimate_gas
            {
              call;
              with_da_fees = (if enabled_with_da_fees then Some true else None);
            }
        in
        match res with
        | Ok (Ok {gas_used = Some gas; _}) -> return gas
        | _ -> failwith "The gas estimation simulation with DA fees failed.")

  let estimate_gas call =
    let open Lwt_result_syntax in
    let* enabled_with_da_fees = enabled_with_da_fees () in
    let* res =
      call_estimate_gas
        {
          call;
          with_da_fees = (if enabled_with_da_fees then Some false else None);
        }
    in
    match res with
    | Ok (Ok {gas_used = Some gas; value}) ->
        let+ gas_used = confirm_gas ~enabled_with_da_fees call gas in
        Ok (Ok {Simulation.gas_used = Some gas_used; value})
    | _ -> return res

  let is_tx_valid tx_raw =
    let open Lwt_result_syntax in
    let* bytes =
      call_simulation
        ~log_file:"tx_validity"
        ~input_encoder:Simulation.encode_tx
        ~input:tx_raw
        ()
    in
    Lwt.return (Simulation.is_tx_valid bytes)
end
