(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type SimulationBackend = sig
  val simulate_and_read :
    input:Simulation.Encodings.simulate_input -> bytes tzresult Lwt.t
end

(* This value is a hard maximum used by estimateGas. Set at Int64.max_int / 2 *)
let max_gas_limit = Z.of_int64 0x3FFFFFFFFFFFFFFFL

module Make (SimulationBackend : SimulationBackend) = struct
  let call_simulation ~log_file ~input_encoder ~input =
    let open Lwt_result_syntax in
    let*? messages = input_encoder input in
    let insight_requests =
      [Simulation.Encodings.Durable_storage_key ["evm"; "simulation_result"]]
    in
    SimulationBackend.simulate_and_read
      ~input:
        {
          messages;
          reveal_pages = None;
          insight_requests;
          log_kernel_debug_file = Some log_file;
        }

  let simulate_call call =
    let open Lwt_result_syntax in
    let* bytes =
      call_simulation
        ~log_file:"simulate_call"
        ~input_encoder:Simulation.encode
        ~input:call
    in
    Lwt.return (Simulation.simulation_result bytes)

  let call_estimate_gas call =
    let open Lwt_result_syntax in
    let* bytes =
      call_simulation
        ~log_file:"estimate_gas"
        ~input_encoder:Simulation.encode
        ~input:call
    in
    Lwt.return (Simulation.gas_estimation bytes)

  let rec confirm_gas (call : Ethereum_types.call) gas =
    let open Ethereum_types in
    let open Lwt_result_syntax in
    let double (Qty z) = Qty Z.(mul (of_int 2) z) in
    let reached_max (Qty z) = z >= max_gas_limit in
    let new_call = {call with gas = Some gas} in
    let* result = call_estimate_gas new_call in
    match result with
    | Error _ | Ok (Error _) ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/6984
           All errors should not be treated the same *)
        let new_gas = double gas in
        if reached_max new_gas then
          failwith "Gas estimate reached max gas limit."
        else confirm_gas call new_gas
    | Ok (Ok _) -> return gas

  let estimate_gas call =
    let open Lwt_result_syntax in
    let* res = call_estimate_gas call in
    match res with
    | Ok (Ok {gas_used = Some gas; value}) ->
        let+ gas_used = confirm_gas call gas in
        Ok (Ok {Simulation.gas_used = Some gas_used; value})
    | _ -> return res

  let is_tx_valid tx_raw =
    let open Lwt_result_syntax in
    let* bytes =
      call_simulation
        ~log_file:"tx_validity"
        ~input_encoder:Simulation.encode_tx
        ~input:tx_raw
    in
    Lwt.return (Simulation.is_tx_valid bytes)
end
