(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type SimulationBackend = sig
  val simulate_and_read :
    input:Simulation.Encodings.simulate_input ->
    Simulation.Encodings.insights tzresult Lwt.t
end

(* This value is a hard maximum used by estimateGas. Set at Int64.max_int / 2 *)
let max_gas_limit = Z.of_int64 0x3FFFFFFFFFFFFFFFL

module Make (SimulationBackend : SimulationBackend) = struct
  let simulate_call call =
    let open Lwt_result_syntax in
    let*? messages = Simulation.encode call in
    let insight_requests =
      [
        Simulation.Encodings.Durable_storage_key ["evm"; "simulation_gas"];
        Simulation.Encodings.Durable_storage_key ["evm"; "simulation_result"];
        Simulation.Encodings.Durable_storage_key ["evm"; "simulation_status"];
      ]
    in
    let* results =
      SimulationBackend.simulate_and_read
        ~input:
          {
            messages;
            reveal_pages = None;
            insight_requests;
            log_kernel_debug_file = Some "simulate_call";
          }
    in
    Simulation.call_result results

  let call_estimate_gas call =
    let open Lwt_result_syntax in
    let*? messages = Simulation.encode call in
    let insight_requests =
      [
        Simulation.Encodings.Durable_storage_key ["evm"; "simulation_gas"];
        Simulation.Encodings.Durable_storage_key ["evm"; "simulation_result"];
        Simulation.Encodings.Durable_storage_key ["evm"; "simulation_status"];
      ]
    in
    let* results =
      SimulationBackend.simulate_and_read
        ~input:
          {
            messages;
            reveal_pages = None;
            insight_requests;
            log_kernel_debug_file = Some "estimate_gas";
          }
    in
    Simulation.gas_estimation results

  let rec confirm_gas (call : Ethereum_types.call) gas =
    let open Ethereum_types in
    let open Lwt_result_syntax in
    let double (Qty z) = Qty Z.(mul (of_int 2) z) in
    let reached_max (Qty z) = z >= max_gas_limit in
    let new_call = {call with gas = Some gas} in
    let* result = call_estimate_gas new_call in
    match result with
    | Error _ ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/6984
           All errors should not be treated the same *)
        let new_gas = double gas in
        if reached_max new_gas then
          failwith "Gas estimate reached max gas limit."
        else confirm_gas call new_gas
    | Ok _ -> return (Ok gas)

  let estimate_gas call =
    let open Lwt_result_syntax in
    let* res = call_estimate_gas call in
    match res with
    | Ok gas -> confirm_gas call gas
    | Error e -> failwith "Couldn't estimate gas: %s" e

  let is_tx_valid tx_raw =
    let open Lwt_result_syntax in
    let*? messages = Simulation.encode_tx tx_raw in
    let insight_requests =
      [
        Simulation.Encodings.Durable_storage_key ["evm"; "simulation_gas"];
        Simulation.Encodings.Durable_storage_key ["evm"; "simulation_result"];
        Simulation.Encodings.Durable_storage_key ["evm"; "simulation_status"];
      ]
    in
    let* results =
      SimulationBackend.simulate_and_read
        ~input:
          {
            messages;
            reveal_pages = None;
            insight_requests;
            log_kernel_debug_file = Some "tx_validity";
          }
    in
    Simulation.is_tx_valid results
end
