(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Contracts = Tezos_client_alpha_commands.Client_proto_stresstest_contracts

let weighted_average (xs : (float * float) list) =
  let (total_weight, total_sum) =
    List.fold_left
      (fun (total_weight, total_sum) (weight, value) ->
        (total_weight +. weight, total_sum +. (value *. weight)))
      (0.0, 0.0)
      xs
  in
  total_sum /. total_weight

let average_transaction_cost
    (transaction_costs : Client.stresstest_gas_estimation)
    (average_block : Average_block.t) =
  let regular_cost =
    (float_of_int average_block.regular, float_of_int transaction_costs.regular)
  in
  let infer_contract_cost (mainnet_address, n) =
    let cost =
      match Contracts.mainnet_address_to_alias mainnet_address with
      | None ->
          Stdlib.failwith ("unknown smart contract address: " ^ mainnet_address)
      | Some alias -> (
          match
            List.find
              (fun (x, _) -> String.equal x alias)
              transaction_costs.smart_contracts
          with
          | None ->
              Stdlib.failwith ("no cost for smart contract alias: " ^ alias)
          | Some (_, x) -> x)
    in
    (Float.of_int n, float_of_int cost)
  in
  let contract_costs = List.map infer_contract_cost average_block.contract in
  let calculated_cost =
    regular_cost :: contract_costs
    |> weighted_average |> Float.ceil |> Float.to_int
  in
  calculated_cost

let deduce_tps ~protocol ~protocol_constants ~average_transaction_cost () =
  let json =
    JSON.parse_file
      (Protocol.parameter_file ~constants:protocol_constants protocol)
  in
  let hard_gas_limit_per_block =
    Float.of_int (JSON.as_int (JSON.get "hard_gas_limit_per_block" json))
  in
  let transaction_cost_float = Float.of_int average_transaction_cost in
  let max_transactions_per_block =
    hard_gas_limit_per_block /. transaction_cost_float
  in
  let round0_duration =
    float_of_string (JSON.as_string (JSON.get "minimal_block_delay" json))
  in
  Float.to_int (Float.ceil (max_transactions_per_block /. round0_duration))

let deduce_fee_and_gas_limit gas_estimation =
  let fee =
    Tez.of_mutez_int
      (Q.to_int
         (Q.mul
            (Q.of_int gas_estimation)
            Tezos_baking_alpha.Baking_configuration.default_config.fees
              .minimal_nanotez_per_gas_unit)
      / 100)
  in
  (fee, gas_estimation + 100)

let calculate_smart_contract_parameters (average_block : Average_block.t)
    (transaction_costs : Client.stresstest_gas_estimation) =
  let total =
    float_of_int
      (average_block.regular
      + List.fold_left (fun acc (_, x) -> x + acc) 0 average_block.contract)
  in
  let calculate_one (mainnet_address, n) =
    match Contracts.mainnet_address_to_alias mainnet_address with
    | None ->
        Stdlib.failwith ("unknown smart contract address: " ^ mainnet_address)
    | Some alias ->
        let gas_estimation =
          match
            List.find
              (fun (x, _) -> String.equal x alias)
              transaction_costs.smart_contracts
          with
          | None -> Stdlib.failwith ("no gas cost estimation for: " ^ alias)
          | Some (_, x) -> x
        in
        let (invocation_fee, invocation_gas_limit) =
          deduce_fee_and_gas_limit gas_estimation
        in
        ( alias,
          ({
             probability = float_of_int n /. total;
             invocation_fee;
             invocation_gas_limit;
           }
            : Client.stresstest_contract_parameters) )
  in
  List.map calculate_one average_block.contract
