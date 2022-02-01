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
  (* TODO Use real smart contract cost instead of 0.0 when we support them. *)
  let infer_contract_cost (_, n) = (Float.of_int n, 0.0) in
  let _contract_costs = List.map infer_contract_cost average_block.contract in
  let calculated_cost =
    [regular_cost] |> weighted_average |> Float.ceil |> Float.to_int
  in
  (* Add 100 for safety. *)
  calculated_cost + 100

let deduce_tps ~protocol ~constants ~transaction_cost () =
  let json = JSON.parse_file (Protocol.parameter_file ~constants protocol) in
  let hard_gas_limit_per_block =
    Float.of_int (JSON.as_int (JSON.get "hard_gas_limit_per_block" json))
  in
  let transaction_cost_float = Float.of_int transaction_cost in
  let max_transactions_per_block =
    hard_gas_limit_per_block /. transaction_cost_float
  in
  let round0_duration =
    float_of_string (JSON.as_string (JSON.get "minimal_block_delay" json))
  in
  Float.to_int (Float.ceil (max_transactions_per_block /. round0_duration))
