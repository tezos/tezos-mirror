(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

module S = Saturation_repr

module S_syntax = struct
  let log2 x = S.safe_int (1 + S.numbits x)

  let ( + ) = S.add

  let ( * ) = S.mul
end

module Constants = struct
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/2648
     Fill in real benchmarked values.
     Need to create benchmark and fill in values.
  *)
  let cost_add_message_base = S.safe_int 430

  let cost_add_message_per_byte = S.safe_int 15

  let cost_add_inbox_per_level = S.safe_int 15

  let cost_update_num_and_size_of_messages = S.safe_int 15

  (* equal to Michelson_v1_gas.Cost_of.Unparsing.contract_optimized *)
  let cost_decoding_contract_optimized = S.safe_int 70

  (* equal to Michelson_v1_gas.Cost_of.Unparsing.key_hash_optimized *)
  let cost_decoding_key_hash_optimized = S.safe_int 50
end

(* We assume that the gas cost of adding messages [[ m_1; ... ; m_n]] at level
   [l] is linear in the sum of lengths of the messages, and it is logarithmic
   in [l]. That is, [cost_add_serialized_messages([m_1; .. ; m_n], l)] =
   `n * cost_add_message_base +
    cost_add_message_per_bytes * \sum_{i=1}^n length(m_i) +
    cost_add_inbox_per_level * l`.
*)
let cost_add_serialized_messages ~num_messages ~total_messages_size l =
  let open S_syntax in
  let log_level =
    if Int32.equal l Int32.zero then Saturation_repr.safe_int 0
    else log2 @@ S.safe_int (Int32.to_int l)
  in
  let level_cost = log_level * Constants.cost_add_inbox_per_level in
  (S.safe_int num_messages * Constants.cost_add_message_base)
  + level_cost
  + (Constants.cost_add_message_per_byte * S.safe_int total_messages_size)

(* Reusing model from {!Ticket_costs.has_tickets_of_ty_cost}. *)
let is_valid_parameters_ty_cost ~ty_size =
  let fixed_cost = S.safe_int 10 in
  let coeff = S.safe_int 6 in
  S.add fixed_cost (S.mul coeff ty_size)

let cost_serialize_internal_inbox_message
    Sc_rollup_inbox_message_repr.{payload; sender = _; source = _} =
  let lexpr = Script_repr.lazy_expr payload in
  let expr_cost = Script_repr.force_bytes_cost lexpr in
  S_syntax.(
    expr_cost + Constants.cost_decoding_contract_optimized
    + Constants.cost_decoding_key_hash_optimized)

(** We assume that the cost of deserializing an expression of [bytes_len] is
    greater by a notch to the real cost here.

    TODO: checks if the estimated cost is close to the more precise cost: To
    check the real cost we could traverse the list of expression in the
    deserialized output message. *)
let cost_deserialize_outbox_message ~bytes_len =
  Script_repr.deserialization_cost_estimated_from_bytes bytes_len
