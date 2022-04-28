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
end

(* We assume that the gas cost of adding messages [[ m_1; ... ; m_n]] at level
  [l] is linear in the sum of lengths of the messages, and it is logarithmic 
  in [l]. That is, [cost_add_messages([m_1; .. ; m_n], l)] = 
  `n * cost_add_message_base + 
   cost_add_message_per_bytes * \sum_{i=1}^n length(m_i) +
   cost_add_inbox_per_level * l`.
*)

let cost_add_messages ~num_messages ~total_messages_size l =
  let open S_syntax in
  let log_level =
    if Int32.equal l Int32.zero then Saturation_repr.safe_int 0
    else log2 @@ S.safe_int (Int32.to_int l)
  in
  let level_cost = log_level * Constants.cost_add_inbox_per_level in
  (S.safe_int num_messages * Constants.cost_add_message_base)
  + level_cost
  + (Constants.cost_add_message_per_byte * S.safe_int total_messages_size)
