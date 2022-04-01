(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

type error += Tx_rollup_negative_input_size

module S = Saturation_repr

(** The model in {!Michelson_v1_gas.N_IBlake2b}, plus the allocation
    of bytes from {!Storage_functor}. *)
let hash_cost input_size =
  error_unless Compare.Int.(0 <= input_size) Tx_rollup_negative_input_size
  >>? fun () ->
  let ( + ) = S.add in
  let cost_serialization = Gas_limit_repr.alloc_mbytes_cost input_size in
  let v0 = Saturation_repr.safe_int input_size in
  let cost_N_IBlake2b = S.safe_int 430 + v0 + S.shift_right v0 3 in
  let cost_blake2b = Gas_limit_repr.atomic_step_cost cost_N_IBlake2b in
  ok @@ (cost_serialization + cost_blake2b)

(** Model from {!Ticket_costs.cost_compare_ticket_hash} since they are
    Blake2B hashes too. *)
let compare_blake2b_hash = S.safe_int 10

let check_path_cost element_size path_depth =
  let ( + ) = S.add in
  error_unless Compare.Int.(0 <= path_depth) Tx_rollup_negative_input_size
  >>? fun () ->
  (* We hash the element *)
  hash_cost element_size >>? fun element_hash_cost ->
  (* At each step of the way, we hash 2 hashes together *)
  hash_cost 64 >>? fun hash_cost ->
  let rec acc_hash_cost acc i =
    if Compare.Int.(i <= 0) then acc else acc_hash_cost (hash_cost + acc) (i - 1)
  in

  ok (element_hash_cost + acc_hash_cost compare_blake2b_hash path_depth)

let consume_check_path_inbox_cost ctxt =
  let count_limit = Constants_storage.tx_rollup_max_messages_per_inbox ctxt in
  let max_depth = Merkle_list.max_depth ~count_limit in
  check_path_cost Tx_rollup_prefixes.message_hash.hash_size max_depth
  >>? fun cost -> Raw_context.consume_gas ctxt cost

let consume_check_path_commitment_cost ctxt =
  let count_limit = Constants_storage.tx_rollup_max_messages_per_inbox ctxt in
  let max_depth = Merkle_list.max_depth ~count_limit in
  check_path_cost Tx_rollup_prefixes.message_result_hash.hash_size max_depth
  >>? fun cost -> Raw_context.consume_gas ctxt cost

let snoc_cost element_size max_depth =
  let ( + ) = S.add in
  error_unless Compare.Int.(0 <= max_depth) Tx_rollup_negative_input_size
  >>? fun () ->
  (* We hash the element *)
  hash_cost element_size >>? fun element_hash_cost ->
  (* We consider an over-approximation where the tree is already at it
     maximum height *)
  hash_cost 64 >>? fun hash_cost ->
  let rec acc_hash_cost acc i =
    if Compare.Int.(i <= 0) then acc else acc_hash_cost (hash_cost + acc) (i - 1)
  in

  ok (element_hash_cost + acc_hash_cost (S.safe_int 0) max_depth)

let consume_add_message_cost ctxt =
  let count_limit = Constants_storage.tx_rollup_max_messages_per_inbox ctxt in
  let max_depth = Merkle_list.max_depth ~count_limit in
  snoc_cost Tx_rollup_prefixes.message_hash.hash_size max_depth >>? fun cost ->
  Raw_context.consume_gas ctxt cost

let compute_cost element_size size =
  let ( + ) = S.add in
  let ( * ) = S.mul in
  hash_cost element_size >>? fun element_hash_cost ->
  hash_cost 64 >>? fun inner_hash_cost ->
  ok (element_hash_cost + (S.safe_int 2 * S.safe_int size * inner_hash_cost))

let consume_compact_commitment_cost ctxt n =
  compute_cost Tx_rollup_prefixes.message_result_hash.hash_size n
  >>? fun cost -> Raw_context.consume_gas ctxt cost

let hash ~hash_f ctxt encoding input =
  match Data_encoding.Binary.to_bytes_opt encoding input with
  | Some buffer ->
      let len = Bytes.length buffer in
      hash_cost len >>? fun cost ->
      Raw_context.consume_gas ctxt cost >>? fun ctxt ->
      ok (ctxt, hash_f [buffer])
  | None ->
      error
        (Tx_rollup_errors_repr.Internal_error
           "Cannot serialize input to hash function")

let () =
  let open Data_encoding in
  (* Tx_rollup_negative_message_size *)
  register_error_kind
    `Permanent
    ~id:"tx_rollup_negative_input_size"
    ~title:
      "The protocol has computed a negative size for the input of a hash \
       function"
    ~description:
      "The protocol has computed a negative size for the input of a hash \
       function. This is an internal error, and denotes a bug in the protocol \
       implementation."
    unit
    (function Tx_rollup_negative_input_size -> Some () | _ -> None)
    (fun () -> Tx_rollup_negative_input_size)
