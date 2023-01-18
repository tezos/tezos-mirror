(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Mempool = Plugin.Mempool

let string_gen = QCheck2.Gen.small_string ?gen:None

let public_key_hash_gen :
    (Tezos_crypto.Signature.public_key_hash
    * Tezos_crypto.Signature.public_key
    * Tezos_crypto.Signature.secret_key)
    QCheck2.Gen.t =
  let open QCheck2.Gen in
  let+ seed = string_size (32 -- 64) in
  let seed = Bytes.of_string seed in
  Tezos_crypto.Signature.generate_key ~seed ()

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2407
   move this function to an helper file? *)
let operation_hash_gen : Operation_hash.t QCheck2.Gen.t =
  let open QCheck2.Gen in
  let+ s = QCheck2.Gen.string_size (return 32) in
  Operation_hash.of_string_exn s

let dummy_manager_op_info =
  let fee = Alpha_context.Tez.zero in
  let gas_limit = Alpha_context.Gas.Arith.zero in
  let manager_op =
    let open Alpha_context in
    let source = Tezos_crypto.Signature.Public_key_hash.zero in
    let counter = Manager_counter.Internal_for_tests.of_int 0 in
    let storage_limit = Z.zero in
    let operation = Set_deposits_limit None in
    let contents =
      Manager_operation
        {source; fee; counter; operation; gas_limit; storage_limit}
    in
    let contents = Single contents in
    let protocol_data =
      {contents; signature = Some Tezos_crypto.Signature.zero}
    in
    let branch = Block_hash.zero in
    Mempool.Manager_op {shell = {branch}; protocol_data}
  in
  Mempool.{manager_op; fee; gas_limit; weight = Q.zero}

let oph_and_info_gen =
  let open QCheck2.Gen in
  let+ oph = operation_hash_gen in
  (oph, dummy_manager_op_info)

let filter_state_gen : Plugin.Mempool.ops_state QCheck2.Gen.t =
  let open QCheck2.Gen in
  let open Plugin.Mempool in
  let+ ops = small_list oph_and_info_gen in
  List.fold_left
    (fun state (oph, info) ->
      if Operation_hash.Map.mem oph state.prechecked_manager_ops then state
      else
        let prechecked_manager_op_count =
          state.prechecked_manager_op_count + 1
        in
        let op_weight = mk_op_weight oph info in
        let min_prechecked_op_weight =
          match state.min_prechecked_op_weight with
          | Some old_min
            when Mempool.compare_manager_op_weight old_min op_weight <= 0 ->
              Some old_min
          | Some _ | None -> Some op_weight
        in
        {
          prechecked_manager_op_count;
          prechecked_manager_ops =
            Operation_hash.Map.add oph info state.prechecked_manager_ops;
          prechecked_op_weights =
            ManagerOpWeightSet.add op_weight state.prechecked_op_weights;
          min_prechecked_op_weight;
        })
    Plugin.Mempool.empty_ops_state
    ops

(** Generate a pair of operation hash and manager_op_info, that has
    even odds of belonging to the given filter_state or being fresh. *)
let with_filter_state_operation_gen :
    Plugin.Mempool.ops_state ->
    (Operation_hash.t * Plugin.Mempool.manager_op_info) QCheck2.Gen.t =
 fun state ->
  let open QCheck2.Gen in
  let* use_fresh = bool in
  if use_fresh || Operation_hash.Map.is_empty state.prechecked_manager_ops then
    oph_and_info_gen
  else oneofl (Operation_hash.Map.bindings state.prechecked_manager_ops)

(** Generate both a filter_state, and a pair of operation hash and
    manager_op_info. The pair has even odds of belonging to the
    filter_state or being fresh. *)
let filter_state_with_operation_gen :
    (Plugin.Mempool.ops_state
    * (Operation_hash.t * Plugin.Mempool.manager_op_info))
    QCheck2.Gen.t =
  let open QCheck2.Gen in
  filter_state_gen >>= fun state ->
  pair (return state) (with_filter_state_operation_gen state)

(** Generate a filter_state, and two pairs of operation hash and
    manager_op_info. The pairs have indepedent, even odds of belonging
    to the filter_state or being fresh. *)
let filter_state_with_two_operations_gen :
    (Plugin.Mempool.ops_state
    * (Operation_hash.t * Plugin.Mempool.manager_op_info)
    * (Operation_hash.t * Plugin.Mempool.manager_op_info))
    QCheck2.Gen.t =
  let open QCheck2.Gen in
  let* filter_state = filter_state_gen in
  triple
    (return filter_state)
    (with_filter_state_operation_gen filter_state)
    (with_filter_state_operation_gen filter_state)
