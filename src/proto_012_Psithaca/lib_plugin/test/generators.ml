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

let string_gen = QCheck2.Gen.small_string ?gen:None

let public_key_hash_gen :
    (Signature.public_key_hash * Signature.public_key * Signature.secret_key)
    QCheck2.Gen.t =
  let open QCheck2.Gen in
  let+ seed = string_size (32 -- 64) in
  let seed = Bytes.of_string seed in
  Signature.generate_key ~seed ()

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2407
   move this function to an helper file? *)
let operation_hash_gen : Operation_hash.t QCheck2.Gen.t =
  let open QCheck2.Gen in
  let+ s = QCheck2.Gen.string_size (return 32) in
  Operation_hash.of_string_exn s

let dummy_manager_op_info oph =
  {
    Plugin.Mempool.operation_hash = oph;
    gas_limit = Alpha_context.Gas.Arith.zero;
    fee = Alpha_context.Tez.zero;
    weight = Q.zero;
  }

let dummy_manager_op_info_with_key_gen :
    (Plugin.Mempool.manager_op_info * Signature.public_key_hash) QCheck2.Gen.t =
  let open QCheck2.Gen in
  let+ (oph, (pkh, _, _)) = pair operation_hash_gen public_key_hash_gen in
  (dummy_manager_op_info oph, pkh)

let filter_state_gen : Plugin.Mempool.state QCheck2.Gen.t =
  let open QCheck2.Gen in
  let open Plugin.Mempool in
  let+ inputs = small_list (pair operation_hash_gen public_key_hash_gen) in
  List.fold_left
    (fun state (oph, (pkh, _, _)) ->
      match Operation_hash.Map.find oph state.operation_hash_to_manager with
      | Some _ -> state
      | None ->
          let info = dummy_manager_op_info oph in
          let prechecked_operations_count =
            if Operation_hash.Map.mem oph state.operation_hash_to_manager then
              state.prechecked_operations_count
            else state.prechecked_operations_count + 1
          in
          let op_weight = op_weight_of_info info in
          let min_prechecked_op_weight =
            match state.min_prechecked_op_weight with
            | Some mini when Q.(mini.weight < info.weight) -> Some mini
            | Some _ | None -> Some op_weight
          in
          {
            state with
            op_prechecked_managers =
              Signature.Public_key_hash.Map.add
                pkh
                info
                state.op_prechecked_managers;
            operation_hash_to_manager =
              Operation_hash.Map.add oph pkh state.operation_hash_to_manager;
            ops_prechecked =
              ManagerOpWeightSet.add op_weight state.ops_prechecked;
            prechecked_operations_count;
            min_prechecked_op_weight;
          })
    Plugin.Mempool.empty
    inputs

let with_filter_state_operation_gen :
    Plugin.Mempool.state ->
    (Plugin.Mempool.manager_op_info * Signature.public_key_hash) QCheck2.Gen.t =
 fun state ->
  let open QCheck2.Gen in
  let* use_fresh = bool in
  let to_ops map =
    Operation_hash.Map.bindings map
    |> List.map (fun (oph, pkh) -> (dummy_manager_op_info oph, pkh))
  in
  if use_fresh || Operation_hash.Map.is_empty state.operation_hash_to_manager
  then dummy_manager_op_info_with_key_gen
  else oneofl (to_ops state.operation_hash_to_manager)

let filter_state_with_operation_gen :
    (Plugin.Mempool.state
    * (Plugin.Mempool.manager_op_info * Signature.public_key_hash))
    QCheck2.Gen.t =
  let open QCheck2.Gen in
  filter_state_gen >>= fun state ->
  pair (return state) (with_filter_state_operation_gen state)
