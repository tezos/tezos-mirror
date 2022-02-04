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

open Plugin.Mempool
open Alpha_context

let config drift_opt =
  {
    default_config with
    clock_drift =
      Option.map
        (fun drift -> Period.of_seconds_exn (Int64.of_int drift))
        drift_opt;
    replace_by_fee_factor = Q.make (Z.of_int 105) (Z.of_int 100);
  }

let pp_prechecked_managers fmt set =
  Format.fprintf
    fmt
    "[%a]"
    (Format.pp_print_list (fun ppf (pkh, (op_info : manager_op_info)) ->
         Format.fprintf
           ppf
           "(%a -> (hash:%a,gas:%a,fee:%a))"
           Signature.Public_key_hash.pp
           pkh
           Operation_hash.pp
           op_info.operation_hash
           Alpha_context.Gas.Arith.pp
           op_info.gas_limit
           Alpha_context.Tez.pp
           op_info.fee))
    (Signature.Public_key_hash.Map.bindings set)

let pp_operation_hash_manager fmt map =
  Format.fprintf
    fmt
    "[%a]"
    (Format.pp_print_list (fun ppf (hash, pkh) ->
         Format.fprintf
           ppf
           "(%a -> %a)"
           Operation_hash.pp
           hash
           Signature.Public_key_hash.pp
           pkh))
    (Operation_hash.Map.bindings map)

let pp_manager_op_weight fmt weight =
  Format.fprintf
    fmt
    "(oph: %a;weight: %a)"
    Operation_hash.pp
    weight.operation_hash
    Q.pp_print
    weight.weight

let pp_ops_prechecked fmt set =
  Format.fprintf
    fmt
    "[%a]"
    (Format.pp_print_list (fun ppf manager_op_weight ->
         pp_manager_op_weight ppf manager_op_weight))
    (ManagerOpWeightSet.elements set)

let pp_min_prechecked_op_weight fmt weight =
  Option.fold
    ~none:(Format.fprintf fmt "None")
    ~some:(fun w -> Format.fprintf fmt "Some %a" pp_manager_op_weight w)
    weight

let pp_state fmt state =
  Format.fprintf
    fmt
    "@[<v 0>state:@,\
     {@,\
     prechecked_managers:%a;@,\
     operation_hash_to_managers:%a;@,\
     prechecked_operations_count:%d@,\
     ops_prechecked:%a;@,\
     min_prechecked_op_weight:%a@,\
     }@]"
    pp_prechecked_managers
    state.op_prechecked_managers
    pp_operation_hash_manager
    state.operation_hash_to_manager
    state.prechecked_operations_count
    pp_ops_prechecked
    state.ops_prechecked
    pp_min_prechecked_op_weight
    state.min_prechecked_op_weight

let eq_prechecked_managers =
  Signature.Public_key_hash.Map.equal
    (fun
      ({operation_hash = oph1; gas_limit = _; fee = _; weight = _} :
        manager_op_info)
      ({operation_hash = oph2; gas_limit = _; fee = _; weight = _} :
        manager_op_info)
    -> Operation_hash.equal oph1 oph2)

(* This function needs to be updated if the filter state is extended *)
let eq_state s1 s2 =
  let eq_min_prechecked_op_weight =
    match (s1.min_prechecked_op_weight, s2.min_prechecked_op_weight) with
    | (None, None) -> true
    | (Some _, None) | (None, Some _) -> false
    | (Some w1, Some w2) ->
        Operation_hash.equal w1.operation_hash w2.operation_hash
        && Q.equal w1.weight w2.weight
  in
  eq_prechecked_managers s1.op_prechecked_managers s2.op_prechecked_managers
  && Operation_hash.Map.equal
       (fun k1 k2 -> Signature.Public_key_hash.equal k1 k2)
       s1.operation_hash_to_manager
       s2.operation_hash_to_manager
  && s1.prechecked_operations_count = s2.prechecked_operations_count
  && ManagerOpWeightSet.equal s1.ops_prechecked s2.ops_prechecked
  && eq_min_prechecked_op_weight
