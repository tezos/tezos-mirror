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

let pp_prechecked_manager_ops fmt set =
  Format.fprintf
    fmt
    "[%a]"
    (Format.pp_print_list (fun ppf (oph, (op_info : manager_op_info)) ->
         Format.fprintf
           ppf
           "(%a -> {fee: %a; gas: %a; weight: %a})"
           Operation_hash.pp
           oph
           Alpha_context.Tez.pp
           op_info.fee
           Alpha_context.Gas.Arith.pp
           op_info.gas_limit
           Q.pp_print
           op_info.weight))
    (Operation_hash.Map.bindings set)

let pp_manager_op_weight fmt weight =
  Format.fprintf
    fmt
    "{oph: %a; weight: %a}"
    Operation_hash.pp
    weight.operation_hash
    Q.pp_print
    weight.weight

let pp_prechecked_op_weights fmt set =
  Format.fprintf
    fmt
    "[%a]"
    (Format.pp_print_list (fun ppf manager_op_weight ->
         pp_manager_op_weight ppf manager_op_weight))
    (ManagerOpWeightSet.elements set)

let pp_op_weight_opt fmt = function
  | None -> Format.fprintf fmt "None"
  | Some op_weight ->
      Format.fprintf fmt "Some %a" pp_manager_op_weight op_weight

let pp_state fmt state =
  Format.fprintf
    fmt
    "@[<v 0>state:@,\
     {@,\
     prechecked_manager_op_count: %d@,\
     prechecked_manager_ops: %a;@,\
     prechecked_op_weights: %a;@,\
     min_prechecked_op_weight: %a@,\
     }@]"
    state.prechecked_manager_op_count
    pp_prechecked_manager_ops
    state.prechecked_manager_ops
    pp_prechecked_op_weights
    state.prechecked_op_weights
    pp_op_weight_opt
    state.min_prechecked_op_weight

let eq_prechecked_manager_ops =
  Operation_hash.Map.equal
    (fun
      {manager_op = _; fee = fee1; gas_limit = gas1; weight = w1}
      {manager_op = _; fee = fee2; gas_limit = gas2; weight = w2}
    -> Tez.equal fee1 fee2 && Gas.Arith.equal gas1 gas2 && Q.equal w1 w2)

let eq_op_weight_opt =
  Option.equal (fun op_weight1 op_weight2 ->
      Operation_hash.equal op_weight1.operation_hash op_weight2.operation_hash
      && Q.equal op_weight1.weight op_weight2.weight)

(* This function needs to be updated if the filter state is extended *)
let eq_state s1 s2 =
  s1.prechecked_manager_op_count = s2.prechecked_manager_op_count
  && eq_prechecked_manager_ops
       s1.prechecked_manager_ops
       s2.prechecked_manager_ops
  && ManagerOpWeightSet.equal s1.prechecked_op_weights s2.prechecked_op_weights
  && eq_op_weight_opt s1.min_prechecked_op_weight s2.min_prechecked_op_weight
