(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 DaiLambda, Inc., <contact@dailambda.jp>                *)
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
open S.Syntax

(* model global_constants_storage/expr_to_address_in_context *)
(* Approximating 200 + 1.266960 * number of bytes *)
let cost_expr_to_address_in_context size =
  let v0 = S.safe_int size in
  S.safe_int 200 + (v0 + (v0 lsr 2))

(* Unfortunatelly codegen cannot create 4095 but 4096 *)
(* model global_constants_storage/expand_constant_branch *)
(* fun size -> (4095. * size) *)
let cost_expand_constant_branch size =
  let open S.Syntax in
  let size = S.safe_int size in
  let v0 = size in
  v0 * S.safe_int 4095

(* model global_constants_storage/expand_no_constant_branch *)
(* Approximating 100 + 4.639474 * n*log(n) *)
(* Model expression is different ... *)
let cost_expand_no_constant_branch size =
  let size = S.safe_int size in
  let v0 = size * log2 size in
  S.safe_int 100 + (v0 * S.safe_int 4) + (v0 lsr 1) + (v0 lsr 3)

let expr_to_address_in_context_cost bytes =
  cost_expr_to_address_in_context (Bytes.length bytes)
  |> Gas_limit_repr.atomic_step_cost

let expand_constants_branch_cost =
  cost_expand_constant_branch 1 |> Gas_limit_repr.atomic_step_cost

let expand_no_constants_branch_cost node =
  cost_expand_no_constant_branch (Script_repr.micheline_nodes node)
  |> Gas_limit_repr.atomic_step_cost
