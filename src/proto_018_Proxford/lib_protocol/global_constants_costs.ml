(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let log2 x = S.safe_int (1 + S.numbits x)

let ( + ) = S.add

let ( lsr ) = S.shift_right

(* model global_constants_storage/expr_to_address_in_context *)
(* Approximating 200 + 1.266960 * number of bytes *)
let expr_to_address_in_context_cost bytes =
  let v0 = Bytes.length bytes |> S.safe_int in
  S.safe_int 200 + (v0 + (v0 lsr 2)) |> Gas_limit_repr.atomic_step_cost

(* model global_constants_storage/expand_constant_branch *)
let expand_constants_branch_cost =
  Gas_limit_repr.atomic_step_cost @@ S.safe_int 4095

(* model global_constants_storage/expand_no_constant_branch *)
(* Approximating 100 + 4.639474 * n*log(n) *)
let expand_no_constants_branch_cost node =
  let v0 = Script_repr.micheline_nodes node |> S.safe_int in
  let v0 = S.mul v0 (log2 v0) in
  S.safe_int 100 + S.mul (S.safe_int 4) v0 + (v0 lsr 1) + (v0 lsr 3)
  |> Gas_limit_repr.atomic_step_cost
