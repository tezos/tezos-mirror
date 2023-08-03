(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Cannot use the generated [cost_next] because of the type difference *)
(* model skip_list/next *)
(* fun size -> (19.813850951 * (log2 (1 + size))) *)
let cost_next size =
  let open S.Syntax in
  let v0 = log2 (S.safe_int 1 + size) in
  v0 * S.safe_int 20

let model_next ~length = cost_next (S.safe_z length)

(* Cannot use the generated [cost_hash_cell] because of the type difference *)
(* model skip_list/hash_cell *)
(* fun size -> (242.202299543 + (56.9693504823 * size)) *)
let cost_hash_cell size =
  let open S.Syntax in
  let v0 = size in
  S.safe_int 250 + (v0 * S.safe_int 57)

let model_hash_cell ~backpointers_count =
  cost_hash_cell (S.safe_int backpointers_count)

let model_hash_cell_computed_backpointers_count ~index =
  cost_hash_cell (S.Syntax.log2 (S.safe_z index))
