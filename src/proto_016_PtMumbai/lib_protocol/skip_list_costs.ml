(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Inferred from model model_next in file skip_list_benchmarks.ml *)
(* fun size -> (19.813850951 * (log2 (1 + size))) *)
let model_next ~length =
  let open S.Syntax in
  let length = S.safe_z length in
  S.safe_int 20 * log2 (S.safe_int 1 + length)

(* Inferred from model proto/alpha/skip_list/hash_cell in file
   skip_list_benchmarks.ml *)
(* fun size -> (242.202299543 + (56.9693504823 * size)) *)
let model_hash_cell backpointers_count =
  let open S.Syntax in
  S.safe_int 250 + (S.safe_int 57 * backpointers_count)

let model_hash_cell_computed_backpointers_count ~index =
  model_hash_cell (S.Syntax.log2 (S.safe_z index))

let model_hash_cell ~backpointers_count =
  model_hash_cell (S.safe_int backpointers_count)
