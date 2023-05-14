(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

module LinearModel : sig
  val ridge :
    alpha:float ->
    ?fit_intercept:bool ->
    input:Scikit_matrix.t ->
    output:Scikit_matrix.t ->
    unit ->
    Scikit_matrix.t

  val lasso :
    alpha:float ->
    ?fit_intercept:bool ->
    ?positive:bool ->
    input:Scikit_matrix.t ->
    output:Scikit_matrix.t ->
    unit ->
    Scikit_matrix.t

  val nnls : input:Scikit_matrix.t -> output:Scikit_matrix.t -> Scikit_matrix.t
end

val predict_output :
  input:Scikit_matrix.t -> weights:Scikit_matrix.t -> Pytypes.pyobject

val r2_score :
  output:Scikit_matrix.t -> prediction:Pytypes.pyobject -> float option

val rmse_score : output:Scikit_matrix.t -> prediction:Pytypes.pyobject -> float

val benchmark_score :
  input:Scikit_matrix.t ->
  output:(float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  Scikit_matrix.t * Scikit_matrix.t
