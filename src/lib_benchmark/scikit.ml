(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

module Matrix = Pyplot.Matrix

module Numpy = struct
  let transpose x =
    let npy_transpose = Py.Module.get_function (Pyinit.numpy ()) "transpose" in
    npy_transpose [|x|]
end

module LinearModel = struct
  let assert_matrix_nontrivial (m : Matrix.t) =
    let l, c = Matrix.shape m in
    assert (l <> 0 && c <> 0)

  let ridge ~(alpha : float) ?(fit_intercept : bool = false)
      ?(normalize : bool = false) ~(input : Matrix.t) ~(output : Matrix.t) () =
    assert_matrix_nontrivial input ;
    assert_matrix_nontrivial output ;
    let input = Matrix.to_numpy input in
    let output = Matrix.to_numpy output in
    let ridge_object =
      Py.Module.get_function_with_keywords
        (Pyinit.linear_model ())
        "Ridge"
        [||]
        [
          ("alpha", Py.Float.of_float alpha);
          ("fit_intercept", Py.Bool.of_bool fit_intercept);
          ("normalize", Py.Bool.of_bool normalize);
        ]
    in
    let _ =
      match Py.Object.get_attr_string ridge_object "fit" with
      | None -> Stdlib.failwith "Scikit.LinearModel.ridge: method fit not found"
      | Some meth -> Py.Callable.to_function meth [|input; output|]
    in
    match Py.Object.get_attr_string ridge_object "coef_" with
    | None ->
        Stdlib.failwith "Scikit.LinearModel.ridge: attribute coef_ not found"
    | Some coef -> Matrix.of_numpy (Numpy.transpose coef)

  let lasso ~(alpha : float) ?(fit_intercept : bool = false)
      ?(normalize : bool = false) ?(positive : bool = false) ~(input : Matrix.t)
      ~(output : Matrix.t) () =
    assert_matrix_nontrivial input ;
    assert_matrix_nontrivial output ;
    let input = Matrix.to_numpy input in
    let output = Matrix.to_numpy output in
    let lasso_object =
      Py.Module.get_function_with_keywords
        (Pyinit.linear_model ())
        "Lasso"
        [||]
        [
          ("alpha", Py.Float.of_float alpha);
          ("fit_intercept", Py.Bool.of_bool fit_intercept);
          ("normalize", Py.Bool.of_bool normalize);
          ("positive", Py.Bool.of_bool positive);
        ]
    in
    let _ =
      match Py.Object.get_attr_string lasso_object "fit" with
      | None -> Stdlib.failwith "Scikit.LinearModel.lasso: method fit not found"
      | Some meth -> Py.Callable.to_function meth [|input; output|]
    in
    match Py.Object.get_attr_string lasso_object "coef_" with
    | None ->
        Stdlib.failwith "Scikit.LinearModel.lasso: attribute coef_ not found"
    | Some coef -> Matrix.of_numpy coef

  let nnls ~(input : Matrix.t) ~(output : Matrix.t) =
    assert_matrix_nontrivial input ;
    assert_matrix_nontrivial output ;
    let len = Matrix.dim1 output in
    let input = Matrix.to_numpy input in
    let output = Matrix.to_numpy output in
    let output =
      Py.Module.get_function
        (Pyinit.numpy ())
        "reshape"
        [|output; Py.Int.of_int len|]
    in
    let nnls_outcome =
      Py.Module.get_function (Pyinit.scipy_optimize ()) "nnls" [|input; output|]
    in
    let array = Py.Tuple.to_array nnls_outcome in
    if Array.length array <> 2 then
      Stdlib.failwith "Scikit.nnls: invalid outcome"
    else
      let res = array.(0) in
      Matrix.of_numpy res
end
