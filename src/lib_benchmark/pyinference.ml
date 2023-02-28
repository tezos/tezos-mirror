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

module Numpy = struct
  let transpose x =
    let npy_transpose = Py.Module.get_function (Pyinit.numpy ()) "transpose" in
    npy_transpose [|x|]
end

module LinearModel = struct
  let assert_matrix_nontrivial (m : Scikit_matrix.t) =
    let l, c = Scikit_matrix.shape m in
    assert (l <> 0 && c <> 0)

  let ridge ~(alpha : float) ?(fit_intercept : bool = false)
      ~(input : Scikit_matrix.t) ~(output : Scikit_matrix.t) () =
    assert_matrix_nontrivial input ;
    assert_matrix_nontrivial output ;
    let input = Scikit_matrix.to_numpy input in
    let output = Scikit_matrix.to_numpy output in
    let ridge_object =
      Py.Module.get_function_with_keywords
        (Pyinit.linear_model ())
        "Ridge"
        [||]
        [
          ("alpha", Py.Float.of_float alpha);
          ("fit_intercept", Py.Bool.of_bool fit_intercept);
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
    | Some coef -> Scikit_matrix.of_numpy (Numpy.transpose coef)

  let lasso ~(alpha : float) ?(fit_intercept : bool = false)
      ?(positive : bool = false) ~(input : Scikit_matrix.t)
      ~(output : Scikit_matrix.t) () =
    assert_matrix_nontrivial input ;
    assert_matrix_nontrivial output ;
    let input = Scikit_matrix.to_numpy input in
    let output = Scikit_matrix.to_numpy output in
    let lasso_object =
      Py.Module.get_function_with_keywords
        (Pyinit.linear_model ())
        "Lasso"
        [||]
        [
          ("alpha", Py.Float.of_float alpha);
          ("fit_intercept", Py.Bool.of_bool fit_intercept);
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
    | Some coef -> Scikit_matrix.of_numpy coef

  let nnls ~(input : Scikit_matrix.t) ~(output : Scikit_matrix.t) =
    assert_matrix_nontrivial input ;
    assert_matrix_nontrivial output ;
    let len = Scikit_matrix.dim1 output in
    let input = Scikit_matrix.to_numpy input in
    let output = Scikit_matrix.to_numpy output in
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
      Scikit_matrix.of_numpy res
end

let predict_output ~(input : Scikit_matrix.t) ~(weights : Scikit_matrix.t) =
  let weights = Scikit_matrix.to_numpy weights in
  let input = Scikit_matrix.to_numpy input in
  Py.Module.get_function (Pyinit.numpy ()) "matmul" [|input; weights|]

let r2_score ~output ~prediction =
  let len = Scikit_matrix.dim1 output in
  let output = Scikit_matrix.to_numpy output in
  if len <= 1 then
    (* The following warning will be raised from `r2_score` of Python. *)
    (* `R^2 score is not well-defined with less than two samples.` *)
    (* see https://scikit-learn.org/stable/modules/generated/sklearn.metrics.r2_score.html#sklearn.metrics.r2_score *)

    (* For this case, we use `None` as the score. *)
    None
  else
    Py.Module.get_function
      (Pyinit.sklearn_metrics ())
      "r2_score"
      [|output; prediction|]
    |> Py.Float.to_float |> Option.some

let rmse_score ~output ~prediction =
  let output = Scikit_matrix.to_numpy output in
  Py.Module.get_function_with_keywords
    (Pyinit.sklearn_metrics ())
    "mean_squared_error"
    [|output; prediction|]
    [("squared", Py.Bool.f)]
  |> Py.Float.to_float

let benchmark_score ~input ~output =
  let input = Scikit_matrix.to_numpy input in
  let output = Scikit_matrix.to_numpy_vector output in
  let model =
    Py.Module.get_function (Pyinit.statsmodels_api ()) "OLS" [|output; input|]
  in
  let result =
    Py.Module.get_function model "fit" [||]
    (* We couldn't get tvalue from fit_reguralized for now *)
    (* Py.Module.get_function_with_keywords model "fit_regularized" [||]
       [("method",Py.String.of_string "elastic_net");
       ("alpha", Py.Float.of_float 0.03);
       ("L1_wt", Py.Float.of_float 1.)] *)
  in
  let tvalues =
    Py.Object.find_attr_string result "tvalues" |> Scikit_matrix.of_numpy
  in
  let params =
    Py.Object.find_attr_string result "params" |> Scikit_matrix.of_numpy
  in
  (params, tvalues)
