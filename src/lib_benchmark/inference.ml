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

open Costlang

type constrnt = Full of (Costlang.affine * quantity)

and quantity = Quantity of float

module NMap = Stats.Finbij.Make (Free_variable)

type problem =
  | Non_degenerate of {
      lines : constrnt list;
      input : Scikit.Matrix.t;
      output : Scikit.Matrix.t;
      nmap : NMap.t;
    }
  | Degenerate of {predicted : Scikit.Matrix.t; measured : Scikit.Matrix.t}

type solution = {
  mapping : (Free_variable.t * float) list;
  weights : Scikit.Matrix.t;
}

type solver =
  | Ridge of {alpha : float; normalize : bool}
  | Lasso of {alpha : float; normalize : bool; positive : bool}
  | NNLS

(* -------------------------------------------------------------------------- *)

(* Establish bijection between variable names and integer dimensions *)
let establish_bijection (lines : constrnt list) : NMap.t =
  let elements =
    List.fold_left
      (fun set line ->
        match line with
        | Full ({linear_comb; _}, _quantity) ->
            Free_variable.Sparse_vec.fold
              (fun elt _count set -> Free_variable.Set.add elt set)
              linear_comb
              set)
      Free_variable.Set.empty
      lines
  in
  NMap.of_list (Free_variable.Set.elements elements)

let line_list_to_ols (lines : constrnt list) =
  let nmap = establish_bijection lines in
  let lcount = List.length lines in
  let inputs = Scikit.Matrix.create ~lines:lcount ~cols:(NMap.support nmap) in
  let outputs = Scikit.Matrix.create ~lines:lcount ~cols:1 in
  (* initialize inputs *)
  List.iteri
    (fun i line ->
      match line with
      | Full (affine, Quantity qty) ->
          Free_variable.Sparse_vec.iter
            (fun variable multiplicity ->
              let dim = NMap.idx_exn nmap variable in
              Scikit.Matrix.set inputs i dim multiplicity)
            affine.linear_comb ;
          Scikit.Matrix.set outputs i 0 (qty -. affine.const) ;
          Tezos_stdlib_unix.Utils.display_progress (fun m ->
              m "Initializing matrices %d/%d%!" (i + 1) lcount))
    lines ;
  Tezos_stdlib_unix.Utils.display_progress_end () ;
  (inputs, outputs, nmap)

(* -------------------------------------------------------------------------- *)
(* Computing prediction error *)

type error_statistics = {
  average : float;
  total_l1 : float;
  total_l2 : float;
  avg_l1 : float;
  avg_l2 : float;
  underestimated_measured : float;
}

let pp_error_statistics fmtr err_stat =
  Format.fprintf
    fmtr
    "@[<v 2>{ average = 1/N ∑_i tᵢ - pᵢ = %f;@,\
     total error (L1) = ∑_i |tᵢ - pᵢ| = %f;@,\
     total error (L2) = sqrt(∑_i (tᵢ - pᵢ)²) = %f;@,\
     average error (L1) = 1/N L1 error = %f;@,\
     average error (L2) = 1/N L2 error = %f;@,\
     underestimated = 1/N card{ tᵢ > pᵢ } = %f%% }@]"
    err_stat.average
    err_stat.total_l1
    err_stat.total_l2
    err_stat.avg_l1
    err_stat.avg_l2
    err_stat.underestimated_measured

let column_to_floatarray column =
  let open Scikit in
  assert (Matrix.dim2 column = 1) ;
  let rows = Matrix.dim1 column in
  Array.init rows (fun i -> Matrix.get column i 0)

let compute_error_statistics ~predicted ~measured =
  let open Scikit in
  assert (Matrix.shape predicted = Matrix.shape measured) ;
  assert (Matrix.dim2 predicted = 1) ;
  let predicted = column_to_floatarray predicted in
  let measured = column_to_floatarray measured in
  let error = Array.map2 ( -. ) measured predicted in
  let rows = Array.length error in
  let n = float_of_int rows in
  let arr = Array.init rows (fun i -> error.(i)) in
  let average = Array.fold_left ( +. ) 0.0 arr /. n in
  let total_l1 = Array.map abs_float arr |> Array.fold_left ( +. ) 0.0 in
  let total_l2 =
    let squared_sum =
      Array.map (fun x -> x *. x) arr |> Array.fold_left ( +. ) 0.0
    in
    sqrt squared_sum
  in
  let avg_l1 = total_l1 /. n in
  let avg_l2 = total_l2 /. n in
  let underestimated_measured =
    let indic_under = Array.map (fun x -> if x > 0.0 then 1.0 else 0.0) arr in
    Array.fold_left ( +. ) 0.0 indic_under /. n
  in
  {average; total_l1; total_l2; avg_l1; avg_l2; underestimated_measured}

(* -------------------------------------------------------------------------- *)
(* Making problems *)

let make_problem_from_workloads :
    type workload.
    data:(workload * float) list ->
    overrides:(Free_variable.t -> float option) ->
    evaluate:(workload -> Eval_to_vector.size Eval_to_vector.repr) ->
    problem =
 fun ~data ~overrides ~evaluate ->
  (match data with
  | [] ->
      Stdlib.failwith
        "Inference.make_problem_from_workloads: empty workload data"
  | _ -> ()) ;
  let line_count = List.length data in
  let model_progress =
    Benchmark_helpers.make_progress_printer
      Format.err_formatter
      line_count
      "Applying model to workload data"
  in
  (* This function has to _preserve the order of workloads_. *)
  let lines =
    List.fold_left
      (fun lines (workload, time) ->
        model_progress () ;
        let res = Eval_to_vector.prj (evaluate workload) in
        let res = Hash_cons_vector.prj res in
        let affine = Eval_linear_combination_impl.run overrides res in
        let line = Full (affine, Quantity time) in
        line :: lines)
      []
      data
  in
  Format.eprintf "@." ;
  let lines = List.rev lines in
  if
    List.for_all
      (fun (Full (affine, _)) ->
        Free_variable.Sparse_vec.is_empty affine.linear_comb)
      lines
  then
    let predicted, measured =
      List.map (fun (Full (affine, Quantity q)) -> (affine.const, q)) lines
      |> List.split
    in
    let measured =
      let measured = Array.of_list measured in
      Scikit.Matrix.init ~lines:(Array.length measured) ~cols:1 ~f:(fun l _c ->
          measured.(l))
    in
    let predicted =
      let predicted = Array.of_list predicted in
      Scikit.Matrix.init ~lines:(Array.length predicted) ~cols:1 ~f:(fun l _c ->
          predicted.(l))
    in
    Degenerate {predicted; measured}
  else
    let input, output, nmap = line_list_to_ols lines in
    Non_degenerate {lines; input; output; nmap}

let make_problem :
    data:'workload Measure.workload_data ->
    model:'workload Model.t ->
    overrides:(Free_variable.t -> float option) ->
    problem =
 fun ~data ~model ~overrides ->
  let data = List.map (fun {Measure.workload; qty} -> (workload, qty)) data in
  match model with
  | Model.Packaged {conv; model} ->
      let module M = (val model) in
      let module M = Model.Instantiate (Eval_to_vector) (M) in
      make_problem_from_workloads ~data ~overrides ~evaluate:(fun workload ->
          M.model (conv workload))
  | Model.Preapplied {model} ->
      make_problem_from_workloads ~data ~overrides ~evaluate:(fun workload ->
          let module A = (val model workload) in
          let module I = A (Eval_to_vector) in
          I.applied)

(* -------------------------------------------------------------------------- *)
(* Exporting/importing problems/solutions to CSV *)

let fv_to_string fv = Format.asprintf "%a" Free_variable.pp fv

let to_list_of_rows (m : Scikit.Matrix.t) : float list list =
  let lines, cols = Scikit.Matrix.shape m in
  let init n f =
    List.init ~when_negative_length:() n f
    |> (* lines/column count cannot be negative *)
    WithExceptions.Result.get_ok ~loc:__LOC__
  in
  init lines (fun l -> init cols (fun c -> Scikit.Matrix.get m l c))

let of_list_of_rows (m : float list list) : Scikit.Matrix.t =
  let lines = List.length m in
  let cols =
    List.length (WithExceptions.Option.get ~loc:__LOC__ @@ List.hd m)
  in
  let mat = Scikit.Matrix.create ~lines ~cols in
  List.iteri
    (fun l row -> List.iteri (fun c elt -> Scikit.Matrix.set mat l c elt) row)
    m ;
  mat

let model_matrix_to_csv (m : Scikit.Matrix.t) (nmap : NMap.t) : Csv.csv =
  let _, cols = Scikit.Matrix.shape m in
  let names =
    List.init ~when_negative_length:() cols (fun i ->
        fv_to_string (NMap.nth_exn nmap i))
    |> (* number of column cannot be negative *)
    WithExceptions.Result.get_ok ~loc:__LOC__
  in
  let rows = to_list_of_rows m in
  let rows = List.map (List.map string_of_float) rows in
  names :: rows

let timing_matrix_to_csv colname (m : Scikit.Matrix.t) : Csv.csv =
  let rows = to_list_of_rows m in
  let rows = List.map (List.map string_of_float) rows in
  [colname] :: rows

let problem_to_csv : problem -> Csv.csv = function
  | Non_degenerate {input; output; nmap; _} ->
      let model_csv = model_matrix_to_csv input nmap in
      let timings_csv = timing_matrix_to_csv "timings" output in
      Csv.concat model_csv timings_csv
  | Degenerate {predicted; measured} ->
      let predicted_csv = timing_matrix_to_csv "predicted" predicted in
      let measured_csv = timing_matrix_to_csv "timings" measured in
      Csv.concat predicted_csv measured_csv

let solution_to_csv : solution -> Csv.csv option =
 fun {mapping; _} ->
  match mapping with
  | [] -> None
  | _ ->
      let headers = List.map (fun (fv, _) -> fv_to_string fv) mapping
      and row = List.map (fun x -> Float.to_string (snd x)) mapping in
      Some [headers; row]

(* -------------------------------------------------------------------------- *)
(* Solving problems *)

let solve_problem : problem -> solver -> solution =
 fun problem solver ->
  match problem with
  | Degenerate _ ->
      {mapping = []; weights = Scikit.Matrix.create ~lines:0 ~cols:0}
  | Non_degenerate {input; output; nmap; _} ->
      let weights =
        match solver with
        | Ridge {alpha; normalize} ->
            Scikit.LinearModel.ridge ~alpha ~normalize ~input ~output ()
        | Lasso {alpha; normalize; positive} ->
            Scikit.LinearModel.lasso
              ~alpha
              ~normalize
              ~positive
              ~input
              ~output
              ()
        | NNLS -> Scikit.LinearModel.nnls ~input ~output
      in
      let lines = Scikit.Matrix.dim1 weights in
      if lines <> NMap.support nmap then
        let cols = Scikit.Matrix.dim2 weights in
        let dims = Format.asprintf "%d x %d" lines cols in
        let err =
          Format.asprintf
            "Inference.solve_problem: solution dimensions (%s) mismatch that \
             of given problem"
            dims
        in
        Stdlib.failwith err
      else
        let mapping =
          NMap.fold
            (fun variable dim acc ->
              let param = Scikit.Matrix.get weights dim 0 in
              (variable, param) :: acc)
            nmap
            []
        in
        {mapping; weights}
