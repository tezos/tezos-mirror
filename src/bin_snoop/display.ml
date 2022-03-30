(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(* ------------------------------------------------------------------------- *)
(* Smart display of inference result.
   We're only able to plot simple cases automatically: models with one or
   two dependent variables + constants. Above this, we bail out.

   We plot each Inference.problem in two ways:
   1. The "empirical" plot consists in plotting the vector of sizes vs the
      execution time. Doing this requires access to the raw data (i.e. before
      applying the model). Since we can't plot in 4d, we limit these plots to
      set of data where there are at most two size inputs, e.g. a binary
      instruction such as Add is ok, or a sequence of two unary instructions,
      etc.
   2. The "validator" plot consists in plotting the predicted execution
      time as a function of the basis vs the empirical execution time.

   For 2. we make some effort for simplifiying the model, i.e. we get rid
   of the constant base "functions".
*)

module Matrix = Pyplot.Matrix
open Plot

(* Gnuplot interprets by default underscores as subscript symbols *)
let underscore_to_dash = String.map (fun c -> if c = '_' then '-' else c)

(* A [raw_row] is consists in a list of named values called a workload
   (corresponding to time measurement of events) together with the
   corresponding measured execution time. Hence, if [n] is the length
   of the workload, there are [n+1] columns. *)
type raw_row = {workload : (string * float) list; qty : float}

let convert_workload_data : (Sparse_vec.String.t * float) list -> raw_row list =
 fun workload_data ->
  List.map
    (fun (vec, qty) -> {workload = Sparse_vec.String.to_list vec; qty})
    workload_data

let style i =
  let open Style in
  match i with
  | 0 ->
      default |> set_color Color.blue
      |> set_point ~ptyp:Pointtype.disk ~psize:0.5
  | 1 ->
      default |> set_color Color.red |> set_point ~ptyp:Pointtype.box ~psize:0.5
  | 2 ->
      default |> set_color Color.green
      |> set_point ~ptyp:Pointtype.delta_solid ~psize:0.2
  | _ -> Stdlib.failwith "Display.style: style overflow"

let scatterplot_2d title (xaxis, input) outputs =
  let plots =
    List.mapi
      (fun i output ->
        let style = style i in
        let points = Data.of_array @@ Array.map2 Plot.r2 input output in
        Scatter.points_2d ~points ~style ())
      outputs
  in
  let xaxis = underscore_to_dash xaxis in
  plot2 ~xaxis ~yaxis:"timing" ~title plots

let rec map3 f l1 l2 l3 () =
  let open Seq in
  match (l1 (), l2 (), l3 ()) with
  | Nil, _, _ | _, Nil, _ | _, _, Nil -> Nil
  | Cons (x1, l1'), Cons (x2, l2'), Cons (x3, l3') ->
      Cons (f x1 x2 x3, map3 f l1' l2' l3')

let scatterplot_3d title (xaxis, input_x) (yaxis, input_y) outputs =
  let plots =
    List.mapi
      (fun i output ->
        let style = style i in
        let xs = Array.to_seq input_x in
        let ys = Array.to_seq input_y in
        let zs = Array.to_seq output in
        let points = map3 Plot.r3 xs ys zs |> Data.of_seq in
        Scatter.points_3d ~points ~style ())
      outputs
  in
  let xaxis = underscore_to_dash xaxis in
  let yaxis = underscore_to_dash yaxis in
  plot3 ~xaxis ~yaxis ~zaxis:"timing" ~title plots

(* Scatter plot/s/ of the input vectors specified by [input_columns]
   against the [outputs]. This will superpose [List.length outputs]
   scatter plots on the same page. *)
let plot_scatter title input_columns outputs =
  let open Result_syntax in
  match input_columns with
  | [] ->
      Format.kasprintf
        Result.error
        "Display.plot_scatter (%s): empty scatter data"
        title
  | [column] ->
      let plot = scatterplot_2d title column outputs in
      return [plot]
  | [column1; column2] ->
      let plot = scatterplot_3d title column1 column2 outputs in
      return [plot]
  | _ ->
      let subsets = Benchmark_helpers.enumerate_subsets 2 input_columns in
      let plots =
        List.map
          (function
            | [((dim1, _) as col1); ((dim2, _) as col2)] ->
                let title = Format.asprintf "%s (%s, %s)" title dim1 dim2 in
                scatterplot_3d title col1 col2 outputs
            | _ -> assert false)
          subsets
      in
      return plots

(* Extract size vs timing information from the [workload_data], e.g.
   if   workload_data = (Add_int_int 10 20, 2879) :: (Add_int_int 3 2, 768) :: []
   then outputs named column vectors:
     [ ("Add_int_int1", [| 10 ; 20 |]) ; ("Add_int_int2", [| 3 ; 2 |]) ]
   together with timings:
     [| 2879 ; 768 |] *)

let empirical_data (workload_data : (Sparse_vec.String.t * float) list) =
  let samples = convert_workload_data workload_data in
  (* Extract name of variables and check well-formedness *)
  let variables =
    List.map (fun {workload; _} -> List.map fst workload) samples
  in
  let variables = List.sort_uniq Stdlib.compare variables in
  match variables with
  | [] | _ :: _ :: _ ->
      Format.kasprintf
        Result.error
        "Display.empirical_data: variables not named consistenly@."
  | [vars] ->
      let rows = List.length samples in
      let input_dims = List.length vars in
      let columns = Array.init input_dims (fun _ -> Array.make rows 0.0) in
      let timings = Array.make rows 0.0 in
      List.iteri
        (fun i {workload; qty} ->
          assert (Compare.List_length_with.(workload = input_dims)) ;
          List.iteri
            (fun input_dim (_, size) -> columns.(input_dim).(i) <- size)
            workload ;
          timings.(i) <- qty)
        samples ;
      let columns = Array.to_list columns in
      let named_columns =
        List.combine ~when_different_lengths:() vars columns
        |> (* [columns = Array.to_list (Array.init (List.length vars))] *)
        WithExceptions.Result.get_ok ~loc:__LOC__
      in
      Ok (named_columns, timings)

let column_is_constant (m : Matrix.t) =
  let rows, cols = Matrix.shape m in
  assert (cols = 1) ;
  let fst = Matrix.get m 0 0 in
  let flg = ref true in
  for i = 1 to rows - 1 do
    let v = Matrix.get m i 0 in
    flg := !flg && v = fst
  done ;
  !flg

(* Prune the dimensions of the input matrix which are constant. *)
let prune_problem problem : (Free_variable.t * Matrix.t) list * Matrix.t =
  match problem with
  | Inference.Degenerate _ -> assert false
  | Inference.Non_degenerate {input; output; nmap; _} ->
      let _, cols = Matrix.shape input in
      let named_columns =
        List.init ~when_negative_length:() cols (fun c ->
            let name = Inference.NMap.nth_exn nmap c in
            let col = Matrix.column input c in
            (name, col))
        |> (* column count cannot be negative *)
        WithExceptions.Result.get_ok ~loc:__LOC__
      in
      let columns =
        List.filter (fun (_, col) -> not (column_is_constant col)) named_columns
      in
      (columns, output)

let column_to_array (m : Matrix.t) =
  let rows = Matrix.dim1 m in
  let cols = Matrix.dim2 m in
  assert (cols = 1) ;
  Array.init rows (fun i -> Matrix.get m i 0)

let row_to_array (m : Matrix.t) =
  let rows = Matrix.dim1 m in
  let cols = Matrix.dim2 m in
  assert (rows = 1) ;
  Array.init cols (fun i -> Matrix.get m 0 i)

let validator (problem : Inference.problem) (solution : Inference.solution) =
  let open Result_syntax in
  match problem with
  | Inference.Degenerate _ -> Error "Display.validator: degenerate plot"
  | Inference.Non_degenerate {input; _} ->
      let {Inference.weights; _} = solution in
      let predicted = Matrix.numpy_mul input weights in
      let columns, timings = prune_problem problem in
      let columns =
        List.map
          (fun (c, m) ->
            (Format.asprintf "%a" Free_variable.pp c, column_to_array m))
          columns
      in
      let timings = column_to_array timings in
      let predicted = column_to_array predicted in
      let* plots =
        plot_scatter "Validation (chosen basis)" columns [timings; predicted]
      in
      return (List.length plots, plots)

let empirical (workload_data : (Sparse_vec.String.t * float) list) =
  let open Result_syntax in
  let* columns, timings = empirical_data workload_data in
  let* plots = plot_scatter "Empirical" columns [timings] in
  let nrows = List.length plots in
  return (nrows, plots)

let eval_mset (mset : Free_variable.Sparse_vec.t)
    (eval : Free_variable.t -> float) =
  Free_variable.Sparse_vec.fold
    (fun var coeff acc -> acc +. (eval var *. coeff))
    mset
    0.0

let eval_affine (aff : Costlang.affine) (eval : Free_variable.t -> float) =
  eval_mset aff.linear_comb eval +. aff.const

let validator_empirical workload_data (problem : Inference.problem)
    (solution : Inference.solution) =
  let open Result_syntax in
  let {Inference.mapping; _} = solution in
  let valuation name =
    WithExceptions.Option.get ~loc:__LOC__
    @@ List.assoc ~equal:Free_variable.equal name mapping
  in
  let predicted =
    match problem with
    | Inference.Degenerate {predicted; _} -> row_to_array predicted
    | Inference.Non_degenerate {lines; _} ->
        let predicted_list =
          List.map
            (fun ols_line ->
              let (Inference.Full (affine, _)) = ols_line in
              eval_affine affine valuation)
            lines
        in
        Array.of_list predicted_list
  in
  let* columns, timings = empirical_data workload_data in
  let* plots = plot_scatter "Validation (raw)" columns [timings; predicted] in
  let nrows = List.length plots in
  return (nrows, plots)

type plot_target =
  | Save of {file : string option}
  | Show
  | ShowAndSave of {file : string option}

let perform_plot ~measure ~model_name ~problem ~solution ~plot_target =
  let open Result_syntax in
  let (Measure.Measurement ((module Bench), measurement)) = measure in
  let filename kind =
    Format.asprintf "%s_%s_%s.pdf" Bench.name model_name kind
  in
  let workload_data =
    List.map
      (fun {Measure.workload; qty} -> (Bench.workload_to_vector workload, qty))
      measurement.workload_data
  in
  let* rows1, empirical_plots = empirical workload_data in
  let* rows2, validator_plots = validator problem solution in
  let* rows3, validator_emp_plots =
    validator_empirical workload_data problem solution
  in
  let rows = max rows1 (max rows2 rows3) in
  let plot_matrix = Array.make_matrix rows 3 None in
  List.iteri (fun i plot -> plot_matrix.(i).(0) <- Some plot) empirical_plots ;
  List.iteri (fun i plot -> plot_matrix.(i).(1) <- Some plot) validator_plots ;
  List.iteri
    (fun i plot -> plot_matrix.(i).(2) <- Some plot)
    validator_emp_plots ;
  let target =
    match plot_target with
    | Save {file} ->
        let pdf_file =
          match file with
          | None -> filename "collected"
          | Some filename -> filename
        in
        pdf ~pdf_file ()
    | Show -> qt ~pixel_size:(1920, 1080) ()
    | ShowAndSave {file = _} ->
        Format.eprintf "display: ShowAndSave target deprecated@." ;
        qt ()
  in
  Plot.run_matrix ~target exec plot_matrix ;
  return ()

let perform_plot ~measure ~model_name ~problem ~solution ~plot_target =
  match perform_plot ~measure ~model_name ~problem ~solution ~plot_target with
  | Error msg ->
      Format.eprintf "Display: error (%s)@." msg ;
      false
  | _ -> true
