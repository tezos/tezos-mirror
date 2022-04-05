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

open Plot

type options = {
  save_directory : string;
  point_size : float;
  qt_target_pixel_size : (int * int) option;
  pdf_target_cm_size : (float * float) option;
  reduced_plot_verbosity : bool;
  plot_raw_workload : bool;
}

let options_encoding =
  let open Data_encoding in
  conv
    (fun {
           save_directory;
           point_size;
           qt_target_pixel_size;
           pdf_target_cm_size;
           reduced_plot_verbosity;
           plot_raw_workload;
         } ->
      ( save_directory,
        point_size,
        qt_target_pixel_size,
        pdf_target_cm_size,
        reduced_plot_verbosity,
        plot_raw_workload ))
    (fun ( save_directory,
           point_size,
           qt_target_pixel_size,
           pdf_target_cm_size,
           reduced_plot_verbosity,
           plot_raw_workload ) ->
      {
        save_directory;
        point_size;
        qt_target_pixel_size;
        pdf_target_cm_size;
        reduced_plot_verbosity;
        plot_raw_workload;
      })
    (obj6
       (req "save_directory" string)
       (req "point_size" float)
       (opt "qt_target_pixel_size" (tup2 int31 int31))
       (opt "pdf_target_cm_size" (tup2 float float))
       (dft "reduced_plot_verbosity" bool true)
       (dft "plot_raw_workload" bool false))

let default_options =
  {
    save_directory = Filename.get_temp_dir_name ();
    point_size = 0.5;
    qt_target_pixel_size = None;
    pdf_target_cm_size = None;
    reduced_plot_verbosity = true;
    plot_raw_workload = false;
  }

let point_size opts = opts.point_size

let qt_pixel_size opts = opts.qt_target_pixel_size

let pdf_cm_size opts = opts.pdf_target_cm_size

(* A [raw_row] is consists in a list of named values called a workload
   (corresponding to time measurement of events) together with the
   corresponding measured execution time. Hence, if [n] is the length
   of the workload, there are [n+1] columns. *)
type raw_row = {workload : (string * float) list; qty : float}

(* Gnuplot interprets by default underscores as subscript symbols *)
let underscore_to_dash = String.map (fun c -> if c = '_' then '-' else c)

(* Workload contains all sample but by default, we only plot the median. *)
let convert_workload_data :
    (Sparse_vec.String.t * float array) list -> raw_row list =
 fun workload_data ->
  List.map
    (fun (vec, qty) ->
      let a = Stats.Emp.quantile (module Float) qty 0.5 in
      {workload = Sparse_vec.String.to_list vec; qty = a})
    workload_data

let style opts i =
  let open Style in
  match i with
  | 0 ->
      default |> set_color Color.blue
      |> set_point ~ptyp:Pointtype.disk ~psize:(point_size opts)
  | 1 ->
      default |> set_color Color.red
      |> set_point ~ptyp:Pointtype.box ~psize:(point_size opts)
  | 2 ->
      default |> set_color Color.green
      |> set_point ~ptyp:Pointtype.delta_solid ~psize:(point_size opts)
  | _ -> Stdlib.failwith "Display.style: style overflow"

let scatterplot_2d opts title (xaxis, input) outputs =
  let plots =
    List.mapi
      (fun i output ->
        let style = style opts i in
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

let scatterplot_3d opts title (xaxis, input_x) (yaxis, input_y) outputs =
  let plots =
    List.mapi
      (fun i output ->
        let style = style opts i in
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
let plot_scatter opts title input_columns outputs =
  let open Result_syntax in
  match input_columns with
  | [] ->
      Format.kasprintf
        Result.error
        "Display.plot_scatter (%s): empty scatter data"
        title
  | [column] ->
      let plot = scatterplot_2d opts title column outputs in
      return [plot]
  | [column1; column2] ->
      let plot = scatterplot_3d opts title column1 column2 outputs in
      return [plot]
  | _ ->
      if opts.reduced_plot_verbosity then return []
      else
        let subsets = Benchmark_helpers.enumerate_subsets 2 input_columns in
        let plots =
          List.map
            (function
              | [((dim1, _) as col1); ((dim2, _) as col2)] ->
                  let dim1 = underscore_to_dash dim1 in
                  let dim2 = underscore_to_dash dim2 in
                  let title = Format.asprintf "%s (%s, %s)" title dim1 dim2 in
                  scatterplot_3d opts title col1 col2 outputs
              | cols ->
                  let len = List.length cols in
                  Format.kasprintf
                    Stdlib.failwith
                    "plot_scatter: bug found (got %d columns, expected 2)"
                    len)
            subsets
        in
        return plots

(* Extract size vs timing information from the [workload_data], e.g.
   if   workload_data = (Add_int_int 10 20, 2879) :: (Add_int_int 3 2, 768) :: []
   then outputs named column vectors:
     [ ("Add_int_int1", [| 10 ; 20 |]) ; ("Add_int_int2", [| 3 ; 2 |]) ]
   together with timings:
     [| 2879 ; 768 |] *)

let empirical_data (workload_data : (Sparse_vec.String.t * float array) list) =
  let samples = convert_workload_data workload_data in
  (* Extract name of variables and check well-formedness *)
  let variables =
    List.rev_map (fun {workload; _} -> List.rev_map fst workload) samples
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

let column_is_constant (v : Maths.vector) =
  let rows = Maths.vec_dim v in
  assert (rows > 0) ;
  let fst = Maths.Vector.get v 0 in
  let flg = ref true in
  for i = 1 to rows - 1 do
    let v = Maths.Vector.get v i in
    flg := !flg && v = fst
  done ;
  !flg

(* Prune the dimensions of the input matrix which are constant. *)
let prune_problem input nmap : (Free_variable.t * Maths.vector) list =
  let cols = Maths.col_dim input in
  let named_columns =
    List.init ~when_negative_length:() cols (fun c ->
        let name = Inference.NMap.nth_exn nmap c in
        let col = Maths.Matrix.col input c in
        (name, col))
    |> (* column count cannot be negative *)
    WithExceptions.Result.get_ok ~loc:__LOC__
  in
  List.filter (fun (_, col) -> not (column_is_constant col)) named_columns

let column_to_array (m : Maths.matrix) =
  let rows = Maths.row_dim m in
  let cols = Maths.col_dim m in
  assert (cols = 1) ;
  Array.init rows (fun i -> Maths.Matrix.get m (0, i))

let vector_to_array = Maths.vector_to_array

let validator opts (problem : Inference.problem) (solution : Inference.solution)
    =
  let open Result_syntax in
  match problem with
  | Inference.Degenerate _ -> Error "Display.validator: degenerate plot"
  | Inference.Non_degenerate {input; output; nmap; _} ->
      let {Inference.weights; _} = solution in
      let predicted = Maths.Matrix.mm input weights in
      let columns = prune_problem input nmap in
      let columns =
        List.map
          (fun (c, m) ->
            (Format.asprintf "%a" Free_variable.pp c, vector_to_array m))
          columns
      in
      let median_timing =
        Maths.map_rows
          (fun row ->
            Stats.Emp.quantile (module Float) (Maths.vector_to_array row) 0.5)
          output
      in
      let timings = vector_to_array median_timing in
      let predicted = vector_to_array (Maths.Matrix.col predicted 0) in
      let* plots =
        plot_scatter
          opts
          "Validation (chosen basis)"
          columns
          [timings; predicted]
      in
      return plots

let empirical opts (workload_data : (Sparse_vec.String.t * float array) list) =
  let open Result_syntax in
  if opts.reduced_plot_verbosity then return []
  else
    let* columns, timings = empirical_data workload_data in
    let* plots = plot_scatter opts "Empirical" columns [timings] in
    return plots

let eval_mset (mset : Free_variable.Sparse_vec.t)
    (eval : Free_variable.t -> float) =
  Free_variable.Sparse_vec.fold
    (fun var coeff acc -> acc +. (eval var *. coeff))
    mset
    0.0

let eval_affine (aff : Costlang.affine) (eval : Free_variable.t -> float) =
  eval_mset aff.linear_comb eval +. aff.const

let validator_empirical opts workload_data (problem : Inference.problem)
    (solution : Inference.solution) =
  let open Result_syntax in
  let {Inference.mapping; _} = solution in
  let valuation name =
    WithExceptions.Option.get ~loc:__LOC__
    @@ List.assoc ~equal:Free_variable.equal name mapping
  in
  let predicted =
    match problem with
    | Inference.Degenerate {predicted; _} -> column_to_array predicted
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
  let* plots =
    plot_scatter opts "Validation (raw)" columns [timings; predicted]
  in
  return plots

type plot_target = Save | Show

let raw_workload (workload_data : (Sparse_vec.String.t * float array) list) =
  let open Plot in
  List.map
    (fun (workload, data) ->
      let workload = Sparse_vec.String.to_list workload in
      let title =
        Format.asprintf
          "%a"
          (Format.pp_print_list
             ~pp_sep:(fun fmtr () -> Format.fprintf fmtr "-")
             (fun fmtr (k, v) -> Format.fprintf fmtr "%s=%d" k (int_of_float v)))
          workload
        |> underscore_to_dash
      in
      let points = data |> Array.to_seq |> Seq.map r1 |> Data.of_seq in
      plot2
        ~xaxis:"time"
        ~yaxis:"freq"
        ~title
        [Histogram.hist ~binwidth:50.0 ~points ()])
    workload_data

let perform_plot ~measure ~model_name ~problem ~solution ~plot_target ~options =
  let (Measure.Measurement ((module Bench), measurement)) = measure in
  let filename ?index kind =
    let dir = options.save_directory in
    let kind =
      match index with None -> kind | Some i -> Format.asprintf "%s-%d" kind i
    in
    Filename.Infix.(
      dir // Format.asprintf "%s_%s_%s.pdf" Bench.name model_name kind)
  in
  let workload_data =
    List.map
      (fun {Measure.workload; measures} ->
        (Bench.workload_to_vector workload, Maths.vector_to_array measures))
      measurement.workload_data
  in
  let try_plot plot_target kind plot_result =
    match plot_result with
    | Ok [] -> []
    | Ok plots -> (
        match plot_target with
        | Save ->
            List.mapi
              (fun i plot ->
                let pdf_file = filename ~index:i kind in
                let target = pdf ?cm_size:(pdf_cm_size options) ~pdf_file () in
                Plot.run ~target exec_detach plot ;
                pdf_file)
              plots
        | Show ->
            let target =
              match Plot.get_targets () with
              | None ->
                  Format.eprintf
                    "Failed performing plot: could not get list of terminal \
                     types, defaulting to x11@." ;
                  x11
              | Some available_targets ->
                  if List.mem ~equal:String.equal "qt" available_targets then
                    qt ?pixel_size:(qt_pixel_size options) ()
                  else (
                    Format.eprintf
                      "\"qt\" gnuplot terminal not available, defaulting to \
                       x11@." ;
                    x11)
            in
            let plots = Array.of_list (List.map (fun x -> [|Some x|]) plots) in
            Plot.run_matrix ~target exec_detach plots ;
            [])
    | Error msg ->
        Format.eprintf "Failed performing plot: %s@." msg ;
        []
  in
  let raw =
    if options.plot_raw_workload then
      List.mapi
        (fun i plot ->
          let kind = Format.asprintf "raw-%.2d" i in
          try_plot Save kind (Result.ok [plot]))
        (raw_workload workload_data)
      |> List.flatten
    else []
  in
  (try_plot plot_target "emp" @@ empirical options workload_data)
  @ (try_plot plot_target "validation" @@ validator options problem solution)
  @ (try_plot plot_target "emp-validation"
    @@ validator_empirical options workload_data problem solution)
  @ raw
