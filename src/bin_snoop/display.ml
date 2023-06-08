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

type empirical_plot =
  | Empirical_plot_full
  | Empirical_plot_quantiles of float list

type options = {
  save_directory : string;
  point_size : float;
  qt_target_pixel_size : (int * int) option;
  pdf_target_cm_size : (float * float) option;
  reduced_plot_verbosity : bool;
  plot_raw_workload : bool;
  empirical_plot : empirical_plot;
}

let default_empirical_plot = Empirical_plot_quantiles [0.5]

let nonempty_list_of_quantiles_encoding =
  let open Data_encoding in
  conv_with_guard
    Fun.id
    (fun list ->
      match list with
      | [] -> Error "Display.nonempty_list_of_quantiles: empty"
      | qs ->
          if List.exists (fun q -> q < 0.0 || q > 1.0) qs then
            Error "Display.nonempty_list_of_quantiles: invalid"
          else Ok qs)
    (list float)

let empirical_plot_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"Empirical_plot_full"
        (Tag 0)
        unit
        (function Empirical_plot_full -> Some () | _ -> None)
        (fun () -> Empirical_plot_full);
      case
        ~title:"Empirical_plot_quantiles"
        (Tag 1)
        nonempty_list_of_quantiles_encoding
        (function Empirical_plot_quantiles list -> Some list | _ -> None)
        (fun list -> Empirical_plot_quantiles list);
    ]

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
           empirical_plot;
         } ->
      ( save_directory,
        point_size,
        qt_target_pixel_size,
        pdf_target_cm_size,
        reduced_plot_verbosity,
        plot_raw_workload,
        empirical_plot ))
    (fun ( save_directory,
           point_size,
           qt_target_pixel_size,
           pdf_target_cm_size,
           reduced_plot_verbosity,
           plot_raw_workload,
           empirical_plot ) ->
      {
        save_directory;
        point_size;
        qt_target_pixel_size;
        pdf_target_cm_size;
        reduced_plot_verbosity;
        plot_raw_workload;
        empirical_plot;
      })
    (obj7
       (req "save_directory" string)
       (req "point_size" float)
       (opt "qt_target_pixel_size" (tup2 int31 int31))
       (opt "pdf_target_cm_size" (tup2 float float))
       (dft "reduced_plot_verbosity" bool true)
       (dft "plot_raw_workload" bool false)
       (dft "empirical_plot" empirical_plot_encoding default_empirical_plot))

let default_options =
  {
    save_directory = Filename.get_temp_dir_name ();
    point_size = 0.5;
    qt_target_pixel_size = None;
    pdf_target_cm_size = None;
    reduced_plot_verbosity = true;
    plot_raw_workload = false;
    empirical_plot = default_empirical_plot;
  }

let point_size opts = opts.point_size

let qt_pixel_size opts = opts.qt_target_pixel_size

let pdf_cm_size opts = opts.pdf_target_cm_size

(* A [raw_row] is consists in a list of named values called a workload
   (corresponding to the size of some computational jobs) together with the
   corresponding measured execution time. Hence, if [n] is the length
   of the workload, there are [n+1] columns. *)
type raw_row = {workload : (string * float) list; qty : float array}

(* Gnuplot interprets by default underscores as subscript symbols *)
let underscore_to_dash = String.map (fun c -> if c = '_' then '-' else c)

(* Workload contains all sample but by default, we only plot the median. *)
let convert_workload_data :
    (Sparse_vec.String.t * float array) list -> raw_row list =
 fun workload_data ->
  List.map
    (fun (vec, qty) -> {workload = Sparse_vec.String.to_list vec; qty})
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
      (fun i (output : float array array) ->
        let style = style opts i in
        let xs = Array.to_seq input in
        let ys = Array.to_seq output in
        let points =
          Seq.map2 (fun x ys -> Array.map (fun y -> Plot.r2 x y) ys) xs ys
          |> List.of_seq |> Array.concat |> Data.of_array
        in
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
      (fun i (output : float array array) ->
        let style = style opts i in
        let xs = Array.to_seq input_x in
        let ys = Array.to_seq input_y in
        let zs = Array.to_seq output in
        let points =
          map3 (fun x y zs -> Array.map (fun z -> Plot.r3 x y z) zs) xs ys zs
          |> List.of_seq |> Array.concat |> Data.of_array
        in
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
      let rows = Array.length @@ Stdlib.List.hd outputs in
      let column = ("constant axis", Array.init rows (fun _ -> 0.)) in
      let plot = scatterplot_2d opts title column outputs in
      return [plot]
  | [column] ->
      let plot = scatterplot_2d opts title column outputs in
      return [plot]
  | [column1; column2] ->
      let plot = scatterplot_3d opts title column1 column2 outputs in
      return [plot]
  | _ ->
      if opts.reduced_plot_verbosity then return []
      else
        let subsets = Stats.Combi.enumerate_subsets 2 input_columns in
        let plots =
          List.map
            (function
              | [((dim1, _) as col1); ((dim2, _) as col2)] ->
                  let dim1 = underscore_to_dash dim1 in
                  let dim2 = underscore_to_dash dim2 in
                  let title = Format.asprintf "%s\\n(%s, %s)" title dim1 dim2 in
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

(* Plotting the full dataset makes plot generation slow. We bin the data
   and plot nonempty bins. [bin_data] works in linear time. *)
let bin_data bins data =
  let min, max =
    Array.fold_left
      (fun (min, max) x -> (Float.min x min, Float.max x max))
      (Float.max_float, -.Float.max_float)
      data
  in
  let range = max -. min in
  let width = range /. float_of_int bins in
  let spec = Stats.Binning.regular ~origin:min ~width ~truncate:None in
  let binned = Stats.Binning.from_empirical spec data in
  let acc = ref [] in
  Stats.Fin.Float.iter_mes binned (fun bin qty ->
      match Stats.Binning.map_from_bin spec bin with
      | None ->
          (* Cannot happen because [truncate] is set to [None]*)
          assert false
      | Some x when qty > 0.0 -> acc := x :: !acc
      | _ -> ()) ;
  Array.of_list !acc

let process_empirical_data empirical_plot data =
  match empirical_plot with
  | Empirical_plot_full ->
      if Array.length data > 500 then bin_data 500 data else data
  | Empirical_plot_quantiles qs ->
      List.map (fun q -> Stats.Emp.quantile (module Float) data q) qs
      |> Array.of_list

let empirical_data opts
    (workload_data : (Sparse_vec.String.t * float array) list) =
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
        "Display.empirical_data: variables not named consistently@."
  | [vars] ->
      let rows = List.length samples in
      let input_dims = List.length vars in
      let columns = Array.make_matrix input_dims rows 0.0 in
      let timings = Array.make rows [||] in
      List.iteri
        (fun i {workload; qty} ->
          assert (Compare.List_length_with.(workload = input_dims)) ;
          List.iteri
            (fun input_dim (_, size) -> columns.(input_dim).(i) <- size)
            workload ;
          timings.(i) <- process_empirical_data opts.empirical_plot qty)
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

let scores_to_string =
  let pp_tvalues =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
      (fun fmt (v, f) ->
        Format.fprintf
          fmt
          "%s = %.1f"
          (Free_variable.to_namespace v
          |> Namespace.basename |> underscore_to_dash)
          f)
  in
  fun scores ->
    let Inference.{r2_score; rmse_score; tvalues} = scores in
    Format.asprintf
      "R2-score = %s, RMSE-score = %.3f\\nT-values: %a"
      (match r2_score with None -> "None" | Some f -> Format.sprintf "%3f" f)
      rmse_score
      pp_tvalues
      tvalues

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
      let rows = Maths.row_dim output in
      let timings = Array.make rows [||] in
      for i = 0 to rows - 1 do
        let row = Maths.vector_to_array (Maths.Matrix.row output i) in
        timings.(i) <- process_empirical_data opts.empirical_plot row
      done ;
      let predicted =
        vector_to_array (Maths.Matrix.col predicted 0)
        |> Array.map (fun x -> [|x|])
      in
      let* plots =
        plot_scatter
          opts
          (Format.sprintf
             "Validation (chosen basis)\\n%s"
             (scores_to_string solution.scores))
          columns
          [timings; predicted]
      in
      return plots

let is_trivial_workload workload = Array.for_all (fun x -> x = 1.0) workload

let empirical opts (workload_data : (Sparse_vec.String.t * float array) list) =
  let open Result_syntax in
  if opts.reduced_plot_verbosity then return []
  else
    let* columns, timings = empirical_data opts workload_data in
    (* If the data is non-trivial, we produce a scatter plot.
       Otherwise, we produce a histogram of the data. *)
    match columns with
    | [(name, workload)] when is_trivial_workload workload ->
        let data = timings |> Array.to_list |> Array.concat in
        let std = Maths.std (Maths.vector_of_array data) in
        let points = data |> Array.map Plot.r1 |> Data.of_array in
        let plot =
          plot2
            ~xaxis:name
            ~yaxis:"freq"
            ~title:"Empirical (histogram)"
            [Histogram.hist ~binwidth:(std *. 0.1) ~points ()]
        in
        return [plot]
    | _ ->
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
  let* columns, timings = empirical_data opts workload_data in
  let* plots =
    plot_scatter
      opts
      (Format.sprintf
         "Validation (raw)\\n%s"
         (scores_to_string solution.scores))
      columns
      [timings; predicted |> Array.map (fun x -> [|x|])]
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
  let filename_prefix ?index kind =
    let dir = options.save_directory in
    let kind =
      match index with None -> kind | Some i -> Format.asprintf "%s-%d" kind i
    in
    let bench_name =
      match List.rev (Namespace.to_list Bench.name) with
      | "intercept" :: name :: _ -> name ^ "__intercept"
      | name :: _ -> name
      | [] -> assert false
    in
    Filename.Infix.(
      dir
      // Format.asprintf
           "%s_%s_%s"
           bench_name
           (Benchmark_helpers.filename_of_local_model_name model_name)
           kind)
  in
  let workload_data =
    List.map
      (fun {Measure.workload; measures; _} ->
        (Bench.workload_to_vector workload, Maths.vector_to_array measures))
      measurement.workload_data
  in
  let get_targets =
    let res = ref None in
    fun () ->
      match !res with
      | Some res -> res
      | None ->
          let targets = Plot.get_targets () in
          res := Some targets ;
          targets
  in
  let try_plot plot_target kind plot_result =
    match plot_result with
    | Ok [] -> []
    | Ok plots -> (
        match plot_target with
        | Save ->
            List.mapi
              (fun i plot ->
                let prefix = filename_prefix ~index:i kind in
                let pdf_file = prefix ^ ".pdf" in
                let target = pdf ?cm_size:(pdf_cm_size options) ~pdf_file () in
                Plot.run ~target ~detach:true plot ;
                pdf_file)
              plots
        | Show ->
            let target =
              match get_targets () with
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
            let plot_file = filename_prefix kind ^ ".plot" in
            Plot.run_matrix ~target ~detach:true plots ~filename:plot_file ;
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
