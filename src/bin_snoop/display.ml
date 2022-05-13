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

open Pyplot

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
  match i with
  | 0 -> Plot.(Blue, Dot)
  | 1 -> Plot.(Red, Square)
  | 2 -> Plot.(Green, Triangle)
  | _ -> Stdlib.failwith "Display.style: style overflow"

let scatterplot_2d title (name, input) outputs =
  let data =
    List.mapi
      (fun i output -> (Plot.Dim2Scatter {xs = input; ys = output}, style i))
      outputs
  in
  let plot : Plot.dim2 Plot.scatter =
    let open Plot in
    Scatter {data; axes = Dim2Axes {xaxis = name; yaxis = "timing"}; title}
  in
  `Dim2 (Plot.scatter plot)

let scatterplot_3d title (name_x, input_x) (name_y, input_y) outputs =
  let data =
    List.mapi
      (fun i output ->
        let sty = style i in
        (Plot.Dim3Scatter {xs = input_x; ys = input_y; zs = output}, sty))
      outputs
  in
  let plot : Plot.dim3 Plot.scatter =
    Scatter
      {
        data;
        axes = Dim3Axes {xaxis = name_x; yaxis = name_y; zaxis = "timing"};
        title;
      }
  in
  `Dim3 (Plot.scatter plot)

(* Scatter plot/s/ of the input vectors specified by [input_columns]
   against the [outputs]. This will superpose [List.length outputs]
   scatter plots on the same page. *)
let plot_scatter title input_columns outputs =
  match input_columns with
  | [] ->
      let msg =
        Format.asprintf "Display.plot_scatter (%s): empty scatter data" title
      in
      Error msg
  | [column] ->
      let plot = scatterplot_2d title column outputs in
      Ok [plot]
  | [column1; column2] ->
      let plot = scatterplot_3d title column1 column2 outputs in
      Ok [plot]
  | _ ->
      let subsets = Benchmark_helpers.enumerate_subsets 2 input_columns in
      let plots =
        List.map
          (function
            | [((dim1, _) as col1); ((dim2, _) as col2)] ->
                let title = Format.asprintf "%s\n(%s, %s)" title dim1 dim2 in
                scatterplot_3d title col1 col2 outputs
            | _ -> assert false)
          subsets
      in
      Ok plots

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
      let msg =
        Format.asprintf
          "Display.empirical_data: variables not named consistenly@."
      in
      Error msg
  | [vars] ->
      let rows = List.length samples in
      let input_dims = List.length vars in
      let columns =
        Array.init input_dims (fun _ -> Matrix.create ~lines:rows ~cols:1)
      in
      let timings = Matrix.create ~lines:rows ~cols:1 in
      List.iteri
        (fun i {workload; qty} ->
          assert (Compare.List_length_with.(workload = input_dims)) ;
          List.iteri
            (fun input_dim (_, size) -> Matrix.set columns.(input_dim) i 0 size)
            workload ;
          Matrix.set timings i 0 qty)
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

let rec plot_stacked :
    int ->
    int ->
    [> `Dim2 of (unit, Pyplot.Plot.dim2) Pyplot.Plot.Axis.t
    | `Dim3 of (unit, Pyplot.Plot.dim3) Pyplot.Plot.Axis.t ]
    list ->
    unit Plot.t =
 fun row col plots ->
  match plots with
  | [] -> Plot.return ()
  | `Dim2 ax :: tl ->
      let open Plot in
      let* () = subplot_2d ~row ~col ax in
      plot_stacked (row + 1) col tl
  | `Dim3 ax :: tl ->
      let open Plot in
      let* () = subplot_3d ~row ~col ax in
      plot_stacked (row + 1) col tl

let validator (problem : Inference.problem) (solution : Inference.solution) =
  match problem with
  | Inference.Degenerate _ -> Error "Display.validator: degenerate plot"
  | Inference.Non_degenerate {input; _} ->
      let {Inference.weights; _} = solution in
      let predicted = Matrix.numpy_mul input weights in
      let columns, timings = prune_problem problem in
      let columns =
        List.map
          (fun (c, m) -> (Format.asprintf "%a" Free_variable.pp c, m))
          columns
      in
      Result.bind
        (plot_scatter "Validation (chosen basis)" columns [timings; predicted])
        (fun plots ->
          let nrows = List.length plots in
          let plot ~col = plot_stacked 0 col plots in
          Result.ok (nrows, plot))

let empirical (workload_data : (Sparse_vec.String.t * float) list) :
    (int * (col:int -> unit Plot.t), string) result =
  let open Result_syntax in
  let* columns, timings = empirical_data workload_data in
  let* plots = plot_scatter "Empirical" columns [timings] in
  let nrows = List.length plots in
  Ok (nrows, fun ~col -> plot_stacked 0 col plots)

let eval_mset (mset : Free_variable.Sparse_vec.t)
    (eval : Free_variable.t -> float) =
  Free_variable.Sparse_vec.fold
    (fun var coeff acc -> acc +. (eval var *. coeff))
    mset
    0.0

let eval_affine (aff : Costlang.affine) (eval : Free_variable.t -> float) =
  eval_mset aff.linear_comb eval +. aff.const

let validator_empirical workload_data (problem : Inference.problem)
    (solution : Inference.solution) :
    (int * (col:int -> unit Plot.t), string) result =
  let {Inference.mapping; _} = solution in
  let valuation name =
    WithExceptions.Option.get ~loc:__LOC__
    @@ List.assoc ~equal:Free_variable.equal name mapping
  in
  let predicted =
    match problem with
    | Inference.Degenerate {predicted; _} -> predicted
    | Inference.Non_degenerate {lines; _} ->
        let predicted_list =
          List.map
            (fun ols_line ->
              let (Inference.Full (affine, _)) = ols_line in
              eval_affine affine valuation)
            lines
        in
        let array = Array.of_list predicted_list in
        Matrix.init ~lines:(Array.length array) ~cols:1 ~f:(fun l _ ->
            array.(l))
  in
  Result.bind (empirical_data workload_data) @@ fun (columns, timings) ->
  Result.bind (plot_scatter "Validation (raw)" columns [timings; predicted])
  @@ fun plots ->
  let nrows = List.length plots in
  let plot ~col = plot_stacked 0 col plots in
  Result.ok (nrows, plot)

type plot_target =
  | Save of {file : string option}
  | Show
  | ShowAndSave of {file : string option}

let perform_plot ~measure ~model_name ~problem ~solution ~plot_target =
  Pyinit.pyinit () ;
  let (Measure.Measurement ((module Bench), measurement)) = measure in
  let filename kind =
    Format.asprintf "%s_%s_%s.pdf" Bench.name model_name kind
  in
  let main_plot_opt =
    let workload_data =
      List.map
        (fun {Measure.workload; qty} ->
          (Bench.workload_to_vector workload, qty))
        measurement.workload_data
    in
    let current_col = ref 0 in
    let max_rows = ref 0 in
    let with_col f =
      let col = !current_col in
      incr current_col ;
      f col
    in
    let empirical =
      Result.fold
        (empirical workload_data)
        ~ok:(fun (rows, plot) ->
          max_rows := max !max_rows rows ;
          with_col (fun col -> plot ~col))
        ~error:(fun _err -> Plot.return ())
    in
    let validator =
      Result.fold
        (validator problem solution)
        ~ok:(fun (rows, plot) ->
          max_rows := max !max_rows rows ;
          with_col (fun col -> plot ~col))
        ~error:(fun _err -> Plot.return ())
    in
    let validator_emp =
      Result.fold
        (validator_empirical workload_data problem solution)
        ~ok:(fun (rows, plot) ->
          max_rows := max !max_rows rows ;
          with_col (fun col -> plot ~col))
        ~error:(fun _err -> Plot.return ())
    in
    if !current_col = 0 then (* Nothing to plot. *)
      None
    else
      let plot =
        let open Plot in
        let* () = empirical in
        let* () = validator in
        validator_emp
      in
      Some (!max_rows, !current_col, plot)
  in
  match main_plot_opt with
  | None -> false
  | Some (nrows, ncols, plot) ->
      Plot.run
        ~nrows
        ~ncols
        (let open Plot in
        let* () = plot in
        let* () = suptitle ~title:Bench.name ~fontsize:None in
        match plot_target with
        | Save {file} -> (
            match file with
            | None ->
                savefig ~filename:(filename "collected") ~dpi:3000 ~quality:95
            | Some filename -> savefig ~filename ~dpi:3000 ~quality:95)
        | Show ->
            Plot.(
              let* () = show () in
              return ())
        | ShowAndSave {file} -> (
            match file with
            | None ->
                Plot.(
                  let* () = show () in
                  savefig ~filename:(filename "collected") ~dpi:3000 ~quality:95)
            | Some filename ->
                Plot.(
                  let* () = show () in
                  savefig ~filename ~dpi:3000 ~quality:95))) ;
      true
