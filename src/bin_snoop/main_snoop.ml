(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

module Hashtbl = Stdlib.Hashtbl

(* ------------------------------------------------------------------------- *)
(* Listing available models, solvers, benchmarks *)

let list_all_models formatter =
  List.iter
    (fun name -> Format.fprintf formatter "%s@." name)
    (Registration.all_model_names ())

let list_solvers formatter =
  Format.fprintf formatter "ridge --ridge-alpha=<float>@." ;
  Format.fprintf formatter "lasso --lasso-alpha=<float> --lasso-positive@." ;
  Format.fprintf formatter "nnls@."

let list_all_benchmarks formatter =
  List.iter
    (fun (module Bench : Benchmark.S) ->
      Format.fprintf formatter "%s: %s\n" Bench.name Bench.info)
    (Registration.all_benchmarks ())

(* -------------------------------------------------------------------------- *)
(* Built-in commands implementations *)

let benchmark_assoc name = Registration.find_benchmark name

let benchmark_cmd (bench_name : string) (bench_opts : Cmdline.benchmark_options)
    =
  let bench =
    match benchmark_assoc bench_name with
    | None ->
        Format.eprintf
          "Benchmark <%s> is unknown. Available benchmarks:@."
          bench_name ;
        list_all_benchmarks Format.err_formatter ;
        exit 1
    | Some b -> b
  in
  Format.eprintf
    "Benchmarking with the following options:@.%s@."
    (Commands.Benchmark_cmd.benchmark_options_to_string bench_opts) ;
  let bench = Benchmark.ex_unpack bench in
  match bench with
  | Tezos_benchmark.Benchmark.Ex bench ->
      let workload_data = Measure.perform_benchmark bench_opts.options bench in
      Option.iter
        (fun filename -> Measure.to_csv ~filename ~bench ~workload_data)
        bench_opts.csv_export ;
      Measure.save
        ~filename:bench_opts.save_file
        ~options:bench_opts.options
        ~bench
        ~workload_data

let rec infer_cmd model_name workload_data solver infer_opts =
  Pyinit.pyinit () ;
  let file_stats = Unix.stat workload_data in
  match file_stats.st_kind with
  | S_DIR ->
      (* User specified a directory. Automatically process all workload data in that directory. *)
      infer_cmd_full_auto model_name workload_data solver infer_opts
  | S_REG ->
      (* User specified a workload data file. Only process that file. *)
      infer_cmd_one_shot model_name workload_data solver infer_opts
  | _ ->
      Format.eprintf
        "Error: %s is neither a regular file nor a directory.@."
        workload_data ;
      exit 1

and infer_cmd_one_shot model_name workload_data solver
    (infer_opts : Cmdline.infer_parameters_options) =
  let measure = Measure.load ~filename:workload_data in
  match measure with
  | Measure.Measurement
      ((module Bench), {bench_opts = _; workload_data; date = _}) ->
      let model =
        match List.assoc_opt ~equal:String.equal model_name Bench.models with
        | Some m -> m
        | None ->
            Format.eprintf "Requested model: \"%s\" not found@." model_name ;
            Format.eprintf
              "Available for this workload: @[%a@] @."
              (Format.pp_print_list
                 ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ", ")
                 Format.pp_print_string)
              (List.map fst Bench.models) ;
            exit 1
      in
      let overrides_map =
        match infer_opts.override_files with
        | None -> Free_variable.Map.empty
        | Some filenames -> Override.load ~filenames
      in
      let overrides name = Free_variable.Map.find name overrides_map in
      let problem =
        Inference.make_problem ~data:workload_data ~model ~overrides
      in
      if infer_opts.print_problem then (
        Format.eprintf "Dumping problem to stdout as requested by user@." ;
        Csv.export_stdout (Inference.problem_to_csv problem)) ;
      (match problem with
      | Inference.Degenerate {predicted; measured} ->
          let err = Inference.compute_error_statistics ~predicted ~measured in
          Format.printf
            "Error statistics:@.%a@."
            Inference.pp_error_statistics
            err
      | _ -> ()) ;
      let solver = solver_of_string solver infer_opts in
      let solution = Inference.solve_problem problem solver in
      let () =
        let perform_report () =
          let report =
            Report.add_section
              ~measure
              ~model_name
              ~problem
              ~solution
              ~overrides_map
              ~short:false
              (Report.create_empty ~name:"Report")
          in
          Report.to_latex report
        in
        match infer_opts.report with
        | Cmdline.NoReport -> ()
        | Cmdline.ReportToStdout ->
            let s = perform_report () in
            Format.printf "%s" s
        | Cmdline.ReportToFile output_file ->
            let s = perform_report () in
            Lwt_main.run
              (let open Lwt_syntax in
              let* _nwritten = Lwt_utils_unix.create_file output_file s in
              Lwt.return_unit) ;
            Format.eprintf "Produced report on %s@." output_file
      in
      process_output measure model_name problem solution infer_opts

and infer_cmd_full_auto model_name workload_data solver
    (infer_opts : Cmdline.infer_parameters_options) =
  let workload_files = get_all_workload_data_files workload_data in
  let overrides_map =
    match infer_opts.override_files with
    | None -> Free_variable.Map.empty
    | Some filenames -> Override.load ~filenames
  in
  let report_folder =
    match infer_opts.report with
    | Cmdline.ReportToFile s -> Some (Filename.dirname s)
    | _ -> None
  in
  let solver = solver_of_string solver infer_opts in
  let (graph, measurements) = Dep_graph.load_files model_name workload_files in
  if Dep_graph.G.is_empty graph then (
    Format.eprintf "Empty dependency graph.@." ;
    exit 1) ;
  Format.eprintf "Performing topological run@." ;
  let report =
    match infer_opts.report with
    | Cmdline.NoReport -> None
    | _ -> Some (Report.create_empty ~name:"Report")
  in
  Option.iter
    (fun filename ->
      let oc = open_out filename in
      Dep_graph.D.output_graph oc graph ;
      close_out oc)
    infer_opts.dot_file ;
  let (map, report) =
    Dep_graph.T.fold
      (fun workload_file (overrides_map, report) ->
        Format.eprintf "Processing: %s@." workload_file ;
        let measure = Hashtbl.find measurements workload_file in
        let overrides var = Free_variable.Map.find var overrides_map in
        let (Measure.Measurement ((module Bench), m)) = measure in
        let model =
          match Dep_graph.find_model_or_generic model_name Bench.models with
          | None ->
              Format.eprintf
                "No valid model (%s or generic) found in file %s. Availble \
                 models:.@."
                model_name
                workload_file ;
              list_all_models Format.err_formatter ;
              exit 1
          | Some model -> model
        in
        let problem =
          Inference.make_problem ~data:m.Measure.workload_data ~model ~overrides
        in
        let solution = Inference.solve_problem problem solver in
        let report =
          Option.map
            (Report.add_section
               ~measure
               ~model_name
               ~problem
               ~solution
               ~overrides_map
               ?report_folder
               ~short:true)
            report
        in
        let overrides_map =
          List.fold_left
            (fun map (variable, solution) ->
              Format.eprintf
                "Adding solution %a := %f@."
                Free_variable.pp
                variable
                solution ;
              Free_variable.Map.add variable solution map)
            overrides_map
            solution.mapping
        in
        perform_plot measure model_name problem solution infer_opts ;
        perform_csv_export solution infer_opts ;
        (overrides_map, report))
      graph
      (overrides_map, report)
  in
  perform_save_solution map infer_opts ;
  match (infer_opts.report, report) with
  | (Cmdline.NoReport, _) -> ()
  | (Cmdline.ReportToStdout, Some report) ->
      let s = Report.to_latex report in
      Format.printf "%s" s
  | (Cmdline.ReportToFile output_file, Some report) ->
      let s = Report.to_latex report in
      Lwt_main.run
        (let open Lwt_syntax in
        let* _nwritten = Lwt_utils_unix.create_file output_file s in
        Lwt.return_unit) ;
      Format.eprintf "Produced report on %s@." output_file
  | _ -> assert false

and solver_of_string (solver : string)
    (infer_opts : Cmdline.infer_parameters_options) =
  match solver with
  | "ridge" ->
      Inference.Ridge {alpha = infer_opts.ridge_alpha; normalize = false}
  | "lasso" ->
      Inference.Lasso
        {
          alpha = infer_opts.lasso_alpha;
          normalize = false;
          positive = infer_opts.lasso_positive;
        }
  | "nnls" -> Inference.NNLS
  | _ ->
      Format.eprintf "Unknown solver name.@." ;
      list_solvers Format.err_formatter ;
      exit 1

and process_output measure model_name problem solution infer_opts =
  perform_csv_export solution infer_opts ;
  let map = Free_variable.Map.of_seq (List.to_seq solution.mapping) in
  perform_save_solution map infer_opts ;
  perform_plot measure model_name problem solution infer_opts

and perform_csv_export solution (infer_opts : Cmdline.infer_parameters_options)
    =
  match infer_opts.csv_export with
  | None -> ()
  | Some filename -> (
      let solution_csv_opt = Inference.solution_to_csv solution in
      match solution_csv_opt with
      | None -> ()
      | Some solution_csv -> Csv.append_columns ~filename solution_csv)

and perform_save_solution (solution : float Free_variable.Map.t)
    (infer_opts : Cmdline.infer_parameters_options) =
  match infer_opts.save_solution with
  | None -> ()
  | Some filename ->
      Codegen.save_solution solution filename ;
      Format.eprintf "Saved solution to %s@." filename

and perform_plot measure model_name problem solution
    (infer_opts : Cmdline.infer_parameters_options) =
  if infer_opts.plot then
    if
      Display.perform_plot
        ~measure
        ~model_name
        ~problem
        ~solution
        ~plot_target:Display.Show
    then ()
    else Format.eprintf "Could not plot given data."
  else ()

and get_all_workload_data_files directory =
  let is_workload_data file =
    let regexp = Str.regexp ".*\\.workload" in
    Str.string_match regexp file 0
  in
  let lift file = directory ^ "/" ^ file in
  let handle = Unix.opendir directory in
  let rec loop acc =
    match Unix.readdir handle with
    | file ->
        if is_workload_data file then loop (lift file :: acc) else loop acc
    | exception End_of_file ->
        Unix.closedir handle ;
        acc
  in
  loop []

let cull_outliers_cmd workload_data nsigmas save_file =
  let measure = Measure.load ~filename:workload_data in
  match measure with
  | Measure.Measurement (bench, {bench_opts; workload_data; date = _}) ->
      let workload_data = Measure.cull_outliers ~nsigmas workload_data in
      Measure.save ~filename:save_file ~options:bench_opts ~bench ~workload_data

let codegen_cmd solution model_name codegen_options =
  let sol = Codegen.load_solution solution in
  match Registration.find_model model_name with
  | None ->
      Format.eprintf "Model %s not found, exiting@." model_name ;
      exit 1
  | Some model ->
      let transform =
        match codegen_options with
        | Cmdline.No_transform ->
            ((module Costlang.Identity) : Costlang.transform)
        | Cmdline.Fixed_point_transform options ->
            let module P = struct
              let options = options
            end in
            let module Transform = Fixed_point_transform.Apply (P) in
            ((module Transform) : Costlang.transform)
      in
      let code =
        match Codegen.codegen model sol transform with
        | exception e ->
            Format.eprintf
              "Error in code generation for model %s, exiting@."
              model_name ;
            Format.eprintf "Exception caught: %s@." (Printexc.to_string e) ;
            exit 1
        | None ->
            Format.eprintf "Code generation failed. Bad model? Exiting.@." ;
            exit 1
        | Some s -> s
      in
      Format.printf "let model_%s = %a@." model_name Codegen.pp_expr code

let codegen_all_cmd solution regexp codegen_options =
  let () = Format.eprintf "regexp: %s@." regexp in
  let regexp = Str.regexp regexp in
  let ok (name, _) = Str.string_match regexp name 0 in
  let sol = Codegen.load_solution solution in
  let models = List.filter ok (Registration.all_registered_models ()) in
  let transform =
    match codegen_options with
    | Cmdline.No_transform -> ((module Costlang.Identity) : Costlang.transform)
    | Cmdline.Fixed_point_transform options ->
        let module P = struct
          let options = options
        end in
        let module Transform = Fixed_point_transform.Apply (P) in
        ((module Transform) : Costlang.transform)
  in
  let result = Codegen.codegen_module models sol transform in
  Codegen.pp_structure_item Format.std_formatter result

(* -------------------------------------------------------------------------- *)
(* Entrypoint *)

(* Activate logging system. *)
let () =
  Lwt_main.run
  @@ Tezos_base_unix.Internal_event_unix.(
       init
         ~lwt_log_sink:Lwt_log_sink_unix.default_cfg
         ~configuration:Configuration.default)
       ()

let () =
  if Commands.list_solvers then list_solvers Format.std_formatter ;
  if Commands.list_models then list_all_models Format.std_formatter

let () =
  match !Cmdline.commandline_outcome_ref with
  | None -> ()
  | Some outcome -> (
      match outcome with
      | Cmdline.No_command -> exit 0
      | Cmdline.Cull_outliers {workload_data; nsigmas; save_file} ->
          cull_outliers_cmd workload_data nsigmas save_file
      | Cmdline.Benchmark {bench_name; bench_opts} ->
          benchmark_cmd bench_name bench_opts
      | Cmdline.Infer {model_name; workload_data; solver; infer_opts} ->
          infer_cmd model_name workload_data solver infer_opts
      | Cmdline.Codegen {solution; model_name; codegen_options} ->
          codegen_cmd solution model_name codegen_options
      | Cmdline.Codegen_all {solution; matching; codegen_options} ->
          codegen_all_cmd solution matching codegen_options)
