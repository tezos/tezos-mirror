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

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/4025
   Remove backwards compatible Tezos symlinks. *)
let () =
  (* warn_if_argv0_name_not_octez *)
  let executable_name = Filename.basename Sys.argv.(0) in
  let prefix = "tezos-" in
  if TzString.has_prefix executable_name ~prefix then
    let expected_name =
      let len_prefix = String.length prefix in
      "octez-"
      ^ String.sub
          executable_name
          len_prefix
          (String.length executable_name - len_prefix)
    in
    Format.eprintf
      "@[<v 2>@{<warning>@{<title>Warning@}@}@,\
       The executable with name @{<kwd>%s@} has been renamed to @{<kwd>%s@}. \
       The name @{<kwd>%s@} is now@,\
       deprecated, and it will be removed in a future release. Please update@,\
       your scripts to use the new name.@]@\n\
       @."
      executable_name
      expected_name
      executable_name
  else ()

(* ------------------------------------------------------------------------- *)
(* Listing available models, solvers, benchmarks *)

let list_all_models formatter =
  List.iter
    (fun name -> Format.fprintf formatter "%a@." Namespace.pp name)
    (Registration.all_model_names ())

let list_solvers formatter =
  Format.fprintf formatter "ridge --ridge-alpha=<float>@." ;
  Format.fprintf formatter "lasso --lasso-alpha=<float> --lasso-positive@." ;
  Format.fprintf formatter "nnls@."

let list_benchmarks formatter list =
  List.iter
    (fun (module Bench : Benchmark.S) ->
      Format.fprintf formatter "%a: %s\n" Namespace.pp Bench.name Bench.info)
    list

let list_all_benchmarks formatter =
  list_benchmarks formatter (Registration.all_benchmarks () |> List.map snd)

(* -------------------------------------------------------------------------- *)
(* Built-in commands implementations *)

let perform_benchmark (bench_pattern : Namespace.t)
    (bench_opts : Cmdline.benchmark_options) =
  let bench =
    try Registration.find_benchmark_exn bench_pattern
    with Registration.Benchmark_not_found _ ->
      Format.eprintf "Available benchmarks:@." ;
      list_all_benchmarks Format.err_formatter ;
      exit 1
  in
  let (module Bench) = bench in
  Format.eprintf
    "@[<2>Benchmarking %a with the following options:@ @[%a@]@]@."
    Namespace.pp
    Bench.name
    Commands.Benchmark_cmd.pp_benchmark_options
    bench_opts ;
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

let benchmark_cmd bench_pattern bench_opts =
  ignore @@ perform_benchmark bench_pattern bench_opts

let is_constant_input (type a t) (bench : (a, t) Benchmark.poly) workload_data =
  let module Bench = (val bench) in
  List.map
    (fun Measure.{workload; _} -> Bench.workload_to_vector workload)
    workload_data
  |> List.all_equal Sparse_vec.String.equal

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
      let is_constant_input = is_constant_input (module Bench) workload_data in
      let solution =
        Inference.solve_problem ~is_constant_input problem solver
      in
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
              ~display_options:infer_opts.display
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
  let graph, measurements =
    Dep_graph.load_workload_files ~model_name workload_files
  in
  if Dep_graph.Graph.is_empty graph then (
    Format.eprintf "Empty dependency graph.@." ;
    exit 1) ;
  Option.iter
    (fun filename -> Dep_graph.Graph.save_graphviz graph filename)
    infer_opts.dot_file ;
  Format.eprintf "Performing topological run@." ;
  let solution = Dep_graph.Graph.to_sorted_list graph in
  ignore
  @@ infer_for_measurements ~model_name measurements solution ~solver infer_opts

and infer_for_measurements ~model_name measurements
    (solved_list :
      Dep_graph.Solver.Solved.t list (* sorted in the topological order *))
    ~solver (infer_opts : Cmdline.infer_parameters_options) =
  let overrides_map =
    match infer_opts.override_files with
    | None -> Free_variable.Map.empty
    | Some filenames -> Override.load ~filenames
  in
  let display_options =
    match infer_opts.report with
    | Cmdline.ReportToFile s ->
        {infer_opts.display with Display.save_directory = Filename.dirname s}
    | _ -> infer_opts.display
  in
  let solver = solver_of_string solver infer_opts in
  let report =
    match infer_opts.report with
    | Cmdline.NoReport -> None
    | _ -> Some (Report.create_empty ~name:"Report")
  in
  let scores_list = [] in
  let map, scores_list, report =
    List.fold_left
      (fun (overrides_map, scores_list, report) solved ->
        let measure =
          Stdlib.Option.get
          @@ Namespace.Hashtbl.find
               measurements
               solved.Dep_graph.Solver.Solved.name
        in
        let (Measure.Measurement ((module Bench), m)) = measure in
        (* Run inference of the models only bound by the local [model_name] *)
        Option.fold
          (Dep_graph.find_model_or_generic model_name Bench.models)
          ~none:(overrides_map, scores_list, report)
          ~some:(fun model ->
            Format.eprintf
              "Running inference for %a (model_name: %s)@."
              Namespace.pp
              solved.Dep_graph.Solver.Solved.name
              model_name ;
            Format.eprintf "  @[%a@]@." Dep_graph.Solver.Solved.pp solved ;
            let overrides var = Free_variable.Map.find var overrides_map in
            let problem =
              Inference.make_problem
                ~data:m.Measure.workload_data
                ~model
                ~overrides
            in
            if infer_opts.print_problem then (
              Format.eprintf "Dumping problem to stdout as requested by user@." ;
              Csv.export_stdout (Inference.problem_to_csv problem)) ;
            let is_constant_input =
              is_constant_input (module Bench) m.Measure.workload_data
            in
            let solution =
              Inference.solve_problem ~is_constant_input problem solver
            in
            let report =
              Option.map
                (Report.add_section
                   ~measure
                   ~model_name
                   ~problem
                   ~solution
                   ~overrides_map
                   ~display_options
                   ~short:true)
                report
            in
            let overrides_map =
              List.fold_left
                (fun map (variable, solution) ->
                  if Free_variable.Set.mem variable solved.provides then (
                    Format.eprintf
                      "Adding solution %a := %f@."
                      Free_variable.pp
                      variable
                      solution ;
                    Free_variable.Map.add variable solution map)
                  else if Free_variable.Set.mem variable solved.dependencies
                  then (
                    (* Variables analyzed as dependencies inferred.  It is a bug
                       of the dependency analysis or the inference. *)
                    Format.eprintf
                      "ERROR: bug found.  A dependency variable is solved %a = \
                       %f@."
                      Free_variable.pp
                      variable
                      solution ;
                    exit 1)
                  else (
                    (* Variables eliminated at dependency analysis may not be gone
                       at the infernece. They have arbitrary solution
                       (in LASSO 0.0) and therefore must be ignored.

                       We should remove this case by fixing the expression used in
                       the inference.
                    *)
                    Format.eprintf
                      "@[<v2>Warning: ignoring a solution of an eliminated \
                       variable %a = %f@,\
                       It is safe to proceed but it may be caused by a bug of \
                       inference.@]@."
                      Free_variable.pp
                      variable
                      solution ;
                    map))
                overrides_map
                solution.mapping
            in
            (* solved.provides should be really solved by the inference *)
            Free_variable.Set.iter
              (fun fv ->
                if
                  not
                  @@ List.mem_assoc
                       ~equal:Free_variable.equal
                       fv
                       solution.mapping
                then
                  Format.eprintf
                    "@[<v2>Warning: a provided free variable %a is not solved \
                     by the inference.@,\
                     It is safe to proceed but it may be caused by a bug of \
                     dependency analysis.@]@."
                    Free_variable.pp
                    fv)
              solved.provides ;
            let scores_label = (model_name, Bench.name) in
            let scores_list = (scores_label, solution.scores) :: scores_list in
            perform_plot measure model_name problem solution infer_opts ;
            perform_csv_export scores_label solution infer_opts ;
            (overrides_map, scores_list, report)))
      (overrides_map, scores_list, report)
      solved_list
  in
  let solution =
    Codegen.{inference_model_name = model_name; map; scores_list}
  in
  perform_save_solution solution infer_opts ;
  (match (infer_opts.report, report) with
  | Cmdline.NoReport, _ -> ()
  | ReportToStdout, Some report ->
      let s = Report.to_latex report in
      Format.printf "%s" s
  | ReportToFile output_file, Some report ->
      let s = Report.to_latex report in
      Lwt_main.run
        (let open Lwt_syntax in
        let* _nwritten = Lwt_utils_unix.create_file output_file s in
        Lwt.return_unit) ;
      Format.eprintf "Produced report on %s@." output_file
  | _ -> assert false) ;
  solution

and solver_of_string solver (infer_opts : Cmdline.infer_parameters_options) =
  match solver with
  | "ridge" -> Inference.Ridge {alpha = infer_opts.ridge_alpha}
  | "lasso" ->
      Inference.Lasso
        {alpha = infer_opts.lasso_alpha; positive = infer_opts.lasso_positive}
  | "nnls" -> Inference.NNLS
  | _ ->
      Format.eprintf "Unknown solver name.@." ;
      list_solvers Format.err_formatter ;
      exit 1

and process_output measure model_name problem solution infer_opts =
  let (Measure.Measurement ((module Bench), _)) = measure in
  let scores_label = (model_name, Bench.name) in
  perform_csv_export scores_label solution infer_opts ;
  let map = Free_variable.Map.of_seq (List.to_seq solution.mapping) in
  perform_save_solution
    Codegen.
      {
        inference_model_name = model_name;
        map;
        scores_list = [(scores_label, solution.scores)];
      }
    infer_opts ;
  perform_plot measure model_name problem solution infer_opts

and perform_csv_export scores_label solution
    (infer_opts : Cmdline.infer_parameters_options) =
  match infer_opts.csv_export with
  | None -> ()
  | Some filename -> (
      let solution_csv_opt = Inference.solution_to_csv solution in
      match solution_csv_opt with
      | None -> ()
      | Some solution_csv ->
          let Inference.{scores; _} = solution in
          Csv.append_columns
            ~filename
            Inference.(scores_to_csv_column scores_label scores) ;
          Csv.append_columns ~filename solution_csv)

and perform_save_solution solution
    (infer_opts : Cmdline.infer_parameters_options) =
  match infer_opts.save_solution with
  | None -> ()
  | Some filename ->
      Codegen.(save_solution solution filename) ;
      Format.eprintf "Saved solution to %s@." filename

and perform_plot measure model_name problem solution
    (infer_opts : Cmdline.infer_parameters_options) =
  if infer_opts.plot then
    ignore
    @@ Display.perform_plot
         ~measure
         ~model_name
         ~problem
         ~solution
         ~plot_target:Display.Show
         ~options:infer_opts.Cmdline.display
  else ()

and get_all_workload_data_files directory =
  let is_workload_data = String.ends_with ~suffix:".workload" in
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

let stdout_or_file fn f =
  match fn with
  | None -> f Format.std_formatter
  | Some fn ->
      let oc = open_out fn in
      Fun.protect ~finally:(fun () -> close_out oc) @@ fun () ->
      f (Format.formatter_of_out_channel oc)

let codegen_cmd solution_fn model_name codegen_options =
  let sol = Codegen.load_solution solution_fn in
  match Registration.find_model model_name with
  | None ->
      Format.eprintf "Model %a not found, exiting@." Namespace.pp model_name ;
      exit 1
  | Some {Registration.model; _} ->
      let transform =
        match codegen_options.Cmdline.transform with
        | None -> ((module Costlang.Identity) : Costlang.transform)
        | Some options ->
            let module P = struct
              let options = options
            end in
            let module Transform = Fixed_point_transform.Apply (P) in
            ((module Transform) : Costlang.transform)
      in
      let code =
        match Codegen.codegen model sol transform model_name with
        | exception e ->
            Format.eprintf
              "Error in code generation for model %a, exiting@."
              Namespace.pp
              model_name ;
            Format.eprintf "Exception caught: %s@." (Printexc.to_string e) ;
            exit 1
        | s -> s
      in
      stdout_or_file codegen_options.save_to (fun ppf ->
          Format.fprintf ppf "%a@." Codegen.pp_code code)

let generate_code_for_models sol models codegen_options ~exclusions =
  (* The order of the models is pretty random.  It is better to sort them. *)
  let models =
    List.sort (fun (n1, _) (n2, _) -> Namespace.compare n1 n2) models
  in
  let transform =
    match codegen_options.Cmdline.transform with
    | None -> ((module Costlang.Identity) : Costlang.transform)
    | Some options ->
        let module P = struct
          let options = options
        end in
        let module Transform = Fixed_point_transform.Apply (P) in
        ((module Transform) : Costlang.transform)
  in
  Codegen.codegen_models models sol transform ~exclusions

let save_code_list_as_a_module save_to code_list =
  let result = Codegen.make_toplevel_module code_list in
  stdout_or_file save_to (fun ppf ->
      Format.fprintf ppf "%a@." Codegen.pp_module result)

let codegen_all_cmd solution_fn regexp codegen_options =
  let () = Format.eprintf "regexp: %s@." regexp in
  let regexp = Str.regexp regexp in
  let ok (name, _) = Str.string_match regexp (Namespace.to_string name) 0 in
  let sol = Codegen.load_solution solution_fn in
  let models = List.filter ok (Registration.all_models ()) in
  save_code_list_as_a_module codegen_options.Cmdline.save_to
  @@ generate_code_for_models
       sol
       models
       codegen_options (* no support of exclusions for this command *)
       ~exclusions:String.Set.empty

let codegen_for_a_solution solution codegen_options ~exclusions =
  let found_codegen_models =
    (* They include builtin/TIMER_LATENCY. *)
    List.filter
      Registration.(
        fun (_, {from; _}) ->
          List.exists
            (fun {local_model_name; _} ->
              solution.Codegen.inference_model_name = local_model_name)
            from)
      (Registration.all_models ())
  in

  let fvs_of_codegen_model model =
    let (Model.Model model) = model in
    let module Model = (val model) in
    let module FV = Model.Def (Costlang.Free_variables) in
    FV.model
  in

  let model_fvs_included_in_sol model =
    let fvs = fvs_of_codegen_model model in
    Free_variable.Set.for_all
      (fun fv -> Free_variable.Map.mem fv solution.map)
      fvs
  in

  (* Model's free variables must be included in the solution's keys *)
  let codegen_models =
    List.filter
      (fun (_model_name, {Registration.model; _}) ->
        model_fvs_included_in_sol model)
      found_codegen_models
  in
  generate_code_for_models solution codegen_models codegen_options ~exclusions

let save_codegen_for_solutions solutions codegen_options ~exclusions =
  save_code_list_as_a_module codegen_options.Cmdline.save_to
  @@ List.concat_map
       (fun solution ->
         codegen_for_a_solution solution codegen_options ~exclusions)
       solutions

let codegen_for_solutions_cmd solution_fns codegen_options ~exclusions =
  let solutions = List.map Codegen.load_solution solution_fns in
  save_codegen_for_solutions solutions codegen_options ~exclusions

let save_solutions_in_text out_fn nsolutions =
  stdout_or_file out_fn @@ fun ppf ->
  List.iter
    (fun (n, solution) ->
      Format.fprintf ppf "@[<2>%s:@ @[%a@]@]@." n Codegen.pp_solution solution)
    nsolutions

let solution_print_cmd out_fn solution_fns =
  save_solutions_in_text out_fn
  @@ List.map
       (fun solution_fn ->
         let solution = Codegen.load_solution solution_fn in
         (solution_fn, solution))
       solution_fns

let codegen_check_definitions_cmd files =
  let map =
    List.fold_left
      (fun acc fn ->
        match Codegen.Parser.get_cost_functions fn with
        | Ok vs ->
            Format.eprintf "%s have %d definitions@." fn (List.length vs) ;
            List.fold_left
              (fun acc v ->
                String.Map.update
                  v
                  (fun old -> Some (fn :: Option.value ~default:[] old))
                  acc)
              acc
              vs
        | Error exn ->
            Format.eprintf "%s: %s@." fn (Printexc.to_string exn) ;
            exit 1)
      String.Map.empty
      files
  in
  let fail =
    String.Map.fold
      (fun v fns fail ->
        match fns with
        | [] -> assert false (* impossible *)
        | [_] -> fail
        | fns ->
            Format.(
              eprintf
                "@[<2>%s: defined in multiple modules:@ @[<v>%a@]@]@."
                v
                (pp_print_list pp_print_string)
                fns) ;
            true)
      map
      false
  in
  if fail then exit 1 ;
  Format.eprintf "Good. No duplicated cost function definitions found@."

module Auto_build = struct
  (* Render a dot file to SVG.  It is optional and we do not care of any failure. *)
  let run_dot fn = ignore @@ Sys.command (Printf.sprintf "dot -Tsvg -O %s" fn)

  type state = {
    (* Free variables of a benchmark.
       When [measurement=None], it is just an approximation obtained by applying
       a sample workload. Once [measurement=Some _], it becomes precise since
       we can apply the actual workload. *)
    free_variables : Free_variable.Set.t;
    (* Free variables occur in the models of the benchmark without application of
       any workload. *)
    free_variables_without_workload : Free_variable.Set.t;
    (* if [Some _], [free_variables] is precise. *)
    measurement : Measure.packed_measurement option;
  }

  (* Get the dependency problem under the current state *)
  let get_problem state_tbl =
    Namespace.Hashtbl.fold
      (fun name state ->
        Dep_graph.Solver.Unsolved.build
          name
          ~fvs_unapplied:state.free_variables_without_workload
          state.free_variables
        |> List.cons)
      state_tbl
      []

  (* Perform the benchmark of name [bench_name] *)
  let benchmark outdir bench_name measure_options =
    let (module Bench) = Registration.find_benchmark_exn bench_name in
    (* override [measure_options] for intercept and TIMER_LATENCY *)
    let measure_options =
      match Namespace.basename bench_name with
      | "intercept" -> {measure_options with Measure.bench_number = 1}
      | "TIMER_LATENCY" ->
          {measure_options with bench_number = 1; nsamples = 10000}
      | _ -> measure_options
    in
    let save_file =
      Filename.concat outdir (Namespace.to_filename bench_name ^ ".workload")
    in
    let save_file_tmp = save_file ^ ".tmp" in
    let bench_options =
      {
        Commands.Benchmark_cmd.default_benchmark_options with
        save_file = save_file_tmp;
        options = measure_options;
      }
    in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/5471
       Need an option to force/skip rebench
    *)
    let measurement =
      if not @@ Sys.file_exists save_file then (
        let measurement = perform_benchmark bench_name bench_options in
        Unix.rename save_file_tmp save_file ;
        measurement)
      else Measure.load ~filename:save_file
    in
    let json_file = save_file ^ ".json" in
    Measure.packed_measurement_save_json measurement (Some json_file) ;
    measurement

  let init_state_tbl () =
    let state_tbl = Namespace.Hashtbl.create 513 in
    List.iter (fun (bench_name, bench) ->
        let free_variables_without_workload =
          Benchmark.get_free_variable_set bench
        in
        (* At this point, no workload(measurement) is loaded, and therefore
           we can only have small subset of the free variables of
           the benchmarks. *)
        let free_variables = free_variables_without_workload in
        Namespace.Hashtbl.replace
          state_tbl
          bench_name
          {free_variables; free_variables_without_workload; measurement = None})
    @@ Registration.all_benchmarks () ;
    state_tbl

  (* Running benchmarks until we reach a stable dependency graph

     We start from a small [state_tbl] with possibly incomplete free variable
     sets. They are completed on demand by running the corresponding benchmarks.
  *)
  let rec analyze_dependency measure_options outdir state_tbl
      free_variables_to_infer =
    let open Dep_graph in
    let open Solver.Solved in
    let module Fv_set = Free_variable.Set in
    let module Fv_map = Free_variable.Map in
    Format.eprintf
      "@[<2>Analyzing graph of @[%a@]@]@."
      Fv_set.pp
      free_variables_to_infer ;

    (* Analyze provides/dependencies under the current [state_tbl] *)
    let solution = Solver.solve @@ get_problem state_tbl in

    (* Benchmarks which seem to provide [free_variables_to_infer] *)
    let providers =
      List.filter
        (fun solved ->
          Fv_set.(not @@ disjoint solved.provides free_variables_to_infer))
        solution
    in

    (* Benchmark [providers] if not yet *)
    let run_benchmark all_required_benchmark_has_measurement solved =
      let bench_name = solved.name in
      let state =
        (* [solved] comes from [providers], which is a sub-list of [solution],
           the latter only containing elements from [state_tbl], so [find]
           always returns [Some]. *)
        Stdlib.Option.get @@ Namespace.Hashtbl.find state_tbl bench_name
      in
      match state.measurement with
      | Some _ ->
          all_required_benchmark_has_measurement (* Already benchmarked *)
      | None ->
          Format.eprintf "Benchmarking %a...@." Namespace.pp bench_name ;
          let measurement = benchmark outdir bench_name measure_options in
          (* Now we have the exact free variable set. *)
          let free_variables = Measure.get_free_variable_set measurement in
          let state =
            {state with measurement = Some measurement; free_variables}
          in
          Namespace.Hashtbl.replace state_tbl bench_name state ;
          false
    in
    let all_required_benchmark_has_measurement =
      List.fold_left run_benchmark true providers
    in

    if not all_required_benchmark_has_measurement then
      (* Recurse if [state_tbl] is updated by [run_benchmark] *)
      analyze_dependency
        measure_options
        outdir
        state_tbl
        free_variables_to_infer
    else
      (* Add the dependencies of [providers] to [free_variables_to_infer] *)
      let new_free_variables_to_infer =
        List.fold_left
          (fun acc solved -> Fv_set.union acc solved.dependencies)
          (* It can be [empty], but the monotonicity is better to make
             sure the algorithm terminates *)
          free_variables_to_infer
          providers
      in
      if not @@ Fv_set.equal new_free_variables_to_infer free_variables_to_infer
      then
        (* Recurse with the updated free variables to infer *)
        analyze_dependency
          measure_options
          outdir
          state_tbl
          new_free_variables_to_infer
      else (
        prerr_endline "Reached fixedpoint" ;
        let Graph.{resolved = _; with_ambiguities; providers_map} =
          Graph.build providers
        in
        (* Same as the above [providers] but sorted *)
        let providers =
          List.rev
          @@ Dep_graph.Graph.fold
               (fun solved acc -> solved :: acc)
               with_ambiguities
               []
        in
        (providers, providers_map))

  let infer mkfilename local_model_name measurements solution infer_opts =
    let solver = "lasso" in
    let csv_export = mkfilename ".sol.csv" in
    (* If [csv_export] already exists, it must be removed first,
       otherwise it fails at adding columns.
    *)
    if Sys.file_exists csv_export then Unix.unlink csv_export ;
    let solution_fn = mkfilename ".sol" in
    let dot_file = mkfilename ".dot" in
    let report_file = mkfilename ".tex" in
    let infer_opts =
      {
        infer_opts with
        Cmdline.csv_export = Some csv_export;
        save_solution = Some solution_fn;
        dot_file = Some dot_file;
        lasso_positive = true;
        report = ReportToFile report_file;
      }
    in
    let solution =
      infer_for_measurements
        ~model_name:local_model_name
        measurements
        solution
        ~solver
        infer_opts
    in
    save_solutions_in_text
      (Some (mkfilename ".sol.txt"))
      [(solution_fn, solution)] ;
    solution

  let codegen mkfilename solution =
    let codegen_options =
      Cmdline.{transform = None; save_to = Some (mkfilename "_non_fp.ml")}
    in
    save_codegen_for_solutions
      [solution]
      codegen_options
      ~exclusions:String.Set.empty ;
    let codegen_options =
      Cmdline.
        {
          transform =
            Some
              {
                Fixed_point_transform.default_options with
                max_relative_error = 0.5;
              };
          save_to = Some (mkfilename ".ml");
        }
    in
    save_codegen_for_solutions
      [solution]
      codegen_options
      ~exclusions:String.Set.empty

  let cmd bench_names
      Cmdline.{destination_directory; infer_parameters; measure_options} =
    let exitf status fmt =
      Format.kasprintf
        (fun s ->
          prerr_endline s ;
          exit status)
        fmt
    in
    let outdir =
      Option.value_f destination_directory ~default:(fun () ->
          exitf 1 "Need to specify --out-dir")
    in
    (* No non-lwt version available... *)
    Lwt_main.run (Lwt_utils_unix.create_dir outdir) ;

    let state_tbl = init_state_tbl () in

    let unknowns =
      List.filter
        (fun bench_name -> Registration.find_benchmark bench_name = None)
        bench_names
    in
    if unknowns <> [] then
      exitf
        1
        "@[<2>Error: unknown benchmark name(s):@ @[%a@]@]@."
        Format.(
          pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " ") Namespace.pp)
        unknowns ;

    (* Benchmark dependency analysis *)
    Format.eprintf "Analyzing the global benchmark dependency...@." ;
    let free_variables_to_infer =
      let benches = List.map Registration.find_benchmark_exn bench_names in
      List.fold_left
        (fun acc bench ->
          Free_variable.Set.union acc @@ Benchmark.get_free_variable_set bench)
        Free_variable.Set.empty
        benches
    in
    let providers, providers_map =
      analyze_dependency
        measure_options
        outdir
        state_tbl
        free_variables_to_infer
    in
    Format.eprintf
      "@[<v2>Required benchmarks:@ @[<v>%a@]@]@."
      Format.(pp_print_list ~pp_sep:pp_print_space Dep_graph.Solver.Solved.pp)
      providers ;
    let dot_file = Filename.concat outdir "dependency.dot" in
    Dep_graph.Graphviz.save dot_file providers ;
    run_dot dot_file ;
    Dep_graph.Graph.warn_ambiguities providers_map ;
    if Dep_graph.Graph.is_ambiguous providers_map then
      exitf 1 "Dependency graph is ambiguous. Exiting" ;

    let measurements =
      let tbl = Namespace.Hashtbl.create 101 in
      Namespace.Hashtbl.iter
        (fun ns state ->
          Option.iter (Namespace.Hashtbl.add tbl ns) state.measurement)
        state_tbl ;
      tbl
    in

    (* Inference and codegen per each local model name *)
    let local_model_names =
      let open String.Set in
      List.fold_left
        (fun acc solved ->
          let (module Bench : Benchmark.S) =
            Registration.find_benchmark_exn solved.Dep_graph.Solver.Solved.name
          in
          union acc @@ of_list @@ List.map fst Bench.models)
        empty
        providers
    in
    Pyinit.pyinit () ;
    String.Set.iter
      (function
        | "*" -> ()
        | local_model_name ->
            let mkfilename ext =
              Filename.concat
                outdir
                String.(concat "__" @@ split_on_char '/' local_model_name)
              ^ ext
            in
            (* Infernece *)
            let solution =
              infer
                mkfilename
                local_model_name
                measurements
                providers
                infer_parameters
            in
            (* Codegen *)
            codegen mkfilename solution)
      local_model_names
end

(* -------------------------------------------------------------------------- *)
(* Entrypoint *)

(* Activate logging system. *)
let () = Lwt_main.run @@ Tezos_base_unix.Internal_event_unix.init ()

let () =
  if Commands.list_solvers then list_solvers Format.std_formatter ;
  if Commands.list_models then list_all_models Format.std_formatter

let () =
  match !Cmdline.commandline_outcome_ref with
  | None -> ()
  | Some outcome -> (
      match outcome with
      | No_command -> exit 0
      | Benchmark {bench_name; bench_opts} ->
          benchmark_cmd bench_name bench_opts
      | Infer {model_name; workload_data; solver; infer_opts} ->
          infer_cmd model_name workload_data solver infer_opts
      | Codegen {solution; model_name; codegen_options} ->
          codegen_cmd solution model_name codegen_options
      | Codegen_all {solution; matching; codegen_options} ->
          codegen_all_cmd solution matching codegen_options
      | Codegen_inferred {solution; codegen_options; exclusions} ->
          codegen_for_solutions_cmd [solution] codegen_options ~exclusions
      | Codegen_for_solutions {solutions; codegen_options; exclusions} ->
          codegen_for_solutions_cmd solutions codegen_options ~exclusions
      | Codegen_check_definitions {files} -> codegen_check_definitions_cmd files
      | Solution_print solutions -> solution_print_cmd None solutions
      | Auto_build {bench_names; auto_build_options} ->
          Auto_build.cmd bench_names auto_build_options)
