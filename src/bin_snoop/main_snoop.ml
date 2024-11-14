(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023  Marigold <contact@marigold.dev>                       *)
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

let rec infer_cmd local_model_name workload_data solver infer_opts =
  Pyinit.pyinit () ;
  let file_stats = Unix.stat workload_data in
  match file_stats.st_kind with
  | S_DIR ->
      (* User specified a directory. Automatically process all workload data in that directory. *)
      infer_cmd_full_auto local_model_name workload_data solver infer_opts
  | S_REG ->
      (* User specified a workload data file. Only process that file. *)
      infer_cmd_one_shot local_model_name workload_data solver infer_opts
  | _ ->
      Format.eprintf
        "Error: %s is neither a regular file nor a directory.@."
        workload_data ;
      exit 1

and infer_all_cmd workload_data solver infer_opts =
  Pyinit.pyinit () ;
  let file_stats = Unix.stat workload_data in
  match file_stats.st_kind with
  | S_DIR ->
      (* User specified a directory. Automatically process all workload data in that directory. *)
      infer_all_cmd_full_auto workload_data solver infer_opts
  | _ ->
      Format.eprintf "Error: %s is not a directory.@." workload_data ;
      exit 1

and infer_cmd_one_shot local_model_name workload_data solver
    (infer_opts : Cmdline.infer_parameters_options) =
  let measure = Measure.load ~filename:workload_data in
  match measure with
  | Measure.Measurement
      ((module Bench), {bench_opts = _; workload_data; date = _}) ->
      let model =
        match
          List.assoc_opt ~equal:String.equal local_model_name Bench.models
        with
        | Some m -> m
        | None ->
            Format.eprintf
              "Requested local model: \"%s\" not found@."
              local_model_name ;
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
              ~local_model_name
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
      process_output measure local_model_name problem solution infer_opts

and infer_cmd_full_auto local_model_name workload_data solver
    (infer_opts : Cmdline.infer_parameters_options) =
  let workload_files = get_all_workload_data_files workload_data in
  let graph, measurements =
    Dep_graph.load_workload_files ~local_model_name workload_files
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
  @@ infer_for_measurements
       ~local_model_name
       measurements
       solution
       ~solver
       infer_opts

and infer_all_cmd_full_auto workload_data solver
    (infer_opts : Cmdline.infer_parameters_options) =
  let map_file ~local_model_name =
    let local_model_name =
      String.split_on_char '/' local_model_name |> String.concat "__"
    in
    fun original_name ->
      let extension = Filename.extension original_name in
      Format.sprintf
        "%s_%s%s"
        (Filename.remove_extension original_name)
        local_model_name
        extension
  in
  let workload_files = get_all_workload_data_files workload_data in
  let local_model_names =
    List.concat_map
      (fun filename ->
        match Measure.load ~filename with
        | Measure.Measurement (b, _) ->
            let module B : Benchmark.S = (val b) in
            List.map fst B.models)
      workload_files
    |> List.filter (fun x -> x <> "*")
    |> List.sort_uniq String.compare
  in
  List.iter
    (fun local_model_name ->
      let remap_file = Option.map (map_file ~local_model_name) in
      let infer_opts =
        {
          infer_opts with
          csv_export = remap_file infer_opts.csv_export;
          dot_file = remap_file infer_opts.dot_file;
          save_solution = remap_file infer_opts.save_solution;
          report =
            (match infer_opts.report with
            | Cmdline.ReportToFile x ->
                ReportToFile (map_file ~local_model_name x)
            | _ as x -> x);
        }
      in
      let graph, measurements =
        Dep_graph.load_workload_files ~local_model_name workload_files
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
      @@ infer_for_measurements
           ~local_model_name
           measurements
           solution
           ~solver
           infer_opts)
    (List.sort_uniq String.compare local_model_names)

(* If [local_model_name] is specified, the inference is restricted only to
   the models with [local_model_name]. *)
and infer_for_measurements ?local_model_name measurements
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
  let overrides_map, scores_list, report =
    List.fold_left
      (fun (overrides_map, scores_list, report) solved ->
        let measure =
          Stdlib.Option.get
          @@ Namespace.Hashtbl.find
               measurements
               solved.Dep_graph.Solver.Solved.name
        in
        let (Measure.Measurement ((module Bench), m)) = measure in

        (* Filter [Bench.models] if [local_model_name] is specified *)
        let models =
          match local_model_name with
          | None -> Bench.models
          | Some local_model_name ->
              List.filter
                (fun (local_model_name', _) ->
                  (* [builtin/Timer_latency] bound with ["*"] must be chosen
                     in every case *)
                  local_model_name' = "*"
                  || local_model_name = local_model_name')
                Bench.models
        in

        (* Run inference of [models]. Here we assume the inferences of the models
           do not depend on each other *)
        let solutions =
          List.map
            (fun (local_model_name, model) ->
              Format.eprintf
                "Running inference for %a (local_model_name: %s)@."
                Namespace.pp
                solved.Dep_graph.Solver.Solved.name
                local_model_name ;
              Format.eprintf "  @[%a@]@." Dep_graph.Solver.Solved.pp solved ;
              let overrides var = Free_variable.Map.find var overrides_map in
              let problem =
                Inference.make_problem
                  ~data:m.Measure.workload_data
                  ~model
                  ~overrides
              in
              if infer_opts.print_problem then (
                Format.eprintf
                  "Dumping problem to stdout as requested by user@." ;
                Csv.export_stdout (Inference.problem_to_csv problem)) ;
              let solution = Inference.solve_problem problem solver in
              (local_model_name, problem, solution))
            models
        in

        (* [solved.provides] must be all solved in [solutions] *)
        Free_variable.Set.iter
          (fun fv ->
            if
              not
              @@ List.exists
                   (fun (_, _, solution) ->
                     List.mem_assoc
                       ~equal:Free_variable.equal
                       fv
                       solution.Inference.mapping)
                   solutions
            then (
              Format.eprintf
                "Error: a provided free variable %a is not solved by the \
                 inference.@."
                Free_variable.pp
                fv ;
              exit 1))
          solved.provides ;

        List.fold_left
          (fun (overrides_map, scores_list, report)
               (local_model_name, problem, solution) ->
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
                solution.Inference.mapping
            in
            (* Lift up the intercept of memory allocation costs for overestimation *)
            let overrides_map =
              match List.rev (Namespace.to_list Bench.name) with
              | "intercept" :: "alloc" :: basename :: rem
              | "alloc" :: basename :: rem ->
                  (* Generate intercept parameter name from the benchmark name *)
                  let fv_intercept =
                    let l = (basename ^ "_alloc_const") :: rem in
                    Free_variable.of_namespace (Namespace.of_list (List.rev l))
                  in
                  Free_variable.Map.update
                    fv_intercept
                    (Option.map (fun intercept ->
                         Format.eprintf
                           "Updating intercept %a := %f + %f@."
                           Free_variable.pp
                           fv_intercept
                           intercept
                           solution.intercept_lift ;
                         intercept +. solution.intercept_lift))
                    overrides_map
              | _ -> overrides_map
            in
            let report =
              Option.map
                (Report.add_section
                   ~measure
                   ~local_model_name
                   ~problem
                   ~solution
                   ~overrides_map
                   ~display_options
                   ~short:true)
                report
            in
            let scores_label = (local_model_name, Bench.name) in
            let scores_list = (scores_label, solution.scores) :: scores_list in
            perform_plot measure local_model_name problem solution infer_opts ;
            (overrides_map, scores_list, report))
          (overrides_map, scores_list, report)
          solutions)
      (overrides_map, scores_list, report)
      solved_list
  in
  let solution = Codegen.{map = overrides_map; scores_list} in
  perform_save_solution solution infer_opts ;
  perform_save_solution_to_csv solution infer_opts ;
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

and process_output measure local_model_name problem solution infer_opts =
  let (Measure.Measurement ((module Bench), _)) = measure in
  let scores_label = (local_model_name, Bench.name) in
  perform_csv_export scores_label solution infer_opts ;
  let map = Free_variable.Map.of_seq (List.to_seq solution.mapping) in
  perform_save_solution
    Codegen.{map; scores_list = [(scores_label, solution.scores)]}
    infer_opts ;
  perform_plot measure local_model_name problem solution infer_opts

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

and perform_save_solution_to_csv solution infer_opts =
  Option.iter
    (fun filename ->
      let solution_csv = Codegen.solution_to_csv solution in
      Csv.append_columns ~filename solution_csv)
    infer_opts.csv_export

and perform_save_solution solution
    (infer_opts : Cmdline.infer_parameters_options) =
  match infer_opts.save_solution with
  | None -> ()
  | Some filename ->
      Codegen.save_solution solution filename ;
      Format.eprintf "Saved solution to %s@." filename

and perform_plot measure local_model_name problem solution
    (infer_opts : Cmdline.infer_parameters_options) =
  if infer_opts.plot then
    ignore
    @@ Display.perform_plot
         ~measure
         ~local_model_name
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

let code_transform codegen_options =
  let open Costlang in
  let fp : transform =
    match codegen_options.Cmdline.transform with
    | None -> (module Identity)
    | Some options ->
        (module Fixed_point_transform.Apply (struct
          let options = options
        end))
  in
  compose fp (compose (module Beta_normalize) (module Fold_constants))

let codegen_cmd solution_fn model_name codegen_options =
  let sol = Codegen.load_solution solution_fn in
  match Registration.find_model model_name with
  | None ->
      Format.eprintf "Model %a not found, exiting@." Namespace.pp model_name ;
      exit 1
  | Some {Registration.model; _} ->
      let transform = code_transform codegen_options in
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

(** Returns [(destination, code list) map] *)
let generate_code_for_models sol models codegen_options =
  (* The order of the models is pretty random.  It is better to sort them. *)
  let models =
    List.sort (fun (n1, _) (n2, _) -> Namespace.compare n1 n2) models
  in
  let transform = code_transform codegen_options in
  let generated = Codegen.codegen_models models sol transform in
  if codegen_options.split then
    List.fold_left
      (fun m -> function
        | Some dest, code ->
            String.Map.update
              dest
              (function None -> Some [code] | Some x -> Some (x @ [code]))
              m
        | None, _ -> m)
      String.Map.empty
      generated
  else String.Map.singleton "auto_build" (List.map snd generated)

(* Try to convert the given file name "*_costs_generated.ml" to "*_costs.ml" *)
let convert_costs_file_name path =
  let open Option_syntax in
  let dir = Filename.dirname path in
  let base = Filename.remove_extension @@ Filename.basename path in
  let suffix = "_costs_generated" in
  let+ prefix = String.remove_suffix ~suffix base in
  let fn = Format.asprintf "%s_costs.ml" prefix in
  Filename.concat dir fn

let get_defined_cost_function_list save_to =
  let open Option_syntax in
  let* costs_file = convert_costs_file_name save_to in
  let+ costs_file =
    if Sys.file_exists costs_file then Some costs_file else None
  in
  let () = Format.eprintf "Detected %s@." costs_file in
  let res = Codegen.Parser.get_cost_functions costs_file in
  match res with Ok cost_fun_list -> cost_fun_list | Error err -> raise err

let save_code_list_as_a_module save_to code_list =
  let result = Codegen.make_toplevel_module code_list in
  stdout_or_file save_to (fun ppf -> Codegen.pp_module ppf result)

let generate_code codegen_options generated =
  let make_destination destination_module save_to =
    let is_split = codegen_options.Cmdline.split in
    let dirname, destination =
      if not is_split then
        ( Filename.dirname save_to,
          Filename.remove_extension @@ Filename.basename save_to )
      else (
        (match Sys.is_directory save_to with
        | true -> ()
        | false ->
            Sys.remove save_to ;
            Sys.mkdir save_to 0o777
        | exception Sys_error _ -> Sys.mkdir save_to 0o777) ;
        ( save_to,
          Filename.remove_extension @@ Filename.basename destination_module ))
    in
    let suffix =
      if Option.is_some codegen_options.Cmdline.transform then String.empty
      else "__non_fp"
    in
    let result = Format.sprintf "%s%s.ml" destination suffix in
    Filename.concat dirname result
  in
  let save_to destination_module =
    Option.map
      (make_destination destination_module)
      codegen_options.Cmdline.save_to
  in
  let filter_defined_cost_function dest codes =
    (* Filter [cost_*] functions which is already defined in "*_costs.ml" file *)
    let fun_list =
      let open Option_syntax in
      let* split_to_dir =
        if codegen_options.split then codegen_options.save_to else None
      in
      let generated_file = Filename.(concat split_to_dir (basename dest)) in
      get_defined_cost_function_list generated_file
    in
    match fun_list with
    | None -> codes
    | Some fun_list ->
        let codes, filtered =
          List.partition_map
            (fun code ->
              match Codegen.get_name_of_code code with
              | None -> Left code
              | Some name ->
                  if List.mem ~equal:String.equal name fun_list then Right name
                  else Left code)
            codes
        in
        let () =
          let open Format in
          eprintf
            "@[<v2>Skipped functions to be output:@;%a@]@."
            (pp_print_list pp_print_string)
            filtered
        in
        codes
  in
  String.Map.iter
    (fun k v ->
      let v = filter_defined_cost_function k v in
      save_code_list_as_a_module (save_to k) v)
    generated

let codegen_all_cmd solution_fn regexp codegen_options =
  let () = Format.eprintf "regexp: %s@." regexp in
  let regexp = Str.regexp regexp in
  let ok (name, _) = Str.string_match regexp (Namespace.to_string name) 0 in
  let sol = Codegen.load_solution solution_fn in
  let models = List.filter ok (Registration.all_models ()) in
  let generated = generate_code_for_models sol models codegen_options in
  generate_code codegen_options generated

let codegen_for_a_solution solution codegen_options =
  let fvs_of_codegen_model model =
    let (Model.Model model) = model in
    let module Model = (val model) in
    let module FV = Model.Def (Costlang.Free_variables) in
    FV.model
  in
  let model_fvs_included_in_sol model =
    let fvs = fvs_of_codegen_model model in
    Free_variable.Set.for_all
      (fun fv -> Free_variable.Map.mem fv solution.Codegen.map)
      fvs
  in
  (* Model's free variables must be included in the solution's keys *)
  let codegen_models =
    List.filter (fun (_model_name, {Registration.model; _}) ->
        model_fvs_included_in_sol model)
    @@ Registration.all_models ()
  in
  generate_code_for_models solution codegen_models codegen_options

let save_codegen_for_solutions solutions codegen_options =
  let generated =
    List.fold_left
      (fun m solution ->
        let m' = codegen_for_a_solution solution codegen_options in
        String.Map.merge (fun _dest -> Option.merge (fun a b -> a @ b)) m m')
      String.Map.empty
      solutions
  in
  generate_code codegen_options generated

let codegen_for_solutions_cmd ?(build_all = false) solution_fns codegen_options
    =
  let is_dir, solution_dir =
    match solution_fns with
    | x :: [] -> (Sys.is_directory x, x)
    | _ -> (false, "")
  in
  let solutions =
    if is_dir then
      Sys.readdir solution_dir |> Array.to_list
      |> List.filter (fun x -> Filename.extension x = ".sol")
      |> List.map (fun x -> Filename.concat solution_dir x)
    else solution_fns
  in
  let solutions = List.map Codegen.load_solution solutions in
  save_codegen_for_solutions solutions codegen_options ;
  if build_all then
    let transform =
      Option.fold
        ~none:(Some Fixed_point_transform.default_options)
        ~some:(fun _ -> None)
        codegen_options.transform
    in
    save_codegen_for_solutions solutions {codegen_options with transform}

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

  (* Assumes the data files are found in [_snoop/sapling_data] *)
  let make_sapling_benchmark_config dest ns =
    let open Tezos_benchmarks_proto_alpha in
    let open Interpreter_benchmarks.Default_config in
    let sapling_txs_file = "_snoop/sapling_data" in
    let data_files_available =
      let check ty =
        try Sapling_generation.load ~filename:sapling_txs_file ty <> []
        with _ -> false
      in
      check Empty && check Non_empty
    in
    if data_files_available then (
      let sapling = {default_config.sapling with sapling_txs_file} in
      let config = {default_config with sapling} in
      let json = Data_encoding.Json.construct config_encoding config in
      Config.(save_config dest (build [(ns, json)])) ;
      Some dest)
    else (
      Format.eprintf
        "@[<v2>WARNING: Failed to load sapling data at %s@,\
         Auto-build's sapling benchmarks assume sapling data under this \
         directory.@,\
         Prepare the data by dune exec tezt/snoop/main.exe@]@."
        sapling_txs_file ;
      None)

  (* Assumes the data file is at [mich_fn] *)
  let make_michelson_benchmark_config dest ns mich_fn =
    let open Tezos_benchmarks_proto_alpha in
    let open Translator_benchmarks.Config in
    if Sys.file_exists mich_fn then (
      let config = {default_config with michelson_terms_file = Some mich_fn} in
      let json = Data_encoding.Json.construct config_encoding config in
      Config.(save_config dest (build [(ns, json)])) ;
      Some dest)
    else (
      Format.eprintf
        "@[<v2>WARNING: Michelson data file %s does not exist.@,\
         For faster benchmarks of Michelson encoding you are recommended to@,\
         prepare this file using dune exec tezt/snoop/main.exe@]@."
        mich_fn ;
      None)

  (* Assumes the data files are found in [_snoop/tezos_node] *)
  let make_io_read_random_key_benchmark_config dest ns =
    let open Tezos_shell_benchmarks.Io_benchmarks.Read_random_key_bench in
    let data_dir = "_snoop/tezos-node" in
    let context_dir = Filename.concat data_dir "context" in
    let level, block_hash, context_hash =
      Tezos_shell_benchmarks.Io_helpers.get_head_block_from_context_dir data_dir
    in
    Format.eprintf
      "Using %s, Context_hash: %a; Block: %ld %a@."
      context_dir
      Context_hash.pp
      context_hash
      level
      Block_hash.pp
      block_hash ;
    let config =
      {
        existing_context = (context_dir, context_hash);
        subdirectory = "/contracts/index";
      }
    in
    let json = Data_encoding.Json.construct config_encoding config in
    Config.(save_config dest (build [(ns, json)])) ;
    Some dest

  (* Assumes the data files are found in [_snoop/tezos_node] *)
  let make_io_write_random_keys_benchmark_config dest ns =
    let open Tezos_shell_benchmarks.Io_benchmarks.Write_random_keys_bench in
    let data_dir = "_snoop/tezos-node" in
    let context_dir = Filename.concat data_dir "context" in
    let level, block_hash, context_hash =
      Tezos_shell_benchmarks.Io_helpers.get_head_block_from_context_dir data_dir
    in
    Format.eprintf
      "Using %s, Context_hash: %a; Block: %ld %a@."
      context_dir
      Context_hash.pp
      context_hash
      level
      Block_hash.pp
      block_hash ;
    let config =
      {
        default_config with
        existing_context = (context_dir, context_hash);
        subdirectory = "/contracts/index";
      }
    in
    let json = Data_encoding.Json.construct config_encoding config in
    Config.(save_config dest (build [(ns, json)])) ;
    Some dest

  (* Benchmark specific config overrides *)
  let override_measure_options ~outdir ~bench_name measure_options =
    let open Measure in
    let {bench_number; nsamples; config_file; _} = measure_options in
    let bench_name_tokens = Namespace.to_list bench_name in
    (* override [config_file] for sapling and michelson encoding benchmarks *)
    let config_file =
      Option.either_f config_file @@ fun () ->
      let dest =
        Filename.concat outdir
        @@ Namespace.to_filename bench_name
        ^ "_benchmark.config"
      in
      match bench_name_tokens with
      | "." :: "sapling" :: _ -> make_sapling_benchmark_config dest bench_name
      | "." :: "interpreter" :: n :: _
        when String.starts_with ~prefix:"ISapling" n ->
          make_sapling_benchmark_config dest bench_name
      | "." :: "translator" :: ("UNPARSING_CODE" | "TYPECHECKING_CODE") :: _
      | "." :: "script_typed_ir_size" :: "KINSTR_SIZE" :: _ ->
          make_michelson_benchmark_config
            dest
            bench_name
            "_snoop/michelson_data/code.mich"
      | "." :: "translator" :: ("UNPARSING_DATA" | "TYPECHECKING_DATA") :: _
      | "." :: "encoding" :: ("ENCODING_MICHELINE" | "DECODING_MICHELINE") :: _
      | "." :: "script_typed_ir_size" :: "VALUE_SIZE" :: _ ->
          make_michelson_benchmark_config
            dest
            bench_name
            "_snoop/michelson_data/data.mich"
      | ["."; "io"; "READ_RANDOM_KEY"] ->
          make_io_read_random_key_benchmark_config dest bench_name
      | ["."; "io"; "WRITE_RANDOM_KEYS"] ->
          make_io_write_random_keys_benchmark_config dest bench_name
      | _ -> None
    in
    (* override [nsamples] for "alloc"s *)
    let nsamples =
      if List.mem ~equal:String.equal "alloc" bench_name_tokens then 1
      else nsamples
    in
    (* override [bench_number] and [nsamples] for intercept and TIMER_LATENCY *)
    let bench_number, nsamples =
      match bench_name_tokens with
      | "." :: "io" :: _ ->
          (* Currently, IO benchmarks reloads the context and purges the disk cache only
             once for each benchmark.  [nsamples] must be 1 otherwise the second and later
             samples are executed with the caches available.
          *)
          (bench_number, 1)
      | ["."; "interpreter"; "N_IOpen_chest"; "intercept"] ->
          (* Timings of [IOpen_chest] are highly affected by hidden randomness
             of [puzzle].  We need multiple [bench_number] to avoid this effect.
          *)
          (100, 100)
      | _ -> (
          match Namespace.basename bench_name with
          | "intercept" -> (1, nsamples)
          | "TIMER_LATENCY" -> (1, 10000)
          | _ -> (bench_number, nsamples))
    in
    {measure_options with config_file; bench_number; nsamples}

  (* Perform the benchmark of name [bench_name] *)
  let benchmark outdir bench_name measure_options =
    let (module Bench) = Registration.find_benchmark_exn bench_name in
    let save_file =
      Filename.concat outdir (Namespace.to_filename bench_name ^ ".workload")
    in
    let save_file_tmp = save_file ^ ".tmp" in
    let bench_options =
      {
        Commands.Benchmark_cmd.default_benchmark_options with
        save_file = save_file_tmp;
        options = override_measure_options ~outdir ~bench_name measure_options;
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

  let infer ~outdir mkfilename measurements solution infer_opts =
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
        display = {infer_opts.Cmdline.display with save_directory = outdir};
      }
    in
    let solution =
      infer_for_measurements measurements solution ~solver infer_opts
    in
    save_solutions_in_text
      (Some (mkfilename ".sol.txt"))
      [(solution_fn, solution)] ;
    solution

  let codegen ~split mkfilename solution =
    let codegen_options =
      Cmdline.{transform = None; save_to = Some (mkfilename ".ml"); split}
    in
    save_codegen_for_solutions [solution] codegen_options ;
    let codegen_options =
      Cmdline.
        {
          transform = Some Fixed_point_transform.default_options;
          save_to = Some (mkfilename ".ml");
          split;
        }
    in
    save_codegen_for_solutions [solution] codegen_options

  let cmd ?(split = false) targets
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

    (* Benchmark dependency analysis *)
    Format.eprintf "Analyzing the global benchmark dependency...@." ;
    let free_variables_to_infer =
      match targets with
      | Cmdline.Benchmarks benches ->
          List.fold_left
            (fun acc bench ->
              Free_variable.Set.union acc
              @@ Benchmark.get_free_variable_set bench)
            Free_variable.Set.empty
            benches
      | Models models ->
          List.fold_left
            (fun acc (Model.Model model) ->
              Free_variable.Set.union acc @@ Model.get_free_variable_set model)
            Free_variable.Set.empty
            models
      | Parameters parameters -> Free_variable.Set.of_list parameters
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

    (* Inference and codegen *)
    Pyinit.pyinit () ;
    let mkfilename ext = Filename.concat outdir "auto_build" ^ ext in
    (* Infernece *)
    let solution =
      infer ~outdir mkfilename measurements providers infer_parameters
    in
    let mkfilename ext =
      if split then outdir else Filename.concat outdir "auto_build" ^ ext
    in
    (* Codegen *)
    codegen ~split mkfilename solution
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
      | Infer {local_model_name; workload_data; solver; infer_opts} ->
          infer_cmd local_model_name workload_data solver infer_opts
      | Infer_all {workload_data; solver; infer_opts} ->
          infer_all_cmd workload_data solver infer_opts
      | Codegen {solution; model_name; codegen_options} ->
          codegen_cmd solution model_name codegen_options
      | Codegen_all {solution; matching; codegen_options} ->
          codegen_all_cmd solution matching codegen_options
      | Codegen_inferred {solution; codegen_options} ->
          codegen_for_solutions_cmd [solution] codegen_options
      | Codegen_for_solutions {solutions; codegen_options} ->
          codegen_for_solutions_cmd ~build_all:true solutions codegen_options
      | Codegen_check_definitions {files} -> codegen_check_definitions_cmd files
      | Solution_print solutions -> solution_print_cmd None solutions
      | Auto_build {targets; auto_build_options; split} ->
          Auto_build.cmd ~split targets auto_build_options)
