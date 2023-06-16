(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

open Cmdline

let lift_opt f opt_arg state =
  match opt_arg with None -> state | Some arg -> f arg state

let parse_parameter f m =
  Tezos_clic.parameter (fun (_ : unit) p ->
      Lwt.return
      @@
      match f p with
      | Some x -> Ok x
      | None -> Error_monad.error_with_exn (Failure m))

let pp_tags =
  Format.pp_print_list
    ~pp_sep:(fun formatter () -> Format.fprintf formatter "; ")
    Format.pp_print_string

let print_model : type a. a Model.model -> string =
 fun model ->
  let module M = (val model) in
  let module M = M.Def (Costlang.Pp) in
  M.model

module Benchmark_cmd = struct
  (* ----------------------------------------------------------------------- *)
  (* Handling the options of the benchmarker *)
  open Measure

  let set_seed seed (options : options) = {options with seed = Some seed}

  let set_nsamples nsamples options = {options with nsamples}

  let set_save_file save_file options = {options with save_file}

  let set_csv_export csv_export (options : Cmdline.benchmark_options) =
    {options with csv_export}

  let set_storage_kind storage options = {options with storage}

  let set_bench_number bench_number options = {options with bench_number}

  let set_minor_heap_size minor_heap_size options =
    {options with minor_heap_size}

  let set_config_file config_file options = {options with config_file}

  let default_benchmark_options =
    let options =
      {
        seed = None;
        nsamples = 500;
        bench_number = 300;
        minor_heap_size = `words (256 * 1024);
        config_file = None;
      }
    in
    {
      options;
      save_file = "<This field /will/ be set by Tezos_clic>";
      storage = Memory;
      csv_export = None;
    }

  let pp_benchmark_options ppf (options : Cmdline.benchmark_options) =
    let open Printf in
    let save_file = options.save_file in
    let storage =
      match options.storage with
      | Memory -> "Mem"
      | Disk {source; base_dir; header_json} ->
          let pp_src =
            Format.asprintf
              "%a"
              Tezos_crypto.Signature.Public_key_hash.pp
              source
          in
          sprintf
            "Disk { source = %s ; base_dir = %s ; header_json = %s }"
            pp_src
            base_dir
            header_json
    in
    Format.fprintf
      ppf
      "@[<v 2>{ options = %a;@ save_file = %s;@ storage = %s }@]"
      Measure.pp_options
      options.options
      save_file
      storage

  (* ----------------------------------------------------------------------- *)
  (* "benchmark" command handler *)

  let benchmark_handler
      (nsamples, seed, bench_number, minor_heap_size, config_file, csv_export)
      bench_name save_file () =
    let options =
      default_benchmark_options.options
      |> lift_opt set_nsamples nsamples
      |> lift_opt set_seed seed
      |> lift_opt set_bench_number bench_number
      |> lift_opt set_minor_heap_size minor_heap_size
      |> set_config_file config_file
    in
    let options =
      {default_benchmark_options with options}
      |> set_save_file save_file |> set_csv_export csv_export
    in
    let bench_name = Namespace.of_string bench_name in
    commandline_outcome_ref :=
      Some (Benchmark {bench_name; bench_opts = options}) ;
    Lwt.return_ok ()

  module Options = struct
    (* Integer argument --nsamples *)
    let nsamples_arg =
      let nsamples_arg_param =
        parse_parameter
          int_of_string_opt
          "Error while parsing --nsamples argument."
      in
      Tezos_clic.arg
        ~doc:"Number of samples per benchmark"
        ~long:"nsamples"
        ~placeholder:"strictly positive int"
        nsamples_arg_param

    (* Integer argument --seed *)
    let seed_arg =
      let seed =
        parse_parameter int_of_string_opt "Error while parsing --seed argument."
      in
      Tezos_clic.arg ~doc:"RNG seed" ~long:"seed" ~placeholder:"int" seed

    (* String argument --dump-csv
       Parameter: filename of the file where to write the csv data. *)
    let dump_csv_arg =
      let dump_csv_arg_param =
        Tezos_clic.parameter (fun (_ : unit) parsed -> Lwt.return_ok parsed)
      in
      Tezos_clic.arg
        ~doc:"Dumps raw benchmark results to CSV"
        ~long:"dump-csv"
        ~placeholder:"filename"
        dump_csv_arg_param

    (* String argument --bench-number
       Parameter: Number of random stacks to generate. *)
    let bench_number_arg =
      let bench_number_param =
        parse_parameter
          int_of_string_opt
          "Error while parsing --bench-num argument."
      in
      Tezos_clic.arg
        ~doc:"Number of benchmarks (i.e. random stacks)"
        ~long:"bench-num"
        ~placeholder:"strictly positive int"
        bench_number_param

    (* String argument --minor-heap-size
       Parameter: size of minor heap in kb. *)
    let minor_heap_size_arg =
      let minor_heap_size_param =
        parse_parameter
          (fun s -> Option.map (fun p -> `words p) (int_of_string_opt s))
          "Error while parsing --minor-heap-size argument."
      in
      Tezos_clic.arg
        ~doc:"Size of minor heap in words"
        ~long:"minor-heap-size"
        ~placeholder:"strictly positive int"
        minor_heap_size_param

    let config_file_arg =
      let config_file_arg_param =
        Tezos_clic.parameter (fun (_ : unit) parsed -> Lwt.return_ok parsed)
      in
      Tezos_clic.arg
        ~doc:"Specify a benchmark configuration file"
        ~short:'c'
        ~long:"config-file"
        ~placeholder:"file"
        config_file_arg_param
  end

  let options =
    let open Options in
    Tezos_clic.args6
      nsamples_arg
      seed_arg
      bench_number_arg
      minor_heap_size_arg
      config_file_arg
      dump_csv_arg

  let benchmark_param () =
    Tezos_clic.param
      ~name:"BENCH-NAME"
      ~desc:"Name of the benchmark"
      (Tezos_clic.parameter
         ~autocomplete:(fun _ ->
           let res =
             List.map
               (fun (name, _) -> Namespace.to_string name)
               (Registration.all_benchmarks ())
           in
           Lwt.return_ok res)
         (fun _ str -> Lwt.return_ok str))

  let params =
    Tezos_clic.(
      prefix "benchmark" @@ benchmark_param ()
      @@ prefixes ["and"; "save"; "to"]
      @@ string
           ~name:"FILENAME"
           ~desc:"Name of the file where to save the workload data"
      @@ stop)

  let group =
    {
      Tezos_clic.name = "benchmark";
      title = "Commands for benchmarking parts of the protocol";
    }

  let command =
    Tezos_clic.command
      ~group
      ~desc:"Runs benchmarks"
      options
      params
      benchmark_handler
end

module Infer_cmd = struct
  (* -------------------------------------------------------------------------- *)
  (* Handling options for the "infer parameters" command *)

  let default_infer_parameters_options =
    {
      print_problem = false;
      csv_export = None;
      plot = false;
      ridge_alpha = 1.0;
      lasso_alpha = 1.0;
      lasso_positive = false;
      override_files = None;
      report = NoReport;
      save_solution = None;
      dot_file = None;
      display = Display.default_options;
    }

  let set_print_problem print_problem options = {options with print_problem}

  let set_csv_export csv_export options = {options with csv_export}

  let set_plot plot options = {options with plot}

  let set_ridge_alpha ridge_alpha options = {options with ridge_alpha}

  let set_lasso_alpha lasso_alpha options = {options with lasso_alpha}

  let set_lasso_positive lasso_positive options = {options with lasso_positive}

  let set_report report options =
    match report with
    | None -> {options with report = NoReport}
    | Some file ->
        if String.equal file "stdout" then
          {options with report = ReportToStdout}
        else {options with report = ReportToFile file}

  let set_override_files override_files options = {options with override_files}

  let set_save_solution save_solution options = {options with save_solution}

  let set_dot_file dot_file options = {options with dot_file}

  let set_full_plot_verbosity full_plot_verbosity options =
    {
      options with
      display =
        {options.display with reduced_plot_verbosity = not full_plot_verbosity};
    }

  let list_solvers () =
    Printf.eprintf "ridge --ridge-alpha=<float>\n" ;
    Printf.eprintf "lasso --lasso-alpha=<float> --lasso-positive\n" ;
    Printf.eprintf "nnls\n%!"

  let set_plot_raw_workload plot_raw_workload options =
    match plot_raw_workload with
    | None ->
        {
          options with
          display = {options.display with plot_raw_workload = false};
        }
    | Some save_directory ->
        {
          options with
          display =
            {options.display with plot_raw_workload = true; save_directory};
        }

  let set_empirical_plot empirical_plot options =
    {options with display = {options.display with empirical_plot}}

  let infer_handler
      ( print_problem,
        csv,
        plot,
        ridge_alpha,
        lasso_alpha,
        lasso_positive,
        report,
        override_files,
        save_solution,
        dot_file,
        full_plot_verbosity,
        plot_raw_workload,
        empirical_plot ) model_name workload_data solver () =
    let options =
      default_infer_parameters_options
      |> set_print_problem print_problem
      |> set_csv_export csv |> set_plot plot
      |> lift_opt set_ridge_alpha ridge_alpha
      |> lift_opt set_lasso_alpha lasso_alpha
      |> set_lasso_positive lasso_positive
      |> set_report report
      |> set_override_files override_files
      |> set_save_solution save_solution
      |> set_dot_file dot_file
      |> set_full_plot_verbosity full_plot_verbosity
      |> set_plot_raw_workload plot_raw_workload
      |> lift_opt set_empirical_plot empirical_plot
    in
    commandline_outcome_ref :=
      Some (Infer {model_name; workload_data; solver; infer_opts = options}) ;
    Lwt.return_ok ()

  module Options = struct
    (* Boolean argument --print-problem *)
    let print_problem =
      Tezos_clic.switch
        ~doc:"Prints problem as obtained after applying model to workload data"
        ~long:"print-problem"
        ()

    (* Boolean argument --plot *)
    let plot_arg =
      Tezos_clic.switch
        ~doc:"Plot results of parameter inference"
        ~long:"plot"
        ()

    (* Float argument --ridge-alpha *)
    let ridge_alpha_arg =
      let ridge_alpha_arg_param =
        parse_parameter
          float_of_string_opt
          "Error while parsing --ridge-alpha argument."
      in
      Tezos_clic.arg
        ~doc:"Regularization parameter for ridge regression"
        ~long:"ridge-alpha"
        ~placeholder:"positive float"
        ridge_alpha_arg_param

    (* Float argument --lasso-alpha *)
    let lasso_alpha_arg =
      let lasso_alpha_arg_param =
        parse_parameter
          float_of_string_opt
          "Error while parsing --lasso-alpha argument."
      in
      Tezos_clic.arg
        ~doc:"Regularization parameter for lasso regression"
        ~long:"lasso-alpha"
        ~placeholder:"positive float"
        lasso_alpha_arg_param

    (* Boolean argument --lasso-positive *)
    let lasso_positive_arg =
      Tezos_clic.switch
        ~doc:"Constrains solution of lasso regression to be positive"
        ~long:"lasso-positive"
        ()

    let dump_csv_arg =
      let dump_csv_arg_param =
        Tezos_clic.parameter (fun (_ : unit) parsed -> Lwt.return_ok parsed)
      in
      Tezos_clic.arg
        ~doc:"Dumps solution of inference to a CSV file"
        ~long:"dump-csv"
        ~placeholder:"filename"
        dump_csv_arg_param

    let report_arg =
      let dump_report_param =
        Tezos_clic.parameter (fun (_ : unit) parsed -> Lwt.return_ok parsed)
      in
      Tezos_clic.arg
        ~doc:"Produces a detailed report"
        ~long:"report"
        ~placeholder:"filename"
        dump_report_param

    let override_arg =
      let override_file_param =
        Tezos_clic.parameter (fun (_ : unit) parsed ->
            let files = String.split_no_empty ',' parsed in
            Lwt.return_ok files)
      in
      Tezos_clic.arg
        ~doc:"Specify CSV file containing overrided variables for inference"
        ~long:"override-csv"
        ~placeholder:"filename"
        override_file_param

    let save_solution_arg =
      let override_file_param =
        Tezos_clic.parameter (fun (_ : unit) parsed -> Lwt.return_ok parsed)
      in
      Tezos_clic.arg
        ~doc:
          "Specify file to which inference solution will be saved for code \
           generation"
        ~long:"save-solution"
        ~placeholder:"filename"
        override_file_param

    let dot_file_arg =
      let override_file_param =
        Tezos_clic.parameter (fun (_ : unit) parsed -> Lwt.return_ok parsed)
      in
      Tezos_clic.arg
        ~doc:
          "Specify file to which dependency graph will be saved in graphviz \
           format"
        ~long:"dot-file"
        ~placeholder:"filename"
        override_file_param

    let full_plot_verbosity_arg =
      Tezos_clic.switch
        ~doc:"Produces all (possibly redundant) plots"
        ~long:"full-plot-verbosity"
        ()

    let plot_raw_workload_arg =
      let raw_workload_directory_param =
        Tezos_clic.parameter (fun (_ : unit) parsed -> Lwt.return_ok parsed)
      in
      Tezos_clic.arg
        ~doc:
          "For each workload, produces a file containing the plot of the raw \
           data, in the specified directory"
        ~long:"plot-raw-workload"
        ~placeholder:"directory"
        raw_workload_directory_param

    let empirical_plot_arg =
      let empirical_plot_param =
        Tezos_clic.parameter (fun (_ : unit) parsed ->
            match parsed with
            | "full" -> Lwt.return_ok Display.Empirical_plot_full
            | _ -> (
                let splitted = String.split ',' parsed in
                match List.map float_of_string splitted with
                | exception Failure _ ->
                    Error_monad.failwith "can't parse quantile"
                | floats ->
                    if List.exists (fun q -> q < 0.0 || q > 1.0) floats then
                      Error_monad.failwith "quantile not in [0;1] interval"
                    else Lwt.return_ok (Display.Empirical_plot_quantiles floats)
                ))
      in
      Tezos_clic.arg
        ~doc:"Options for plotting empirical data quantiles"
        ~long:"empirical-plot"
        ~placeholder:"full|q1,...,qn"
        empirical_plot_param
  end

  let options =
    let open Options in
    Tezos_clic.args13
      print_problem
      dump_csv_arg
      plot_arg
      ridge_alpha_arg
      lasso_alpha_arg
      lasso_positive_arg
      report_arg
      override_arg
      save_solution_arg
      dot_file_arg
      full_plot_verbosity_arg
      plot_raw_workload_arg
      empirical_plot_arg

  let local_model_param =
    Tezos_clic.param
      ~name:"LOCAL-MODEL-NAME"
      ~desc:"Name of the local model for which to infer parameter"
      (Tezos_clic.parameter
         ~autocomplete:(fun _ ->
           Lwt.return_ok (Registration.all_local_model_names ()))
         (fun _ str -> Lwt.return_ok str))

  let regression_param =
    Tezos_clic.param
      ~name:"REGRESSION-METHOD"
      ~desc:"Regression method used"
      (Tezos_clic.parameter
         ~autocomplete:(fun _ -> Lwt.return_ok ["lasso"; "ridge"; "nnls"])
         (fun _ str -> Lwt.return_ok str))

  let params =
    Tezos_clic.(
      prefixes ["infer"; "parameters"; "for"; "model"]
      @@ local_model_param
      @@ prefixes ["on"; "data"]
      @@ string
           ~name:"WORKLOAD-DATA"
           ~desc:"File or directory containing workload data"
      @@ prefix "using" @@ regression_param @@ stop)

  let group =
    {
      Tezos_clic.name = "inference";
      title = "Command for infering parameters of cost models";
    }

  let command =
    Tezos_clic.command
      ~desc:"Perform parameter inference"
      ~group
      options
      params
      infer_handler
end

module Codegen_cmd = struct
  (* ------------------------------------------------------------------------- *)
  (* Handling options for the "generate code" command *)

  let load_fixed_point_parameters json_file =
    try
      let json =
        let ic = open_in json_file in
        let json = Ezjsonm.from_channel ic in
        close_in ic ;
        json
      in
      Data_encoding.Json.destruct Fixed_point_transform.options_encoding json
    with _ ->
      Format.eprintf
        "Could not parse fixed-point transform parameters; aborting@." ;
      Format.eprintf "Here's a well-formed file:@." ;
      Format.eprintf
        "%a@."
        Data_encoding.Json.pp
        (Data_encoding.Json.construct
           ~include_default_fields:`Always
           Fixed_point_transform.options_encoding
           Fixed_point_transform.default_options) ;
      exit 1

  let codegen_handler (fixed_point, save_to) solution model_name () =
    let transform = Option.map load_fixed_point_parameters fixed_point in
    let codegen_options = {transform; save_to} in
    let model_name = Namespace.of_string model_name in
    commandline_outcome_ref :=
      Some (Codegen {solution; model_name; codegen_options}) ;
    Lwt.return_ok ()

  let fixed_point_arg =
    Tezos_clic.arg
      ~doc:"Apply fixed-point transform to the model"
      ~long:"fixed-point"
      ~placeholder:"json-config-file"
      (Tezos_clic.parameter (fun () filename -> Lwt.return_ok filename))

  let save_to_arg =
    Tezos_clic.arg
      ~doc:"Save the output to a file"
      ~long:"save-to"
      ~placeholder:"file"
      (Tezos_clic.parameter (fun () filename -> Lwt.return_ok filename))

  let options = Tezos_clic.args2 fixed_point_arg save_to_arg

  let codegen_model_param =
    Tezos_clic.param
      ~name:"CODEGEN-MODEL-NAME"
      ~desc:"Name of the codegen model for which to generate code"
      (Tezos_clic.parameter
         ~autocomplete:(fun _ ->
           let res =
             List.map
               (fun (name, _) -> Namespace.to_string name)
               (Registration.all_models ())
           in
           Lwt.return_ok res)
         (fun _ str -> Lwt.return_ok str))

  let params =
    Tezos_clic.(
      prefixes ["generate"; "code"; "using"; "solution"]
      @@ string
           ~name:"SOLUTION-FILE"
           ~desc:
             "File containing solution, as obtained using the --save-solution \
              switch"
      @@ prefixes ["and"; "model"]
      @@ codegen_model_param @@ stop)

  let group =
    {Tezos_clic.name = "codegen"; title = "Command for generating code"}

  let command =
    Tezos_clic.command
      ~group
      ~desc:"Generate code for a specific model"
      options
      params
      codegen_handler
end

module Codegen_all_cmd = struct
  include Codegen_cmd

  let codegen_all_handler (json, save_to) solution matching () =
    let transform = Option.map load_fixed_point_parameters json in
    let codegen_options = {transform; save_to} in
    commandline_outcome_ref :=
      Some (Codegen_all {solution; matching; codegen_options}) ;
    Lwt.return_ok ()

  let params =
    Tezos_clic.(
      prefixes ["generate"; "code"; "using"; "solution"]
      @@ string
           ~name:"SOLUTION-FILE"
           ~desc:
             "File containing solution, as obtained using the --save-solution \
              switch"
      @@ prefixes ["for"; "all"; "models"; "matching"]
      @@ string ~name:"REGEXP" ~desc:"Regular expression on model names"
      @@ stop)

  let command =
    Tezos_clic.command
      ~group
      ~desc:"Generate code for all models matching regexp"
      options
      params
      codegen_all_handler
end

(* Obsolete.  It will be superceded by Codegen_for_solutions *)
module Codegen_inferred_cmd = struct
  include Codegen_cmd

  let codegen_inferred_handler (json, exclusions, save_to) solution () =
    let transform = Option.map load_fixed_point_parameters json in
    let codegen_options = {transform; save_to} in
    let exclusions =
      Option.fold
        ~none:String.Set.empty
        ~some:Codegen.load_exclusions
        exclusions
    in
    commandline_outcome_ref :=
      Some (Codegen_inferred {solution; codegen_options; exclusions}) ;
    Lwt.return_ok ()

  let params =
    Tezos_clic.(
      prefixes ["generate"; "code"; "using"; "solution"]
      @@ string
           ~name:"SOLUTION-FILE"
           ~desc:
             "File containing solution, as obtained using the --save-solution \
              switch"
      @@ fixed ["for"; "inferred"; "models"])

  let exclude_arg =
    Tezos_clic.arg
      ~doc:"A file containing the function names to exclude for the codegen"
      ~long:"exclude-file"
      ~placeholder:"filename"
      (Tezos_clic.parameter (fun (_ : unit) filename -> Lwt.return_ok filename))

  let options =
    Tezos_clic.args3 Codegen_cmd.fixed_point_arg exclude_arg save_to_arg

  let command =
    Tezos_clic.command
      ~group
      ~desc:"Generate code for models inferred from the solution file"
      options
      params
      codegen_inferred_handler
end

module Solution_print_cmd = struct
  let solution_print_handler () solutions () =
    commandline_outcome_ref := Some (Solution_print solutions) ;
    Lwt.return_ok ()

  let group =
    {Tezos_clic.name = "solution"; title = "Command for solution file"}

  let params =
    Tezos_clic.(
      prefixes ["solution"; "print"]
      @@ seq_of_param
           (string
              ~name:"SOLUTION-FILE"
              ~desc:
                "File containing solution, as obtained using the \
                 --save-solution switch"))

  let command =
    Tezos_clic.command
      ~group
      ~desc:"Print out the given solution file(s)"
      Tezos_clic.no_options
      params
      solution_print_handler
end

module Codegen_for_solutions_cmd = struct
  include Codegen_cmd

  let codegen_for_solutions_handler (json, exclusions, save_to) solutions () =
    let transform = Option.map load_fixed_point_parameters json in
    let codegen_options = {transform; save_to} in
    let exclusions =
      Option.fold
        ~none:String.Set.empty
        ~some:Codegen.load_exclusions
        exclusions
    in
    commandline_outcome_ref :=
      Some (Codegen_for_solutions {solutions; codegen_options; exclusions}) ;
    Lwt.return_ok ()

  let params =
    Tezos_clic.(
      prefixes ["generate"; "code"; "for"; "solutions"]
      @@ seq_of_param
           (string
              ~name:"SOLUTION-FILE"
              ~desc:
                "File containing solution, as obtained using the \
                 --save-solution switch"))

  let options = Codegen_inferred_cmd.options

  let command =
    Tezos_clic.command
      ~group
      ~desc:"Generate code for the models inferred from the solution files"
      options
      params
      codegen_for_solutions_handler
end

module Codegen_check_definitions_cmd = struct
  let codegen_check_definitions_handler () files () =
    commandline_outcome_ref := Some (Codegen_check_definitions {files}) ;
    Lwt.return_ok ()

  let group = Codegen_cmd.group

  let params =
    Tezos_clic.(
      prefixes ["check"; "definitions"; "of"]
      @@ seq_of_param
           (string
              ~name:"MLFILE"
              ~desc:"File containing cost function definitions"))

  let command =
    Tezos_clic.(
      command
        ~group
        ~desc:"Check cost functions defined in the given .ml files"
        no_options
        params
        codegen_check_definitions_handler)
end

module Auto_build_cmd = struct
  let auto_build_handler
      ( destination_directory,
        nsamples,
        bench_number,
        print_problem,
        plot,
        override_files,
        full_plot_verbosity,
        plot_raw_workload,
        empirical_plot ) bench_names () =
    let bench_names =
      let all_benchmarks = Registration.all_benchmarks () in
      List.map
        (fun s ->
          let n = Namespace.of_string s in
          if not @@ List.mem_assoc ~equal:Namespace.equal n all_benchmarks then (
            Format.eprintf "Benchmark %a does not exist.@." Namespace.pp n ;
            exit 1) ;
          n)
        bench_names
    in
    let infer_parameters =
      let open Infer_cmd in
      default_infer_parameters_options
      |> set_print_problem print_problem
      |> set_plot plot
      |> set_override_files override_files
      |> set_full_plot_verbosity full_plot_verbosity
      |> set_plot_raw_workload plot_raw_workload
      |> lift_opt set_empirical_plot empirical_plot
    in
    let measure_options =
      let opts = Benchmark_cmd.default_benchmark_options.options in
      {
        opts with
        nsamples = Option.value nsamples ~default:opts.nsamples;
        bench_number = Option.value bench_number ~default:opts.bench_number;
      }
    in
    let auto_build_options =
      {destination_directory; infer_parameters; measure_options}
    in
    commandline_outcome_ref :=
      Some (Auto_build {bench_names; auto_build_options}) ;
    Lwt.return_ok ()

  let params =
    Tezos_clic.(
      prefixes ["generate"; "code"; "for"; "benchmarks"]
      @@ seq_of_param
      @@ Benchmark_cmd.benchmark_param ())

  let destination_directory_arg =
    Tezos_clic.arg
      ~doc:"Destination directory of the auto-build result"
      ~long:"out-dir"
      ~placeholder:"directory"
      (Tezos_clic.parameter (fun () filename -> Lwt.return_ok filename))

  let options =
    Tezos_clic.args9
      destination_directory_arg
      Benchmark_cmd.Options.nsamples_arg
      Benchmark_cmd.Options.bench_number_arg
      Infer_cmd.Options.print_problem
      Infer_cmd.Options.plot_arg
      Infer_cmd.Options.override_arg
      Infer_cmd.Options.full_plot_verbosity_arg
      Infer_cmd.Options.plot_raw_workload_arg
      Infer_cmd.Options.empirical_plot_arg

  let command =
    Tezos_clic.command
      ~group:Codegen_cmd.group
      ~desc:
        "Auto-perform the benchmarks, inference and codegen for the given \
         benchmarks"
      options
      params
      auto_build_handler
end

module List_cmd = struct
  (* ------------------------------------------------------------------------- *)

  let tag_param =
    Tezos_clic.param
      ~name:"TAG"
      ~desc:"Tag of a benchmark"
      (Tezos_clic.parameter
         ~autocomplete:(fun _ -> Lwt.return_ok (Registration.all_tags ()))
         (fun _ s -> Lwt.return_ok s))

  let benchmark_param () =
    Tezos_clic.param
      ~name:"BENCH-NAME"
      ~desc:"Name of the benchmark"
      (Tezos_clic.parameter
         ~autocomplete:(fun _ ->
           let res =
             List.map
               (fun (name, _) -> Namespace.to_string name)
               (Registration.all_benchmarks ())
           in
           Lwt.return_ok res)
         (fun _ str -> Lwt.return_ok str))

  let model_param () =
    Tezos_clic.param
      ~name:"MODEL-NAME"
      ~desc:"Name of the model"
      (Tezos_clic.parameter
         ~autocomplete:(fun _ ->
           let res =
             List.map
               (fun model_name -> Namespace.to_string model_name)
               (Registration.all_model_names ())
           in
           Lwt.return_ok res)
         (fun _ str -> Lwt.return_ok str))

  let local_model_param () =
    Tezos_clic.param
      ~name:"LOCAL-MODEL-NAME"
      ~desc:"Name of the local model"
      (Tezos_clic.parameter
         ~autocomplete:(fun _ ->
           Registration.all_local_model_names () |> Lwt.return_ok)
         (fun _ str -> Lwt.return_ok str))

  let parameter_param () =
    Tezos_clic.param
      ~name:"PARAM-NAME"
      ~desc:"Name of the parameter"
      (Tezos_clic.parameter
         ~autocomplete:(fun _ ->
           let res =
             List.map
               (fun (param, _) -> Free_variable.to_string param)
               (Registration.all_parameters ())
           in
           Lwt.return_ok res)
         (fun _ str -> Lwt.return_ok str))

  let params_all_bench = Tezos_clic.fixed ["list"; "all"; "benchmarks"]

  let options =
    Tezos_clic.args1
      (Tezos_clic.switch
         ~long:"show-tags"
         ~short:'t'
         ~doc:"Show the tags of the benchmarks"
         ())

  let base_handler_bench bench_list show_tags =
    Format.printf
      "@[<v>%a@]@."
      (Format.pp_print_list (fun fmt (module Bench : Benchmark.S) ->
           Format.fprintf fmt "%a: %s" Namespace.pp Bench.name Bench.info ;
           if show_tags then
             Format.fprintf fmt "@.\tTags: %a" pp_tags Bench.tags
           else ()))
      bench_list ;
    Lwt_result_syntax.return_unit

  let handler_all_bench show_tags () =
    base_handler_bench
      (Registration.all_benchmarks () |> List.map snd)
      show_tags

  let params_all_tags = Tezos_clic.fixed ["list"; "all"; "tags"]

  let handler_all_tags () () =
    List.iter
      (fun tag -> Format.fprintf Format.std_formatter "%s\n" tag)
      (Registration.all_tags ()) ;
    Lwt_result_syntax.return_unit

  let params_bench_tags_any =
    Tezos_clic.(
      prefixes ["list"; "benchmarks"; "with"; "tags"; "any"; "of"]
      @@ seq_of_param tag_param)

  let handler_bench_tags_any show_tags tags () =
    base_handler_bench
      (Registration.find_benchmarks_with_tags ~mode:`Any tags |> List.map snd)
      show_tags

  let params_bench_tags_all =
    Tezos_clic.(
      prefixes ["list"; "benchmarks"; "with"; "tags"; "all"; "of"]
      @@ seq_of_param tag_param)

  let handler_bench_tags_all show_tags tags () =
    base_handler_bench
      (Registration.find_benchmarks_with_tags ~mode:`All tags |> List.map snd)
      show_tags

  let params_bench_tags_exact =
    Tezos_clic.(
      prefixes ["list"; "benchmarks"; "with"; "tags"; "exactly"]
      @@ seq_of_param tag_param)

  let handler_bench_tags_exact show_tags tags () =
    base_handler_bench
      (Registration.find_benchmarks_with_tags ~mode:`Exact tags |> List.map snd)
      show_tags

  let params_bench_match =
    Tezos_clic.(
      prefixes ["list"; "benchmarks"; "in"] @@ benchmark_param () @@ stop)

  let handler_bench_match show_tags pattern () =
    base_handler_bench
      (Registration.find_benchmarks_in_namespace (Namespace.of_string pattern)
      |> List.map snd)
      show_tags

  let params_all_param = Tezos_clic.fixed ["list"; "all"; "parameters"]

  let handler_all_param () () =
    Format.printf
      "%a@."
      (Format.pp_print_list (fun fmt (param, models) ->
           Format.fprintf
             fmt
             "%a@.\tModels: %a"
             Free_variable.pp
             param
             (Format.pp_print_list
                ~pp_sep:(fun formatter () -> Format.fprintf formatter "; ")
                Namespace.pp)
             models))
      (Registration.all_parameters ()) ;
    Lwt_result_syntax.return_unit

  let params_all_models = Tezos_clic.fixed ["list"; "all"; "models"]

  let handler_all_models () () =
    Format.printf
      "%a@."
      (Format.pp_print_list (fun fmt (name, {Registration.model; _}) ->
           let printed =
             match model with Model.Model model -> print_model model
           in
           Format.fprintf fmt "%a@.\t%s" Namespace.pp name printed))
      (Registration.all_models ()) ;
    Lwt_result_syntax.return_unit

  let params_all_local_models =
    Tezos_clic.fixed ["list"; "all"; "local"; "models"]

  let handler_all_local_models () () =
    let module S = String.Set in
    S.iter
      (fun name -> Format.fprintf Format.std_formatter "%s\n" name)
      (Registration.all_local_model_names () |> S.of_list) ;
    Lwt_result_syntax.return_unit

  let group =
    {Tezos_clic.name = "list"; title = "Commands for displaying lists"}

  let command_all_bench =
    Tezos_clic.command
      ~group
      ~desc:"List all implemented benchmarks"
      options
      params_all_bench
      handler_all_bench

  let command_all_tags =
    Tezos_clic.command
      ~group
      ~desc:"List all available tags"
      Tezos_clic.no_options
      params_all_tags
      handler_all_tags

  let command_bench_tags_any =
    Tezos_clic.command
      ~group
      ~desc:"List all implemented benchmarks containing any of the given tags"
      options
      params_bench_tags_any
      handler_bench_tags_any

  let command_bench_tags_all =
    Tezos_clic.command
      ~group
      ~desc:"List all implemented benchmarks containing all of the given tags"
      options
      params_bench_tags_all
      handler_bench_tags_all

  let command_bench_tags_exact =
    Tezos_clic.command
      ~group
      ~desc:"List all implemented benchmarks containing exactly the given tags"
      options
      params_bench_tags_exact
      handler_bench_tags_exact

  let command_bench_match =
    Tezos_clic.command
      ~group
      ~desc:"List all benchmarks in the given namespace"
      options
      params_bench_match
      handler_bench_match

  let command_all_param =
    Tezos_clic.command
      ~group
      ~desc:"List all parameters"
      Tezos_clic.no_options
      params_all_param
      handler_all_param

  let command_all_models =
    Tezos_clic.command
      ~group
      ~desc:"List all models"
      Tezos_clic.no_options
      params_all_models
      handler_all_models

  let command_all_local_models =
    Tezos_clic.command
      ~group
      ~desc:"List all local models"
      Tezos_clic.no_options
      params_all_local_models
      handler_all_local_models

  let commands =
    [
      command_all_bench;
      command_all_tags;
      command_bench_tags_any;
      command_bench_tags_all;
      command_bench_tags_exact;
      command_bench_match;
      command_all_param;
      command_all_models;
      command_all_local_models;
    ]
end

module Config_cmd = struct
  let config_file_param () =
    Tezos_clic.param
      ~name:"CONFIG-FILE"
      ~desc:"Configuration file name"
      (Tezos_clic.parameter (fun _ s -> Lwt.return_ok s))

  let benchmark_param = List_cmd.benchmark_param

  let options_merge =
    Tezos_clic.args1
      (Tezos_clic.switch
         ~long:"delete-source"
         ~short:'d'
         ~doc:"Deletes the source config file given as argument for merging"
         ())

  let options_edit =
    Tezos_clic.args4
      (Tezos_clic.arg
         ~long:"use-editor"
         ~short:'e'
         ~placeholder:"EDITOR"
         ~doc:
           "Specify the prefered text editor used for editing the config file"
         (Tezos_clic.parameter (fun _ str -> Lwt.return_ok str)))
      (Tezos_clic.switch
         ~long:"read-stdin"
         ~short:'i'
         ~doc:
           "Read the standard input for a Json document to edit the config file"
         ())
      (Tezos_clic.arg
         ~long:"read-file"
         ~short:'f'
         ~placeholder:"FILE"
         ~doc:"Use the given Json document to edit the config file"
         (Tezos_clic.parameter (fun _ str -> Lwt.return_ok str)))
      (Tezos_clic.arg
         ~long:"read-json"
         ~short:'j'
         ~placeholder:"JSON"
         ~doc:"Use inlined Json to edit the config file"
         (Tezos_clic.parameter (fun _ str -> Lwt.return_ok str)))

  let params_check =
    Tezos_clic.(
      prefixes ["config"; "check"]
      @@ config_file_param () @@ prefix "for" @@ benchmark_param () @@ stop)

  let handler_check () config_file benchmark () =
    let benchmark = Namespace.of_string benchmark in
    let bench = Registration.find_benchmark_exn benchmark in
    match Benchmark.ex_unpack bench with
    | Benchmark.Ex bench ->
        let _ =
          Config.parse_config ~print:Stdlib.stdout bench (Some config_file)
        in
        Lwt_result_syntax.return_unit

  let params_generate_default =
    Tezos_clic.(
      prefixes ["config"; "generate"; "default"; "in"]
      @@ config_file_param () @@ prefix "for"
      @@ seq_of_param (benchmark_param ()))

  let handler_generate_default () config_file namespaces () =
    let namespaces = List.map Namespace.of_string namespaces in
    let benchmarks =
      List.map Registration.find_benchmarks_in_namespace namespaces
      |> List.flatten |> List.map snd
    in
    let config = Config.generate_default benchmarks in
    let str =
      Data_encoding.Json.construct Config.encoding config
      |> Data_encoding.Json.to_string
    in
    let open Lwt.Infix in
    Tezos_stdlib_unix.Lwt_utils_unix.create_file config_file str >|= fun r ->
    Error_monad.catch (fun () -> r)

  let params_generate_empty =
    Tezos_clic.(
      prefixes ["config"; "generate"; "empty"; "in"]
      @@ config_file_param () @@ stop)

  let handler_generate_empty () config_file () =
    let config = Config.empty in
    let str =
      Data_encoding.Json.construct Config.encoding config
      |> Data_encoding.Json.to_string
    in
    let open Lwt.Infix in
    Tezos_stdlib_unix.Lwt_utils_unix.create_file config_file str >|= fun r ->
    Error_monad.catch (fun () -> r)

  let config_file_param_dst =
    Tezos_clic.param
      ~name:"DST"
      ~desc:"Configuration file path destination"
      (Tezos_clic.parameter (fun _ s -> Lwt.return_ok s))

  let config_file_param_src =
    Tezos_clic.param
      ~name:"SRC"
      ~desc:"Configuration file path source"
      (Tezos_clic.parameter (fun _ s -> Lwt.return_ok s))

  let params_merge =
    Tezos_clic.(
      prefixes ["config"; "merge"]
      @@ config_file_param_src @@ prefix "in" @@ config_file_param_dst @@ stop)

  let handler_merge delete_flag src dst () =
    let () = Config.merge_config_files ~delete_src:delete_flag ~dst ~src () in
    Lwt_result_syntax.return_unit

  let params_edit =
    Tezos_clic.(
      prefixes ["config"; "edit"]
      @@ config_file_param () @@ prefix "for" @@ benchmark_param () @@ stop)

  let handler_edit (editor, stdin, file, json) config_path namespace () =
    let namespace = Namespace.of_string namespace in
    let input =
      match (editor, stdin, file, json) with
      | Some e, false, None, None -> `Edit e
      | None, _, None, None -> `Stdin
      | None, false, Some f, None -> `File f
      | None, false, None, Some s -> `String s
      | _ ->
          Format.eprintf
            "Config file edition failed: too many input methods given as \
             options@." ;
          exit 1
    in
    let () = Config.edit_config ~input config_path namespace in
    Lwt_result_syntax.return_unit

  let group =
    {
      Tezos_clic.name = "config";
      title = "Commands for manipulating config files";
    }

  let command_check =
    Tezos_clic.command
      ~group
      ~desc:
        "Prints the configuration that would be used for a given benchmark, \
         given a configuration file"
      Tezos_clic.no_options
      params_check
      handler_check

  let command_generate_default =
    Tezos_clic.command
      ~group
      ~desc:
        "Generates a configuration file for the given benchmarks using their \
         default configuration"
      Tezos_clic.no_options
      params_generate_default
      handler_generate_default

  let command_generate_empty =
    Tezos_clic.command
      ~group
      ~desc:"Generates an empty configuration file for the given benchmarks"
      Tezos_clic.no_options
      params_generate_empty
      handler_generate_empty

  let command_merge =
    Tezos_clic.command
      ~group
      ~desc:"Merges multiple configuration files. Fails in case of conflict"
      options_merge
      params_merge
      handler_merge

  let command_edit =
    Tezos_clic.command
      ~group
      ~desc:"Edit configuration file at the given point"
      options_edit
      params_edit
      handler_edit

  let commands =
    [
      command_check;
      command_generate_default;
      command_generate_empty;
      command_merge;
      command_edit;
    ]
end

module Generate_config_cmd = struct
  let params = Tezos_clic.fixed ["generate"; "default-config"]

  let options =
    Tezos_clic.args1
      (Tezos_clic.arg
         ~doc:"save default config to file"
         ~long:"save-to"
         ~placeholder:"filename"
         (Tezos_clic.parameter (fun (_ : unit) parsed -> Lwt.return_ok parsed)))

  let show_config_handler filename () =
    let json =
      Data_encoding.Json.construct
        ~include_default_fields:`Always
        Fixed_point_transform.options_encoding
        Fixed_point_transform.default_options
    in
    (match filename with
    | Some filename ->
        Out_channel.with_open_text filename @@ fun oc ->
        let outfile = Format.formatter_of_out_channel oc in
        Format.fprintf outfile "%a@." Data_encoding.Json.pp json ;
        Format.eprintf "Saved default config to %s@." filename
    | None -> Format.printf "%a@." Data_encoding.Json.pp json) ;
    Lwt.return_ok ()

  let command =
    Tezos_clic.command
      ~desc:
        "Show the default configurations for fixed-point code generation as \
         json"
      options
      params
      show_config_handler
end

module Workload_cmd = struct
  let options_dump =
    Tezos_clic.args1
      (Tezos_clic.arg
         ~doc:"JSON file name to write the content"
         ~short:'o'
         ~long:"out-file"
         ~placeholder:"OUTPUT-FILE"
         (Tezos_clic.parameter (fun () parsed -> Lwt.return_ok parsed)))

  let handler_dump output_path workload_data () =
    let packed_measurement = Measure.load ~filename:workload_data in
    Measure.packed_measurement_save_json packed_measurement output_path ;
    Lwt.return_ok ()

  let params_dump =
    Tezos_clic.(
      prefixes ["workload"; "dump"]
      @@ string ~name:"WORKLOAD-FILE" ~desc:"Workload file name"
      @@ stop)

  let group =
    {
      Tezos_clic.name = "workload";
      title = "Commands for manipulating workload files";
    }

  let command_dump =
    Tezos_clic.command
      ~desc:"Dump the content of a workload file in JSON format"
      ~group
      options_dump
      params_dump
      handler_dump

  let commands = [command_dump]
end

module Display_info_cmd = struct
  let group =
    {
      Tezos_clic.name = "display";
      title =
        "Commands for displaying detailed information for Snoop components";
    }

  let params_prefix = Tezos_clic.prefixes ["display"; "info"; "for"]

  let options = Tezos_clic.no_options

  let normal_block fmt title pp obj =
    Format.fprintf fmt "%s:@;    @[<v>%a@]@." title pp obj

  let bold_block fmt title =
    let bold_title = Format.asprintf "\027[0;1;4m%s\027[m" title in
    normal_block fmt bold_title

  let pp_model = Model.pp

  let pp_model_bench fmt (local_name, model) =
    Format.fprintf fmt "@[<v2>%s:@;%a@]" local_name pp_model model

  let pp_models () = Format.pp_print_list pp_model_bench

  let pp_fancy_benchmark fmt (module B : Benchmark.S) =
    let purpose =
      match B.purpose with
      | Generate_code x -> Format.sprintf "Generate code to %s" x
      | Other_purpose x -> x
    in
    bold_block fmt "Name" Namespace.pp B.name ;
    bold_block fmt "Filename" Format.pp_print_string B.module_filename ;
    bold_block fmt "Purpose" Format.pp_print_string purpose ;
    bold_block fmt "Info" Format.pp_print_string B.info ;
    bold_block fmt "Tags" pp_tags B.tags ;
    bold_block fmt "Models" (pp_models ()) B.models

  let pp_fancy_model (type a) fmt
      ((module M : Model.Model_impl with type arg_type = a), l) =
    let pp_local fmt {Registration.bench_name; local_model_name} =
      Format.fprintf
        fmt
        "\027[0;33;40m(%s)\027[m %a"
        local_model_name
        Namespace.pp
        bench_name
    in
    bold_block fmt "Name" Namespace.pp M.name ;
    bold_block fmt "Expression" Format.pp_print_string (print_model (module M)) ;
    let fv_seq =
      Model.get_free_variable_set (module M) |> Free_variable.Set.to_seq
    in
    bold_block
      fmt
      "Free variables"
      (Format.pp_print_seq Free_variable.pp)
      fv_seq ;
    bold_block
      fmt
      "Registered in the following benchmarks"
      (Format.pp_print_list pp_local)
      l

  let pp_fancy_local_model fmt (local_model_name, benchmark_names) =
    bold_block fmt "Name" Format.pp_print_string local_model_name ;
    bold_block
      fmt
      "Benchmarks"
      (Format.pp_print_list Namespace.pp)
      benchmark_names

  let pp_fancy_parameter fmt (s, l) =
    bold_block fmt "Name" Free_variable.pp s ;
    bold_block fmt "In models" (Format.pp_print_list Namespace.pp) l

  let display_benchmark_handler () s () =
    let s = Namespace.of_string s in
    let b = Registration.find_benchmark_exn s in
    Format.printf "@.%a@." pp_fancy_benchmark b ;
    Lwt.return_ok ()

  let display_all_benchmarks_handler () () =
    List.iter (fun (_n, b) -> Format.printf "%a@." Benchmark.pp b)
    @@ Registration.all_benchmarks () ;
    Lwt.return_ok ()

  let display_model_handler () s () =
    let s = Namespace.of_string s in
    let {Registration.model = Model.Model m; from = l} =
      Registration.find_model_exn s
    in
    Format.printf "@.%a@." pp_fancy_model (m, l) ;
    Lwt.return_ok ()

  let display_local_model_handler () local_model_name () =
    let all_benchmarks = Registration.find_local_model_exn local_model_name in
    Format.printf
      "@.%a@."
      pp_fancy_local_model
      (local_model_name, all_benchmarks) ;
    Lwt.return_ok ()

  let display_parameter_handler () s () =
    let s = Free_variable.of_string s in
    let l = Registration.find_parameter_exn s in
    Format.printf "@.%a@." pp_fancy_parameter (s, l) ;
    Lwt.return_ok ()

  let display_benchmark_params =
    Tezos_clic.(
      params_prefix @@ prefix "benchmark" @@ List_cmd.benchmark_param () @@ stop)

  let display_all_benchmarks_params =
    Tezos_clic.fixed ["display"; "info"; "for"; "all"; "benchmarks"]

  let display_model_params =
    Tezos_clic.(
      params_prefix @@ prefix "model" @@ List_cmd.model_param () @@ stop)

  let display_local_model_params =
    Tezos_clic.(
      params_prefix @@ prefix "local" @@ prefix "model"
      @@ List_cmd.local_model_param ()
      @@ stop)

  let display_parameter_params =
    Tezos_clic.(
      params_prefix @@ prefix "parameter" @@ List_cmd.parameter_param () @@ stop)

  let command_benchmark =
    Tezos_clic.command
      ~group
      ~desc:"Display detailed information on the given benchmark"
      options
      display_benchmark_params
      display_benchmark_handler

  let command_all_benchmarks =
    Tezos_clic.command
      ~group
      ~desc:"Display detailed information on all the benchmarks"
      options
      display_all_benchmarks_params
      display_all_benchmarks_handler

  let command_model =
    Tezos_clic.command
      ~group
      ~desc:"Display detailed information on the given model"
      options
      display_model_params
      display_model_handler

  let command_local_model =
    Tezos_clic.command
      ~group
      ~desc:"Display detailed information on the given local model"
      options
      display_local_model_params
      display_local_model_handler

  let command_parameter =
    Tezos_clic.command
      ~group
      ~desc:"Display detailed information on the given parameter"
      options
      display_parameter_params
      display_parameter_handler

  let commands =
    [
      command_benchmark;
      command_all_benchmarks;
      command_model;
      command_parameter;
      command_local_model;
    ]
end

let all_commands =
  [
    Benchmark_cmd.command;
    Infer_cmd.command;
    Codegen_cmd.command;
    Codegen_all_cmd.command;
    Codegen_inferred_cmd.command;
    Codegen_for_solutions_cmd.command;
    Codegen_check_definitions_cmd.command;
    Generate_config_cmd.command;
    Solution_print_cmd.command;
    Auto_build_cmd.command;
  ]
  @ List_cmd.commands @ Config_cmd.commands @ Workload_cmd.commands
  @ Display_info_cmd.commands
  @ Registration.all_custom_commands ()

module Global_options = struct
  (* --list-solvers *)
  let list_solvers =
    Tezos_clic.switch ~doc:"List all available solvers" ~long:"list-solvers" ()

  (* --list-models *)
  let list_models =
    Tezos_clic.switch ~doc:"List all models" ~long:"list-models" ()

  let options = Tezos_clic.args2 list_solvers list_models
end

let commands_with_man =
  Tezos_clic.add_manual
    ~executable_name:(Filename.basename Sys.executable_name)
    ~global_options:Global_options.options
    (if Unix.isatty Unix.stdout then Tezos_clic.Ansi else Tezos_clic.Plain)
    Format.std_formatter
    all_commands

let usage () =
  Tezos_clic.usage
    Format.std_formatter
    ~executable_name:(Filename.basename Sys.executable_name)
    ~global_options:Global_options.options
    commands_with_man

let original_args, autocomplete =
  (* for shell aliases *)
  let rec move_autocomplete_token_upfront acc = function
    | "bash_autocomplete" :: prev_arg :: cur_arg :: script :: args ->
        let args = List.rev acc @ args in
        (args, Some (prev_arg, cur_arg, script))
    | x :: rest -> move_autocomplete_token_upfront (x :: acc) rest
    | [] -> (List.rev acc, None)
  in
  match Array.to_list Sys.argv with
  | _ :: args -> move_autocomplete_token_upfront [] args
  | [] -> ([], None)

let list_solvers, list_models =
  ignore
    Tezos_clic.(
      setup_formatter
        Format.std_formatter
        (if Unix.isatty Unix.stdout then Ansi else Plain)
        Short) ;
  let result =
    Lwt_main.run
      (let open Lwt_result_syntax in
      let* list_flags, args =
        Tezos_clic.parse_global_options Global_options.options () original_args
      in
      match autocomplete with
      | Some (prev_arg, cur_arg, script) ->
          let* completions =
            Tezos_clic.autocompletion
              ~script
              ~cur_arg
              ~prev_arg
              ~args:original_args
              ~global_options:Global_options.options
              commands_with_man
              ()
          in
          List.iter print_endline completions ;
          return list_flags
      | None -> (
          match args with
          | [] -> return list_flags
          | _ ->
              let* () = Tezos_clic.dispatch commands_with_man () args in
              return list_flags))
  in
  match result with
  | Ok global_options -> global_options
  | Error [Tezos_clic.Version] ->
      let version = Tezos_version_value.Bin_version.version_string in
      Format.printf "%s\n" version ;
      exit 0
  | Error [Tezos_clic.Help command] ->
      Tezos_clic.usage
        Format.std_formatter
        ~executable_name:(Filename.basename Sys.executable_name)
        ~global_options:Global_options.options
        (match command with None -> [] | Some c -> [c]) ;
      exit 0
  | Error errors ->
      Tezos_clic.pp_cli_errors
        Format.err_formatter
        ~executable_name:(Filename.basename Sys.executable_name)
        ~global_options:Global_options.options
        ~default:(fun fmt err -> Error_monad.pp_print_trace fmt [err])
        errors ;
      exit 1
