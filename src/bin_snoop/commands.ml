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

open Cmdline

let lift_opt f opt_arg state =
  match opt_arg with None -> state | Some arg -> f arg state

let parse_parameter f m =
  Clic.parameter (fun (_ : unit) p ->
      Lwt.return
      @@
      match f p with
      | Some x -> Ok x
      | None -> Error_monad.error_with_exn (Failure m))

module Benchmark_cmd = struct
  (* ----------------------------------------------------------------------- *)
  (* Handling the options of the benchmarker *)
  open Measure

  let set_flush_cache flush_opt options = {options with flush_cache = flush_opt}

  let set_stabilize_gc stabilize_gc options = {options with stabilize_gc}

  let set_seed seed (options : options) = {options with seed = Some seed}

  let set_nsamples nsamples options = {options with nsamples}

  let set_determinizer determinizer options = {options with determinizer}

  let set_cpu_affinity cpu_affinity options = {options with cpu_affinity}

  let set_save_file save_file options = {options with save_file}

  let set_csv_export csv_export (options : Cmdline.benchmark_options) =
    {options with csv_export}

  let set_storage_kind storage options = {options with storage}

  let set_bench_number bench_number options = {options with bench_number}

  let set_minor_heap_size minor_heap_size options =
    {options with minor_heap_size}

  let set_config_dir config_dir options = {options with config_dir}

  let default_benchmark_options =
    let options =
      {
        flush_cache = `Dont;
        stabilize_gc = false;
        seed = None;
        nsamples = 3000;
        determinizer = Percentile 50;
        cpu_affinity = None;
        bench_number = 300;
        minor_heap_size = `words (256 * 1024);
        config_dir = None;
      }
    in
    {
      options;
      save_file = "<This field /will/ be set by Clic>";
      storage = Memory;
      csv_export = None;
    }

  let benchmark_options_to_string (options : Cmdline.benchmark_options) =
    let open Printf in
    let save_file = options.save_file in
    let storage =
      match options.storage with
      | Memory -> "Mem"
      | Disk {source; base_dir; header_json} ->
          let pp_src =
            Format.asprintf "%a" Signature.Public_key_hash.pp source
          in
          sprintf
            "Disk { source = %s ; base_dir = %s ; header_json = %s }"
            pp_src
            base_dir
            header_json
    in
    Format.asprintf
      "@[<v 2>{ options = %a;@, save_file = %s;@, storage = %s }@]"
      Measure.pp_options
      options.options
      save_file
      storage

  (* ----------------------------------------------------------------------- *)
  (* "benchmark" command handler *)

  let benchmark_handler
      ( cache,
        gc,
        det,
        nsamples,
        seed,
        cpu_affinity,
        bench_number,
        minor_heap_size,
        config_dir,
        csv_export ) bench_name save_file () =
    let options =
      default_benchmark_options.options
      |> lift_opt set_flush_cache cache
      |> set_stabilize_gc gc
      |> lift_opt set_determinizer det
      |> lift_opt set_nsamples nsamples
      |> lift_opt set_seed seed
      |> set_cpu_affinity cpu_affinity
      |> lift_opt set_bench_number bench_number
      |> lift_opt set_minor_heap_size minor_heap_size
      |> set_config_dir config_dir
    in
    let options =
      {default_benchmark_options with options}
      |> set_save_file save_file |> set_csv_export csv_export
    in
    commandline_outcome_ref :=
      Some (Benchmark {bench_name; bench_opts = options}) ;
    Lwt.return_ok ()

  module Options = struct
    (* Argument --flush-cache mb
       Parmeter: size in megabytes of the cache. *)
    let flush_cache_arg =
      let flush_cache_arg_param =
        parse_parameter
          (fun p ->
            Option.map (fun p -> `Cache_megabytes p) (int_of_string_opt p))
          "Error while parsing --flush-cache argument."
      in
      Clic.arg
        ~doc:"Force flushing the cache before each measurement"
        ~long:"flush-cache"
        ~placeholder:"cache size in megabytes"
        flush_cache_arg_param

    (* Boolean argument --stabilize-gc *)
    let stabilize_gc_arg =
      Clic.switch
        ~doc:"Major GC until fixpoint before each measurement"
        ~long:"stabilize-gc"
        ()

    (* Sum type [mean| percentile [1-100]] argument --determinizer *)
    let determinizer_arg =
      let determinizer_arg_param =
        Clic.parameter
          ~autocomplete:(fun () -> Lwt.return_ok ["mean"; "percentile@[0-100]"])
          (fun (_ : unit) parsed ->
            match parsed with
            | "mean" -> Lwt.return_ok Mean
            | s -> (
                let error () =
                  Printf.eprintf "Wrong determinizer specification.\n" ;
                  exit 1
                in
                match String.split_on_char '@' s with
                | ["percentile"; i] ->
                    let i =
                      Option.value_f (int_of_string_opt i) ~default:error
                    in
                    if i < 1 || i > 100 then error ()
                    else Lwt.return_ok (Percentile i)
                | _ -> error ()))
      in
      Clic.arg
        ~doc:"Method for determinizing empirical timing distribution"
        ~long:"determinizer"
        ~placeholder:"{mean | percentile@[1-100]}"
        determinizer_arg_param

    (* Int argument --cpu-affinity
       Parameter: Id of the CPU where to preferentially pin the benchmark *)
    let cpu_affinity_arg =
      let cpu_affinity_arg_param =
        parse_parameter
          int_of_string_opt
          "Error while parsing --cpu-affinity argument."
      in
      Clic.arg
        ~doc:"Sets CPU affinity"
        ~long:"cpu-affinity"
        ~placeholder:"CPU id"
        cpu_affinity_arg_param

    (* Integer argument --nsamples *)
    let nsamples_arg =
      let nsamples_arg_param =
        parse_parameter
          int_of_string_opt
          "Error while parsing --nsamples argument."
      in
      Clic.arg
        ~doc:"Number of samples per benchmark"
        ~long:"nsamples"
        ~placeholder:"strictly positive int"
        nsamples_arg_param

    (* Integer argument --seed *)
    let seed_arg =
      let seed =
        parse_parameter int_of_string_opt "Error while parsing --seed argument."
      in
      Clic.arg ~doc:"RNG seed" ~long:"seed" ~placeholder:"int" seed

    (* String argument --dump-csv
       Parameter: filename of the file where to write the csv data. *)
    let dump_csv_arg =
      let dump_csv_arg_param =
        Clic.parameter (fun (_ : unit) parsed -> Lwt.return_ok parsed)
      in
      Clic.arg
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
      Clic.arg
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
      Clic.arg
        ~doc:"Size of minor heap in words"
        ~long:"minor-heap-size"
        ~placeholder:"strictly positive int"
        minor_heap_size_param

    let config_dir_arg =
      let config_dir_arg_param =
        Clic.parameter (fun (_ : unit) parsed -> Lwt.return_ok parsed)
      in
      Clic.arg
        ~doc:
          "Specify directory where to search for benchmark configuration files"
        ~short:'c'
        ~long:"config-dir"
        ~placeholder:"directory"
        config_dir_arg_param
  end

  let options =
    let open Options in
    Clic.args10
      flush_cache_arg
      stabilize_gc_arg
      determinizer_arg
      nsamples_arg
      seed_arg
      cpu_affinity_arg
      bench_number_arg
      minor_heap_size_arg
      config_dir_arg
      dump_csv_arg

  let benchmark_param =
    Clic.param
      ~name:"BENCH-NAME"
      ~desc:"Name of the benchmark"
      (Clic.parameter
         ~autocomplete:(fun _ ->
           let res =
             List.map
               (fun (module Bench : Benchmark.S) -> Bench.name)
               (Registration.all_benchmarks ())
           in
           Lwt.return_ok res)
         (fun _ str -> Lwt.return_ok str))

  let params =
    Clic.(
      prefix "benchmark" @@ benchmark_param
      @@ prefixes ["and"; "save"; "to"]
      @@ string
           ~name:"FILENAME"
           ~desc:"Name of the file where to save the workload data"
      @@ stop)

  let group =
    {
      Clic.name = "benchmark";
      title = "Commands for benchmarking parts of the protocol";
    }

  let command =
    Clic.command ~group ~desc:"Runs benchmarks" options params benchmark_handler
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

  let list_solvers () =
    Printf.eprintf "ridge --ridge-alpha=<float>\n" ;
    Printf.eprintf "lasso --lasso-alpha=<float> --lasso-positive\n" ;
    Printf.eprintf "nnls\n%!"

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
        dot_file ) model_name workload_data solver () =
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
    in
    commandline_outcome_ref :=
      Some (Infer {model_name; workload_data; solver; infer_opts = options}) ;
    Lwt.return_ok ()

  module Options = struct
    (* Boolean argument --print-problem *)
    let print_problem =
      Clic.switch
        ~doc:"Prints problem as obtained after applying model to workload data"
        ~long:"print-problem"
        ()

    (* Boolean argument --plot *)
    let plot_arg =
      Clic.switch ~doc:"Plot results of parameter inference" ~long:"plot" ()

    (* Float argument --ridge-alpha *)
    let ridge_alpha_arg =
      let ridge_alpha_arg_param =
        parse_parameter
          float_of_string_opt
          "Error while parsing --ridge-alpha argument."
      in
      Clic.arg
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
      Clic.arg
        ~doc:"Regularization parameter for lasso regression"
        ~long:"lasso-alpha"
        ~placeholder:"positive float"
        lasso_alpha_arg_param

    (* Boolean argument --lasso-positive *)
    let lasso_positive_arg =
      Clic.switch
        ~doc:"Constrains solution of lasso regression to be positive"
        ~long:"lasso-positive"
        ()

    let dump_csv_arg =
      let dump_csv_arg_param =
        Clic.parameter (fun (_ : unit) parsed -> Lwt.return_ok parsed)
      in
      Clic.arg
        ~doc:"Dumps solution of inference to a CSV file"
        ~long:"dump-csv"
        ~placeholder:"filename"
        dump_csv_arg_param

    let report_arg =
      let dump_report_param =
        Clic.parameter (fun (_ : unit) parsed -> Lwt.return_ok parsed)
      in
      Clic.arg
        ~doc:"Produces a detailed report"
        ~long:"report"
        ~placeholder:"filename"
        dump_report_param

    let override_arg =
      let override_file_param =
        Clic.parameter (fun (_ : unit) parsed ->
            let files = String.split_no_empty ',' parsed in
            Lwt.return_ok files)
      in
      Clic.arg
        ~doc:"Specify CSV file containing overrided variables for inference"
        ~long:"override-csv"
        ~placeholder:"filename"
        override_file_param

    let save_solution_arg =
      let override_file_param =
        Clic.parameter (fun (_ : unit) parsed -> Lwt.return_ok parsed)
      in
      Clic.arg
        ~doc:
          "Specify file to which inference solution will be saved for code \
           generation"
        ~long:"save-solution"
        ~placeholder:"filename"
        override_file_param

    let dot_file_arg =
      let override_file_param =
        Clic.parameter (fun (_ : unit) parsed -> Lwt.return_ok parsed)
      in
      Clic.arg
        ~doc:
          "Specify file to which dependency graph will be saved in graphviz \
           format"
        ~long:"dot-file"
        ~placeholder:"filename"
        override_file_param
  end

  let options =
    let open Options in
    Clic.args10
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

  let model_param =
    Clic.param
      ~name:"MODEL-NAME"
      ~desc:"Name of the model for which to infer parameter"
      (Clic.parameter
         ~autocomplete:(fun _ ->
           Lwt.return_ok (Registration.all_model_names ()))
         (fun _ str -> Lwt.return_ok str))

  let regression_param =
    Clic.param
      ~name:"REGRESSION-METHOD"
      ~desc:"Regression method used"
      (Clic.parameter
         ~autocomplete:(fun _ -> Lwt.return_ok ["lasso"; "ridge"; "nnls"])
         (fun _ str -> Lwt.return_ok str))

  let params =
    Clic.(
      prefixes ["infer"; "parameters"; "for"; "model"]
      @@ model_param
      @@ prefixes ["on"; "data"]
      @@ string
           ~name:"WORKLOAD-DATA"
           ~desc:"File or directory containing workload data"
      @@ prefix "using" @@ regression_param @@ stop)

  let group =
    {
      Clic.name = "inference";
      title = "Command for infering parameters of cost models";
    }

  let command =
    Clic.command
      ~desc:"Perform parameter inference"
      ~group
      options
      params
      infer_handler
end

module Cull_outliers_cmd = struct
  (* ----------------------------------------------------------------------- *)
  (* Handling options for the "cull outliers" command *)

  let cull_handler () workload_data sigmas save_file () =
    let nsigmas =
      match float_of_string_opt sigmas with
      | Some s -> s
      | None ->
          Printf.eprintf "Could not parse back float value for nsigmas.\n" ;
          exit 1
    in
    commandline_outcome_ref :=
      Some (Cull_outliers {workload_data; nsigmas; save_file}) ;
    Lwt.return_ok ()

  let options = Clic.no_options

  let params =
    Clic.(
      prefixes ["remove"; "outliers"; "from"; "data"]
      @@ string
           ~name:"WORKLOAD-DATA-FILE-IN"
           ~desc:"File containing input workload data"
      @@ prefixes ["above"]
      @@ string
           ~name:"SIGMAS"
           ~desc:
             "Standard deviations around the mean above which data will be \
              culled"
      @@ prefixes ["sigmas"]
      @@ prefixes ["and"; "save"; "to"]
      @@ string
           ~name:"WORKLOAD-DATA-FILE-OUT"
           ~desc:"File to which cleaned workload data will be saved"
      @@ stop)

  let group =
    {
      Clic.name = "cull_outlier";
      title = "Command for removing outliers from raw data";
    }

  let command =
    Clic.command ~group ~desc:"Cull outliers" options params cull_handler
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
      Fixed_point_transform
        (Data_encoding.Json.destruct
           Fixed_point_transform.options_encoding
           json)
    with _ ->
      Format.eprintf
        "Could not parse fixed-point transform parameters; aborting@." ;
      Format.eprintf "Here's a well-formed file:@." ;
      Format.eprintf
        "%a@."
        Data_encoding.Json.pp
        (Data_encoding.Json.construct
           Fixed_point_transform.options_encoding
           Fixed_point_transform.default_options) ;
      exit 1

  let codegen_handler json solution model_name () =
    let codegen_options =
      match json with
      | None -> No_transform
      | Some json_file -> load_fixed_point_parameters json_file
    in
    commandline_outcome_ref :=
      Some (Codegen {solution; model_name; codegen_options}) ;
    Lwt.return_ok ()

  let options =
    Clic.args1
      (Clic.arg
         ~doc:"Apply fixed-point transform to the model"
         ~long:"fixed-point"
         ~placeholder:"json-config-file"
         (Clic.parameter (fun () filename -> Lwt.return_ok filename)))

  let model_param =
    Clic.param
      ~name:"MODEL-NAME"
      ~desc:"Name of the model for which to generate code"
      (Clic.parameter
         ~autocomplete:(fun _ ->
           let res =
             List.map
               (fun (name, _) -> name)
               (Registration.all_registered_models ())
           in
           Lwt.return_ok res)
         (fun _ str -> Lwt.return_ok str))

  let params =
    Clic.(
      prefixes ["generate"; "code"; "using"; "solution"]
      @@ string
           ~name:"SOLUTION-FILE"
           ~desc:
             "File containing solution, as obtained using the --save-solution \
              switch"
      @@ prefixes ["and"; "model"]
      @@ model_param @@ stop)

  let group = {Clic.name = "codegen"; title = "Command for generating code"}

  let command =
    Clic.command
      ~group
      ~desc:"Generate code for a specific model"
      options
      params
      codegen_handler
end

module Codegen_all_cmd = struct
  include Codegen_cmd

  let codegen_all_handler json solution matching () =
    let codegen_options =
      match json with
      | None -> No_transform
      | Some json_file -> load_fixed_point_parameters json_file
    in
    commandline_outcome_ref :=
      Some (Codegen_all {solution; matching; codegen_options}) ;
    Lwt.return_ok ()

  let params =
    Clic.(
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
    Clic.command
      ~group
      ~desc:"Generate code for all models matching regexp"
      options
      params
      codegen_all_handler
end

module List_cmd = struct
  (* ------------------------------------------------------------------------- *)

  let tag_param =
    Clic.param
      ~name:"TAG"
      ~desc:"Tag of a benchmark"
      (Clic.parameter
         ~autocomplete:(fun _ -> Lwt.return_ok (Registration.all_tags ()))
         (fun _ s -> Lwt.return_ok s))

  let params_all_bench = Clic.fixed ["list"; "all"; "benchmarks"]

  let option_show_tags =
    Clic.args1
      (Clic.switch
         ~long:"show-tags"
         ~short:'t'
         ~doc:"Show the tags of the benchmarks"
         ())

  let base_handler_bench bench_list show_tags =
    if show_tags then
      List.iter
        (fun (module Bench : Benchmark.S) ->
          Format.fprintf
            Format.std_formatter
            "%s: %s\n\tTags: %a\n"
            Bench.name
            Bench.info
            (Format.pp_print_list
               ~pp_sep:(fun formatter () -> Format.fprintf formatter "; ")
               Format.pp_print_string)
            Bench.tags)
        bench_list
    else
      List.iter
        (fun (module Bench : Benchmark.S) ->
          Format.fprintf Format.std_formatter "%s: %s\n" Bench.name Bench.info)
        bench_list ;
    Lwt_tzresult_syntax.return_unit

  let handler_all_bench show_tags () =
    base_handler_bench (Registration.all_benchmarks ()) show_tags

  let params_all_tags = Clic.fixed ["list"; "all"; "tags"]

  let handler_all_tags () () =
    List.iter
      (fun tag -> Format.fprintf Format.std_formatter "%s\n" tag)
      (Registration.all_tags ()) ;
    Lwt_tzresult_syntax.return_unit

  let params_bench_tags_any =
    Clic.(
      prefixes ["list"; "benchmarks"; "with"; "tags"; "any"; "of"]
      @@ seq_of_param tag_param)

  let handler_bench_tags_any show_tags tags () =
    base_handler_bench (Registration.all_benchmarks_with_any_of tags) show_tags

  let params_bench_tags_all =
    Clic.(
      prefixes ["list"; "benchmarks"; "with"; "tags"; "all"; "of"]
      @@ seq_of_param tag_param)

  let handler_bench_tags_all show_tags tags () =
    base_handler_bench (Registration.all_benchmarks_with_all_of tags) show_tags

  let params_bench_tags_exact =
    Clic.(
      prefixes ["list"; "benchmarks"; "with"; "tags"; "exactly"]
      @@ seq_of_param tag_param)

  let handler_bench_tags_exact show_tags tags () =
    base_handler_bench (Registration.all_benchmarks_with_exactly tags) show_tags

  let group = {Clic.name = "list"; title = "Commands for displaying lists"}

  let command_all_bench =
    Clic.command
      ~group
      ~desc:"List all implemented benchmarks"
      option_show_tags
      params_all_bench
      handler_all_bench

  let command_all_tags =
    Clic.command
      ~group
      ~desc:"List all available tags"
      Clic.no_options
      params_all_tags
      handler_all_tags

  let command_bench_tags_any =
    Clic.command
      ~group
      ~desc:"List all implemented benchmarks containing any of the given tags"
      option_show_tags
      params_bench_tags_any
      handler_bench_tags_any

  let command_bench_tags_all =
    Clic.command
      ~group
      ~desc:"List all implemented benchmarks containing all of the given tags"
      option_show_tags
      params_bench_tags_all
      handler_bench_tags_all

  let command_bench_tags_exact =
    Clic.command
      ~group
      ~desc:"List all implemented benchmarks containing exactly the given tags"
      option_show_tags
      params_bench_tags_exact
      handler_bench_tags_exact

  let commands =
    [
      command_all_bench;
      command_all_tags;
      command_bench_tags_any;
      command_bench_tags_all;
      command_bench_tags_exact;
    ]
end

let all_commands =
  [
    Benchmark_cmd.command;
    Infer_cmd.command;
    Cull_outliers_cmd.command;
    Codegen_cmd.command;
    Codegen_all_cmd.command;
  ]
  @ List_cmd.commands
  @ Registration.all_custom_commands ()

module Global_options = struct
  (* --list-solvers *)
  let list_solvers =
    Clic.switch ~doc:"List all available solvers" ~long:"list-solvers" ()

  (* --list-models *)
  let list_models = Clic.switch ~doc:"List all models" ~long:"list-models" ()

  let options = Clic.args2 list_solvers list_models
end

let commands_with_man =
  Clic.add_manual
    ~executable_name:(Filename.basename Sys.executable_name)
    ~global_options:Global_options.options
    (if Unix.isatty Unix.stdout then Clic.Ansi else Clic.Plain)
    Format.std_formatter
    all_commands

let usage () =
  Clic.usage
    Format.std_formatter
    ~executable_name:(Filename.basename Sys.executable_name)
    ~global_options:Global_options.options
    commands_with_man

let (original_args, autocomplete) =
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

let (list_solvers, list_models) =
  ignore
    Clic.(
      setup_formatter
        Format.std_formatter
        (if Unix.isatty Unix.stdout then Ansi else Plain)
        Short) ;
  let result =
    Lwt_main.run
      (let open Lwt_result_syntax in
      let* (list_flags, args) =
        Clic.parse_global_options Global_options.options () original_args
      in
      match autocomplete with
      | Some (prev_arg, cur_arg, script) ->
          let* completions =
            Clic.autocompletion
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
              let* () = Clic.dispatch commands_with_man () args in
              return list_flags))
  in
  match result with
  | Ok global_options -> global_options
  | Error [Clic.Version] ->
      let version = Tezos_version.Bin_version.version_string in
      Format.printf "%s\n" version ;
      exit 0
  | Error [Clic.Help command] ->
      Clic.usage
        Format.std_formatter
        ~executable_name:(Filename.basename Sys.executable_name)
        ~global_options:Global_options.options
        (match command with None -> [] | Some c -> [c]) ;
      exit 0
  | Error errors ->
      Clic.pp_cli_errors
        Format.err_formatter
        ~executable_name:(Filename.basename Sys.executable_name)
        ~global_options:Global_options.options
        ~default:(fun fmt err -> Error_monad.pp_print_trace fmt [err])
        errors ;
      exit 1
