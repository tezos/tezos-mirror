(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
(* Constants *)

let bench_config name = Files.working_dir // sf "%s.json" name

let meta_config_file = Files.working_dir // "bench_config.json"

let default_bench_num = 300

let default_nsamples = 500

(* ------------------------------------------------------------------------- *)
(* Helpers *)

let rm_config ~file = Lwt_unix.unlink file

(* Some benchmarks require specific parameters.
   This is a bit brittle, as benchmark names fluctuate... *)
let ( .%{}<- ) json key value = Ezjsonm.update json key (Some value)

(* ------------------------------------------------------------------------- *)
(* Default configs *)

(* It would be better if snoop could output the default
   config of a benchmark. *)

let range min max = Ezjsonm.(dict [("min", int min); ("max", int max)])

let bench_name_to_file_name bench_name =
  String.split_on_char '/' bench_name |> String.concat "__"

let sapling_benchmark_config =
  let open Ezjsonm in
  let sapling_parameters =
    let dir = Files.(working_dir // sapling_data_dir) in
    dict [("sapling_txs_file", string dir); ("seed", `Null)]
  in
  dict [("sapling", sapling_parameters)]

let interpreter_benchmark_config =
  let open Ezjsonm in
  (* Must be equal to the one in Interpreter_benchmarks.Default_config
     of Proto alpha *)
  let sampler_parameters =
    dict
      [
        ("int_size", range 8 100000);
        ("string_size", range 1024 131072);
        ("bytes_size", range 1024 131072);
        ("list_size", range 10 1000);
        ("set_size", range 10 1000);
        ("map_size", range 10 1000);
      ]
  in

  let sapling_parameters =
    let dir = Files.(working_dir // sapling_data_dir) in
    dict [("sapling_txs_file", string dir); ("seed", `Null)]
  in
  let comb_parameters = dict [("max_depth", int 500)] in
  let compare_parameters = dict [("type_size", range 1 15)] in
  dict
    [
      ("sampler", sampler_parameters);
      ("sapling", sapling_parameters);
      ("comb", comb_parameters);
      ("compare", compare_parameters);
    ]

let typecheck_benchmark_config dir =
  let open Ezjsonm in
  dict
    [
      ( "generator_config",
        dict [("target_size", range 10 1000); ("burn_in_multiplier", int 5)] );
      ("michelson_terms_file", string dir);
    ]

(* ------------------------------------------------------------------------- *)
(* Config patching *)

(* Having to patch here is annoying. Some of these constants should go to
   the benchmark definition site. *)

type parameters = {nsamples : int; bench_num : int}

type patch_rule =
  Tezt.Base.rex
  * (Ezjsonm.value option * parameters ->
    (Ezjsonm.value option * parameters) Lwt.t)

(* [patches] are applied top-down *)
let patch_benchmark_config ~patches ~bench_name init =
  Lwt_list.fold_left_s
    (fun acc (regex, callback) ->
      if bench_name =~ regex then callback acc else Lwt.return acc)
    init
    patches

let with_config_dir snoop bench_name json_opt f =
  match json_opt with
  | None -> f None
  | Some json ->
      Log.info "Benchmark %s: using patched configuration" bench_name ;
      let config_file = bench_config (bench_name_to_file_name bench_name) in
      Files.write_json json config_file ;
      let* () =
        Snoop.write_config
          ~benchmark:bench_name
          ~bench_config:config_file
          ~file:meta_config_file
          snoop
      in
      let* () = f (Some meta_config_file) in
      rm_config ~file:config_file

let perform_benchmarks (patches : patch_rule list) snoop benchmarks =
  Lwt_list.iter_s
    (fun bench_name ->
      (* Check if target file exists, if it does we skip. *)
      let bench_file_name = bench_name_to_file_name bench_name in
      let save_to =
        Files.(working_dir // benchmark_results_dir // workload bench_file_name)
      in
      let* exists = Lwt_unix.file_exists save_to in
      if exists then (
        Log.info
          "Benchmark %s: target %s already exists, skipping"
          bench_name
          save_to ;
        return ())
      else
        let* config, {nsamples; bench_num} =
          patch_benchmark_config
            ~patches
            ~bench_name
            (None, {nsamples = default_nsamples; bench_num = default_bench_num})
        in
        with_config_dir snoop bench_name config (fun config_file ->
            Snoop.benchmark
              ~bench_name
              ~bench_num
              ~nsamples
              ~save_to:
                Files.(
                  working_dir // benchmark_results_dir
                  // workload bench_file_name)
              ?config_file
              ~csv_dump:
                Files.(
                  working_dir // benchmark_results_dir // csv bench_file_name)
              ~seed:87612786
              snoop))
    benchmarks

let perform_interpreter_benchmarks snoop proto =
  (* Patches are applied from top to bottom *)
  let patches =
    [
      ( rex "Concat_(bytes|string).*",
        fun (json, parameters) ->
          let json = Option.value ~default:interpreter_benchmark_config json in
          let json =
            json.%{["sampler"; "list_size"; "max"]} <- Ezjsonm.int 100
          in
          let json =
            json.%{["sampler"; "bytes_size"; "max"]} <- Ezjsonm.int 1024
          in
          let json =
            json.%{["sampler"; "string_size"; "max"]} <- Ezjsonm.int 1024
          in
          return (Some json, parameters) );
      ( rex "Pairing_check_bls12_381.*",
        fun (json, parameters) -> return (json, {parameters with nsamples = 100})
      );
      ( rex "Sapling_verify_update.*",
        fun (_json, parameters) ->
          return (Some interpreter_benchmark_config, parameters) );
      (* This patch must come at the end to make sure each alloc benchmark has only
         1 sample. *)
      ( rex "/alloc",
        fun (json, parameters) -> return (json, {parameters with nsamples = 1})
      );
    ]
  in
  let* benches =
    Snoop.(list_benchmarks ~mode:All ~tags:[Interpreter; Proto proto] snoop)
  in
  perform_benchmarks patches snoop benches

let create_config file (json_opt, parameters) =
  let* dir_nonempty =
    Files.(is_directory_nonempty (working_dir // michelson_data_dir))
  in
  if dir_nonempty then
    let json =
      typecheck_benchmark_config
        Files.(working_dir // michelson_data_dir // file)
    in
    return (Some json, parameters)
  else return (json_opt, parameters)

let perform_typechecker_benchmarks snoop proto =
  let patches =
    [
      ( rex "(TYPECHECKING|UNPARSING)_CODE.*",
        create_config Files.michelson_code_file );
      ( rex "(TYPECHECKING|UNPARSING)_DATA.*",
        create_config Files.michelson_data_file );
      (rex "VALUE_SIZE.*", create_config Files.michelson_data_file);
      (rex "KINSTR_SIZE.*", create_config Files.michelson_code_file);
    ]
  in
  let* benches =
    Snoop.(list_benchmarks ~mode:All ~tags:[Translator; Proto proto] snoop)
  in
  perform_benchmarks patches snoop benches

let perform_encoding_benchmarks snoop proto =
  let patches =
    [
      ( rex "(ENCODING|DECODING)_MICHELINE.*",
        create_config Files.michelson_data_file );
    ]
  in
  let* proto_indepenent =
    Snoop.(list_benchmarks ~mode:Exactly ~tags:[Encoding; Shell] snoop)
  in
  let* proto_specific =
    Snoop.(list_benchmarks ~mode:All ~tags:[Encoding; Proto proto] snoop)
  in
  let benches = proto_indepenent @ proto_specific in
  perform_benchmarks patches snoop benches

let perform_global_constants_benchmarks snoop =
  let* benches =
    Snoop.(list_benchmarks ~mode:All ~tags:[Global_constants] snoop)
  in
  perform_benchmarks [] snoop benches

let perform_cache_benchmarks snoop =
  let* benches = Snoop.(list_benchmarks ~mode:All ~tags:[Cache] snoop) in
  perform_benchmarks [] snoop benches

let perform_tickets_benchmarks snoop proto =
  let* benches =
    Snoop.(list_benchmarks ~mode:All ~tags:[Tickets; Proto proto] snoop)
  in
  perform_benchmarks [] snoop benches

let perform_misc_benchmarks snoop =
  let* benches = Snoop.(list_benchmarks ~mode:All ~tags:[Misc] snoop) in
  perform_benchmarks [] snoop benches

let perform_carbonated_map_benchmarks snoop proto =
  let* benches =
    Snoop.(list_benchmarks ~mode:All ~tags:[Carbonated_map; Proto proto] snoop)
  in
  perform_benchmarks [] snoop benches

let perform_big_map_benchmarks snoop proto =
  let* benches =
    Snoop.(list_benchmarks ~mode:All ~tags:[Big_map; Proto proto] snoop)
  in
  perform_benchmarks [] snoop benches

let perform_skip_list_benchmarks snoop _proto =
  let* benches = Snoop.(list_benchmarks ~mode:All ~tags:[Skip_list] snoop) in
  perform_benchmarks [] snoop benches

let perform_sc_rollup_benchmarks snoop proto =
  let* benches =
    Snoop.(list_benchmarks ~mode:All ~tags:[Sc_rollup; Proto proto] snoop)
  in
  perform_benchmarks [] snoop benches

let perform_shell_micheline_benchmarks snoop =
  let* benches =
    Snoop.(list_benchmarks ~mode:All ~tags:[Shell; Micheline] snoop)
  in
  perform_benchmarks [] snoop benches

let perform_sapling_benchmarks snoop =
  let patches =
    [
      ( rex ".*",
        fun (_json, parameters) ->
          return (Some sapling_benchmark_config, parameters) );
    ]
  in
  let* benches = Snoop.(list_benchmarks ~mode:All ~tags:[Sapling] snoop) in
  perform_benchmarks patches snoop benches

let perform_dal_benchmarks snoop =
  let* benches = Snoop.(list_benchmarks ~mode:All ~tags:[Dal] snoop) in
  perform_benchmarks [] snoop benches

let main protocol =
  Log.info "Entering Perform_inference.main" ;
  let snoop = Snoop.create () in
  let* () = perform_misc_benchmarks snoop in
  let* () = perform_interpreter_benchmarks snoop protocol in
  let* () = perform_typechecker_benchmarks snoop protocol in
  let* () = perform_tickets_benchmarks snoop protocol in
  let* () = perform_global_constants_benchmarks snoop in
  let* () = perform_cache_benchmarks snoop in
  let* () = perform_encoding_benchmarks snoop protocol in
  let* () = perform_big_map_benchmarks snoop protocol in
  let* () = perform_skip_list_benchmarks snoop protocol in
  let* () = perform_carbonated_map_benchmarks snoop protocol in
  let* () = perform_sc_rollup_benchmarks snoop protocol in
  let* () = perform_shell_micheline_benchmarks snoop in
  let* () = perform_dal_benchmarks snoop in
  perform_sapling_benchmarks snoop
