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

(* -------------------------------------------------------------------------- *)
(* Typedefs *)

(* Benchmark command related types *)

type determinizer_option = Percentile of int | Mean

type storage_kind =
  | Memory
  | Disk of {
      source : Tezos_crypto.Signature.public_key_hash;
      base_dir : string;
      header_json : string;
    }

type benchmark_options = {
  options : Measure.options;
  save_file : string;
  storage : storage_kind;
  csv_export : string option;
}

type codegen_options = {
  transform : Fixed_point_transform.options option;
  save_to : string option;
  split : bool;
}

(* Infer command related types *)

type report = NoReport | ReportToStdout | ReportToFile of string

type infer_parameters_options = {
  print_problem : bool;
  (* Dump the regression problem *)
  csv_export : string option;
  (* Export solution to csv *)
  plot : bool;
  (* Plot solution *)
  ridge_alpha : float;
  (* Regularisation parameter for ridge regression *)
  lasso_alpha : float;
  (* Regularisation parameter for lasso regression *)
  lasso_positive : bool;
  (* Constrain lasso solution to be positive *)
  override_files : string list option;
  (* Source of CSV files for overriding free variables *)
  report : report;
  (* LaTeX report parameters *)
  save_solution : string option;
  (* Serialise solution to given file *)
  dot_file : string option; (* Export dependency graph to graphviz format *)
  display : Display.options;
}

type auto_build_options = {
  destination_directory : string option;
  (* Where to load/save files *)
  infer_parameters : infer_parameters_options;
  measure_options : Measure.options;
}

(* Outcome of command-line parsing. *)

type command =
  | Benchmark of {bench_name : Namespace.t; bench_opts : benchmark_options}
  | Infer of {
      local_model_name : string;
      workload_data : string;
      solver : string;
      infer_opts : infer_parameters_options;
    }
  | Infer_all of {
      workload_data : string;
      solver : string;
      infer_opts : infer_parameters_options;
    }
  | Codegen of {
      solution : string;
      model_name : Namespace.t;
      codegen_options : codegen_options;
    }
  | Codegen_all of {
      solution : string;
      matching : string;
      codegen_options : codegen_options;
    }
  | Codegen_inferred of {solution : string; codegen_options : codegen_options}
  | Codegen_for_solutions of {
      solutions : string list;
      codegen_options : codegen_options;
    }
  | Codegen_check_definitions of {files : string list}
  | Solution_print of string list
  | Auto_build of {
      split : bool;
      targets : auto_build_targets;
      auto_build_options : auto_build_options;
    }
  | No_command

and auto_build_targets =
  | Benchmarks of Benchmark.t list
  | Models of Model.packed_model list
  | Parameters of Free_variable.t list

(* -------------------------------------------------------------------------- *)
(* Encodings *)

let storage_kind_encoding : storage_kind Data_encoding.t =
  let open Data_encoding in
  union
    [
      case
        ~title:"memory"
        (Tag 0)
        unit
        (function Memory -> Some () | Disk _ -> None)
        (fun () -> Memory);
      case
        ~title:"disk"
        (Tag 1)
        (tup3 Tezos_crypto.Signature.Public_key_hash.encoding string string)
        (function
          | Memory -> None
          | Disk {source; base_dir; header_json} ->
              Some (source, base_dir, header_json))
        (fun (source, base_dir, header_json) ->
          Disk {source; base_dir; header_json});
    ]

let benchmark_options_encoding =
  (* : benchmark_options Data_encoding.encoding in *)
  let open Data_encoding in
  def "benchmark_options_encoding"
  @@ conv
       (fun {options; save_file; storage; csv_export} ->
         (options, save_file, storage, csv_export))
       (fun (options, save_file, storage, csv_export) ->
         {options; save_file; storage; csv_export})
       (obj4
          (req "options" Measure.options_encoding)
          (req "save_file" string)
          (req "storage" storage_kind_encoding)
          (opt "csv-export" string))

(* -------------------------------------------------------------------------- *)
(* Global state set by command line parsing. Custom benchmark commands need
   not set this variable. *)

let commandline_outcome_ref : command option ref = ref None
