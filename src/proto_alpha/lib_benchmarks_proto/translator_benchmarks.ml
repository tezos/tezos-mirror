(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol

module Config = struct
  type config = {
    generator_config : Michelson_generation.generator_config;
    michelson_terms_file : string option;
  }

  let default_config =
    {
      generator_config = Michelson_generation.default;
      michelson_terms_file = None;
    }

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {generator_config; michelson_terms_file} ->
        (generator_config, michelson_terms_file))
      (fun (generator_config, michelson_terms_file) ->
        {generator_config; michelson_terms_file})
      (obj2
         (req "generator_config" Michelson_generation.generator_config_encoding)
         (opt "michelson_terms_file" string))
end

module Default_boilerplate = struct
  type workload = Translator_workload.t

  let workload_encoding = Translator_workload.encoding

  let workload_to_vector = Translator_workload.workload_to_sparse_vec

  let tags = [Tags.translator]

  let make_models t_kind code_or_data =
    [ ( "gas_translator_model",
        Translator_model.gas_based_model t_kind code_or_data );
      ( "size_translator_model",
        Translator_model.size_based_model t_kind code_or_data ) ]
end

(* ----------------------------------------------------------------------- *)
(* Error handling *)

type phase = Workload_production | In_protocol | Global

type error_kind =
  | Global_error of {
      benchmark_name : string;
      workload : Tezos_base.TzPervasives.tztrace;
    }
  | Bad_data of {
      benchmark_name : string;
      micheline : Alpha_context.Script.expr;
      expected_type : Alpha_context.Script.expr;
      phase : phase;
    }
  | Bad_code of {
      benchmark_name : string;
      micheline : Alpha_context.Script.expr;
      expected_stack_type : Alpha_context.Script.expr list;
      phase : phase;
    }

let pp_phase fmtr (phase : phase) =
  match phase with
  | Workload_production ->
      Format.fprintf fmtr "workload production"
  | In_protocol ->
      Format.fprintf fmtr "in protocol"
  | Global ->
      Format.fprintf fmtr "global"

let report_michelson_errors fmtr errs =
  Tezos_client_alpha.Michelson_v1_error_reporter.report_errors
    ~details:true
    ~show_source:true
    fmtr
    errs

let make_printable node =
  Micheline_printer.printable Michelson_v1_primitives.string_of_prim node

let pp_error_kind fmtr (error_kind : error_kind) =
  match error_kind with
  | Global_error {benchmark_name; workload} ->
      Format.open_vbox 1 ;
      Format.fprintf fmtr "Global error:@," ;
      Format.fprintf fmtr "benchmark = %s@," benchmark_name ;
      Format.fprintf fmtr "workload:@," ;
      report_michelson_errors fmtr workload ;
      Format.close_box ()
  | Bad_data {benchmark_name; micheline; expected_type; phase} ->
      Format.open_vbox 1 ;
      Format.fprintf fmtr "Bad data:@," ;
      Format.fprintf fmtr "benchmark = %s@," benchmark_name ;
      Format.fprintf
        fmtr
        "expression = @[<v 1>%a@]@,"
        Micheline_printer.print_expr
        (make_printable micheline) ;
      Format.fprintf
        fmtr
        "expected type = @[<v 1>%a@]@,"
        Micheline_printer.print_expr
        (make_printable expected_type) ;
      Format.fprintf fmtr "phase = %a@," pp_phase phase ;
      Format.close_box ()
  | Bad_code {benchmark_name; micheline; expected_stack_type; phase} ->
      Format.open_vbox 1 ;
      Format.fprintf fmtr "Bad code:@," ;
      Format.fprintf fmtr "benchmark = %s@," benchmark_name ;
      Format.fprintf
        fmtr
        "expression = @[<v 1>%a@]@,"
        Micheline_printer.print_expr
        (make_printable micheline) ;
      Format.fprintf
        fmtr
        "expected stack = @[<v 1>%a@]@,"
        (Format.pp_print_list
           ~pp_sep:(fun fmtr () -> Format.fprintf fmtr "::")
           (fun fmtr node ->
             let printable = make_printable node in
             Format.fprintf fmtr "%a" Micheline_printer.print_expr printable))
        expected_stack_type ;
      Format.fprintf fmtr "phase = %a@," pp_phase phase ;
      Format.close_box ()

exception Translator_benchmark_error of error_kind

let () =
  Printexc.register_printer (function
      | Translator_benchmark_error err ->
          Some (Format.asprintf "%a" pp_error_kind err)
      | _ ->
          None)

let global_error benchmark_name workload =
  raise (Translator_benchmark_error (Global_error {benchmark_name; workload}))

let bad_data benchmark_name micheline expected_type phase =
  raise
    (Translator_benchmark_error
       (Bad_data {benchmark_name; micheline; expected_type; phase}))

let bad_code benchmark_name micheline expected_stack_type phase =
  raise
    (Translator_benchmark_error
       (Bad_code {benchmark_name; micheline; expected_stack_type; phase}))

(* ----------------------------------------------------------------------- *)
(* Typechecking data (Micheline data -> typed data) *)

module Typechecking_data : Benchmark.S = struct
  include Config
  include Default_boilerplate

  let models = make_models Translator_workload.Parsing Translator_workload.Data

  let name = "TYPECHECKING_DATA"

  let info = "Benchmarking typechecking of data"

  let typechecking_data_benchmark rng_state (node : Protocol.Script_repr.expr)
      (michelson_type : Script_repr.expr) =
    Lwt_main.run
      ( Execution_context.make ~rng_state
      >>=? fun (ctxt, _) ->
      let (ex_ty, ctxt) =
        Michelson_generation.michelson_type_to_ex_ty michelson_type ctxt
      in
      let workload =
        match
          Translator_workload.data_typechecker_workload
            ctxt
            Translator_workload.Parsing
            (Micheline.root node)
            ex_ty
        with
        | None ->
            bad_data name node michelson_type Workload_production
        | Some workload ->
            workload
      in
      match ex_ty with
      | Script_ir_translator.Ex_ty ty ->
          let closure () =
            match
              Lwt_main.run
                (Script_ir_translator.parse_data
                   ctxt
                   ~legacy:false
                   ~allow_forged:false
                   ty
                   (Micheline.root node))
            with
            | Error _ | (exception _) ->
                bad_data name node michelson_type In_protocol
            | Ok _ ->
                ()
          in
          return (Generator.Plain {workload; closure}) )
    |> function Ok closure -> closure | Error errs -> global_error name errs

  let make_bench rng_state cfg () =
    match
      Michelson_generation.make_data_sampler rng_state cfg.generator_config
    with
    | Data {term; typ} ->
        typechecking_data_benchmark rng_state term typ
    | _ ->
        assert false

  let create_benchmarks ~rng_state ~bench_num config =
    match config.michelson_terms_file with
    | Some file ->
        Format.eprintf "Loading terms from %s@." file ;
        let terms = Michelson_generation.load_file file in
        List.filter_map
          (function
            | Michelson_generation.Data {term; typ} ->
                Some (fun () -> typechecking_data_benchmark rng_state term typ)
            | _ ->
                None)
          terms
    | None ->
        Format.eprintf "No michelson_terms_file given, generating on-the-fly@." ;
        List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Typechecking_data)

module Unparsing_data : Benchmark.S = struct
  include Config
  include Default_boilerplate

  let models =
    make_models Translator_workload.Unparsing Translator_workload.Data

  let name = "UNPARSING_DATA"

  let info = "Benchmarking unparsing of data"

  let unparsing_data_benchmark rng_state (node : Protocol.Script_repr.expr)
      (michelson_type : Tezos_protocol_alpha.Protocol.Script_repr.expr) =
    Lwt_main.run
      ( Execution_context.make ~rng_state
      >>=? fun (ctxt, _) ->
      let (ex_ty, ctxt) =
        Michelson_generation.michelson_type_to_ex_ty michelson_type ctxt
      in
      let workload =
        match
          Translator_workload.data_typechecker_workload
            ctxt
            Translator_workload.Unparsing
            (Micheline.root node)
            ex_ty
        with
        | None ->
            bad_data name node michelson_type Workload_production
        | Some workload ->
            workload
      in
      match ex_ty with
      | Script_ir_translator.Ex_ty ty ->
          Script_ir_translator.parse_data
            ctxt
            ~legacy:false
            ~allow_forged:false
            ty
            (Micheline.root node)
          >|= Environment.wrap_tzresult
          >>=? fun (typed, ctxt) ->
          let closure () =
            match
              Lwt_main.run
                (Script_ir_translator.unparse_data
                   ctxt
                   Script_ir_translator.Optimized
                   ty
                   typed)
            with
            | Error _ | (exception _) ->
                bad_data name node michelson_type In_protocol
            | Ok _ ->
                ()
          in
          return (Generator.Plain {workload; closure}) )
    |> function Ok closure -> closure | Error errs -> global_error name errs

  let make_bench rng_state cfg () =
    match
      Michelson_generation.make_data_sampler rng_state cfg.generator_config
    with
    | Data {term; typ} ->
        unparsing_data_benchmark rng_state term typ
    | _ ->
        assert false

  let create_benchmarks ~rng_state ~bench_num config =
    match config.michelson_terms_file with
    | Some file ->
        Format.eprintf "Loading terms from %s@." file ;
        let terms = Michelson_generation.load_file file in
        List.filter_map
          (function
            | Michelson_generation.Data {term; typ} ->
                Some (fun () -> unparsing_data_benchmark rng_state term typ)
            | _ ->
                None)
          terms
    | None ->
        Format.eprintf "No michelson_terms_file given, generating on-the-fly@." ;
        List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Unparsing_data)

(* The new elaborator expects one more element at the bottom of the stack. *)
let cushion_stack_type type_list =
  type_list
  @ [ Michelson_generation.base_type_to_michelson_type
        Tezos_benchmark_type_inference_alpha.Type.unit ]

module Typechecking_code : Benchmark.S = struct
  include Config
  include Default_boilerplate

  let models = make_models Translator_workload.Parsing Translator_workload.Code

  let name = "TYPECHECKING_CODE"

  let info = "Benchmarking typechecking of code"

  let typechecking_code_benchmark rng_state (node : Protocol.Script_repr.expr)
      (stack : Script_repr.expr list) =
    Lwt_main.run
      ( Execution_context.make ~rng_state
      >>=? fun (ctxt, _) ->
      let (ex_stack_ty, ctxt) =
        Michelson_generation.michelson_type_list_to_ex_stack_ty stack ctxt
      in
      let workload =
        match
          Translator_workload.code_typechecker_workload
            ctxt
            Translator_workload.Parsing
            (Micheline.root node)
            ex_stack_ty
        with
        | None ->
            bad_code name node stack Workload_production
        | Some workload ->
            workload
      in
      let (Script_ir_translator.Ex_stack_ty bef) = ex_stack_ty in
      let closure () =
        let result =
          Lwt_main.run
            (Script_ir_translator.parse_instr
               Script_ir_translator.Lambda
               ctxt
               ~legacy:false
               (Micheline.root node)
               bef)
        in
        match Environment.wrap_tzresult result with
        | Error errs ->
            Format.eprintf "%a@." Error_monad.pp_print_error errs ;
            bad_code name node stack In_protocol
        | Ok _ ->
            ()
      in
      return (Generator.Plain {workload; closure}) )
    |> function Ok closure -> closure | Error errs -> global_error name errs

  let make_bench rng_state (cfg : Config.config) () =
    let open Michelson_generation in
    match make_code_sampler rng_state cfg.generator_config with
    | Code {term; bef} ->
        typechecking_code_benchmark rng_state term bef
    | Data _ ->
        assert false

  let create_benchmarks ~rng_state ~bench_num config =
    match config.michelson_terms_file with
    | Some file ->
        Format.eprintf "Loading terms from %s@." file ;
        let terms = Michelson_generation.load_file file in
        List.filter_map
          (function
            | Michelson_generation.Code {term; bef} ->
                Some (fun () -> typechecking_code_benchmark rng_state term bef)
            | _ ->
                None)
          terms
    | None ->
        Format.eprintf "No michelson_terms_file given, generating on-the-fly@." ;
        List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Typechecking_code)

module Unparsing_code : Benchmark.S = struct
  include Config
  include Default_boilerplate

  let models =
    make_models Translator_workload.Unparsing Translator_workload.Code

  let name = "UNPARSING_CODE"

  let info = "Benchmarking unparsing of code"

  let unparsing_code_benchmark rng_state (node : Protocol.Script_repr.expr)
      (stack : Script_repr.expr list) =
    Lwt_main.run
      ( Execution_context.make ~rng_state
      >>=? fun (ctxt, _) ->
      let (ex_stack_ty, ctxt) =
        Michelson_generation.michelson_type_list_to_ex_stack_ty stack ctxt
      in
      let workload =
        match
          Translator_workload.code_typechecker_workload
            ctxt
            Translator_workload.Unparsing
            (Micheline.root node)
            ex_stack_ty
        with
        | None ->
            bad_code name node stack Workload_production
        | Some workload ->
            workload
      in
      let (Script_ir_translator.Ex_stack_ty bef) = ex_stack_ty in
      (* We parse the code just to check it is well-typed. *)
      Script_ir_translator.parse_instr
        Script_ir_translator.Lambda
        ctxt
        ~legacy:false
        (Micheline.root node)
        bef
      >|= Environment.wrap_tzresult
      >>=? fun (_typed, ctxt) ->
      let closure () =
        let result =
          Lwt_main.run
            (Script_ir_translator.unparse_code
               ctxt
               Optimized
               (Micheline.root node))
        in
        match Environment.wrap_tzresult result with
        | Error errs ->
            Format.eprintf "%a@." Error_monad.pp_print_error errs ;
            bad_code name node stack In_protocol
        | Ok _ ->
            ()
      in
      return (Generator.Plain {workload; closure}) )
    |> function Ok closure -> closure | Error errs -> global_error name errs

  let make_bench rng_state (cfg : Config.config) () =
    let open Michelson_generation in
    match make_code_sampler rng_state cfg.generator_config with
    | Code {term; bef} ->
        unparsing_code_benchmark rng_state term bef
    | Data _ ->
        assert false

  let create_benchmarks ~rng_state ~bench_num config =
    match config.michelson_terms_file with
    | Some file ->
        Format.eprintf "Loading terms from %s@." file ;
        let terms = Michelson_generation.load_file file in
        List.filter_map
          (function
            | Michelson_generation.Code {term; bef} ->
                Some (fun () -> unparsing_code_benchmark rng_state term bef)
            | _ ->
                None)
          terms
    | None ->
        List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Unparsing_code)

module Micheline_shared = struct
  type config = {max_depth : int; max_width : int}

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {max_depth; max_width} -> (max_depth, max_width))
      (fun (max_depth, max_width) -> {max_depth; max_width})
      (obj2 (req "max_depth" int31) (req "max_width" int31))

  let default_config = {max_depth = 100; max_width = 1000}

  type workload = {micheline_nodes : int}

  let workload_encoding =
    let open Data_encoding in
    conv
      (fun {micheline_nodes} -> micheline_nodes)
      (fun micheline_nodes -> {micheline_nodes})
      (obj1 (req "micheline_nodes" int31))

  let tags = [Tags.translator]

  let workload_to_vector {micheline_nodes} =
    Sparse_vec.String.of_list [("nodes", float_of_int micheline_nodes)]
end

(* Eventually this should be merged in the protocol (and then lib-micheline) *)
let rec micheline_stats node total cumwidth avg k =
  match node with
  | Micheline.Int (_, _) ->
      k (total + 1) cumwidth avg
  | Micheline.String (_, _) ->
      k (total + 1) cumwidth avg
  | Micheline.Bytes (_, _) ->
      k (total + 1) cumwidth avg
  | Micheline.Prim (_, _, subterms, _) ->
      micheline_stats_list subterms 0 (total + 1) cumwidth avg k
  | Micheline.Seq (_, subterms) ->
      micheline_stats_list subterms 0 (total + 1) cumwidth avg k

and micheline_stats_list subterms length total cumwidth avg k =
  match subterms with
  | [] ->
      let avg' = avg + (((length lsl 10) - avg) / total) in
      k total (cumwidth + length) avg'
  | n :: nodes ->
      micheline_stats_list
        nodes
        (length + 1)
        total
        cumwidth
        avg
        (fun total cumwidth avg -> micheline_stats n total cumwidth avg k)

let micheline_stats node =
  micheline_stats node 0 0 0 (fun total cumwidth avg ->
      (total, cumwidth, avg lsr 10))

(* Eventually this should be merged in the protocol (and then lib-micheline) *)
let rec micheline_nodes node acc k =
  match node with
  | Micheline.Int (_, _) ->
      k (acc + 1)
  | Micheline.String (_, _) ->
      k (acc + 1)
  | Micheline.Bytes (_, _) ->
      k (acc + 1)
  | Micheline.Prim (_, _, subterms, _) ->
      micheline_nodes_list subterms (acc + 1) k
  | Micheline.Seq (_, subterms) ->
      micheline_nodes_list subterms (acc + 1) k

and micheline_nodes_list subterms acc k =
  match subterms with
  | [] ->
      k acc
  | n :: nodes ->
      micheline_nodes_list nodes acc (fun acc -> micheline_nodes n acc k)

let micheline_nodes node = micheline_nodes node 0 (fun x -> x)

let stub = Micheline.Int (0, Z.zero)

(* Micheline terms with a definite depth, all nodes at each depth are
   stubs except one. This allows to stress both the "width" recursion
   and the "depth" recursion of [strip_locations]. *)
let rec linear_micheline_generator rng_state max_width depth k =
  if depth = 0 then k stub
  else
    let width = 1 + Random.State.int rng_state max_width in
    linear_micheline_generator rng_state max_width (depth - 1) (fun res ->
        let l = List.repeat (width - 1) stub @ [res] in
        k (Micheline.Seq (0, l)))

let dummy_micheline_generator rng_state max_width depth =
  linear_micheline_generator rng_state depth max_width (fun x -> x)

module Micheline_nodes_benchmark : Benchmark.S = struct
  include Micheline_shared

  let name = "MICHELINE_NODES"

  let info =
    "Benchmarking the time it takes to compute the number of nodes of a \
     Micheline term"

  type workload = {micheline_nodes : int}

  let workload_encoding =
    let open Data_encoding in
    conv
      (fun {micheline_nodes} -> micheline_nodes)
      (fun micheline_nodes -> {micheline_nodes})
      (obj1 (req "micheline_nodes" int31))

  let workload_to_vector {micheline_nodes} =
    Sparse_vec.String.of_list [("nodes", float_of_int micheline_nodes)]

  let size_based_model =
    Model.make
      ~conv:(function {micheline_nodes} -> (micheline_nodes, ()))
      ~model:
        (Model.affine
           ~intercept:
             (Free_variable.of_string (Format.asprintf "%s_const" name))
           ~coeff:
             (Free_variable.of_string
                (Format.asprintf "%s_ns_per_node_coeff" name)))

  let () =
    Registration_helpers.register_for_codegen
      name
      (Model.For_codegen size_based_model)

  let models = [("size_translator_model", size_based_model)]

  let micheline_nodes_benchmark node =
    let node = Micheline.root node in
    let nodes = micheline_nodes node in
    let workload = {micheline_nodes = nodes} in
    let closure () = ignore (micheline_nodes node) in
    Generator.Plain {workload; closure}

  let make_bench rng_state cfg () =
    let depth = 1 + Random.State.int rng_state cfg.max_depth in
    let max_width = 1 + Random.State.int rng_state cfg.max_width in
    let term = dummy_micheline_generator rng_state max_width depth in
    micheline_nodes_benchmark (Micheline.strip_locations term)

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Micheline_nodes_benchmark)

module Micheline_strip_locations_benchmark : Benchmark.S = struct
  include Micheline_shared

  type workload = {micheline_nodes : int; cumwidth : int; avg_degree : int}

  let workload_encoding =
    let open Data_encoding in
    conv
      (fun {micheline_nodes; cumwidth; avg_degree} ->
        (micheline_nodes, cumwidth, avg_degree))
      (fun (micheline_nodes, cumwidth, avg_degree) ->
        {micheline_nodes; cumwidth; avg_degree})
      (obj3
         (req "micheline_nodes" int31)
         (req "cumwidth" int31)
         (req "avg_degree" int31))

  let workload_to_vector {micheline_nodes; cumwidth; avg_degree} =
    Sparse_vec.String.of_list
      [ ("nodes", float_of_int micheline_nodes);
        ("cumwidth", float_of_int cumwidth);
        ("avg_degree", float_of_int avg_degree) ]

  let name = "MICHELINE_STRIP_LOCATIONS"

  let info =
    "Benchmarking the time it takes to strip locations of a Micheline term"

  let size_based_model =
    Model.make
      ~conv:(function
        | {micheline_nodes; cumwidth = _; avg_degree = _} ->
            (micheline_nodes, ()))
      ~model:
        (Model.linear
           ~coeff:(Free_variable.of_string (Format.asprintf "%s_coeff" name)))

  let () =
    Registration_helpers.register_for_codegen
      name
      (Model.For_codegen size_based_model)

  let models = [("size_translator_model", size_based_model)]

  let strip_locations_benchmark node =
    let (micheline_nodes, cumwidth, avg_degree) = micheline_stats node in
    assert (micheline_nodes > 0) ;
    assert (avg_degree > 0) ;
    let workload = {micheline_nodes; cumwidth; avg_degree} in
    let closure () = ignore (Micheline.strip_locations node) in
    Generator.Plain {workload; closure}

  let make_bench rng_state cfg () =
    let depth = 1 + Random.State.int rng_state cfg.max_depth in
    let max_width = 1 + Random.State.int rng_state cfg.max_width in
    let term = dummy_micheline_generator rng_state max_width depth in
    strip_locations_benchmark term

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (make_bench rng_state config)
end

let () =
  Registration_helpers.register (module Micheline_strip_locations_benchmark)

let rec check_printable_ascii v i =
  if Compare.Int.(i < 0) then true
  else
    match v.[i] with
    | '\n' | '\x20' .. '\x7E' ->
        check_printable_ascii v (i - 1)
    | _ ->
        false

let check_printable_benchmark =
  let open Tezos_shell_benchmarks.Encoding_benchmarks_helpers in
  linear_shared
    ~name:"CHECK_PRINTABLE"
    ~generator:(fun rng_state ->
      let open Base_samplers in
      let string =
        readable_ascii_string rng_state ~size:{min = 1; max = 1024}
      in
      (string, {Shared_linear.bytes = String.length string}))
    ~make_bench:(fun generator () ->
      let (generated, workload) = generator () in
      let closure () =
        ignore (check_printable_ascii generated (String.length generated - 1))
      in
      Generator.Plain {workload; closure})

let () = Registration_helpers.register check_printable_benchmark

module Merge_types : Benchmark.S = struct
  type config = {max_depth : int}

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {max_depth} -> max_depth)
      (fun max_depth -> {max_depth})
      (obj1 (req "max_depth" int31))

  let default_config = {max_depth = 64}

  type workload = Merge_types_workload of {nodes : int; consumed : Size.t}

  let workload_encoding =
    let open Data_encoding in
    conv
      (function Merge_types_workload {nodes; consumed} -> (nodes, consumed))
      (fun (nodes, consumed) -> Merge_types_workload {nodes; consumed})
      (obj2 (req "nodes" int31) (req "consumed" int31))

  let workload_to_vector = function
    | Merge_types_workload {nodes; consumed} ->
        Sparse_vec.String.of_list
          [("nodes", float_of_int nodes); ("consumed", float_of_int consumed)]

  let name = "MERGE_TYPES"

  let info = "Benchmarking merging of types"

  let tags = [Tags.translator]

  let intercept_var = Free_variable.of_string (Format.asprintf "%s_const" name)

  let coeff_var = Free_variable.of_string (Format.asprintf "%s_coeff" name)

  let size_model =
    Model.make
      ~conv:(function Merge_types_workload {nodes; _} -> (nodes, ()))
      ~model:
        (Model.affine_split_const
           ~intercept1:Builtin_benchmarks.timer_variable
           ~intercept2:intercept_var
           ~coeff:coeff_var)

  let codegen_model =
    Model.make
      ~conv:(function Merge_types_workload {nodes; _} -> (nodes, ()))
      ~model:(Model.affine ~intercept:intercept_var ~coeff:coeff_var)

  let () =
    Registration_helpers.register_for_codegen
      name
      (Model.For_codegen codegen_model)

  let models =
    [("size_translator_model", size_model); ("codegen", codegen_model)]

  let merge_type_benchmark rng_state (ty : Script_ir_translator.ex_ty) =
    let open Error_monad in
    Lwt_main.run
      ( Execution_context.make ~rng_state
      >>=? fun (ctxt, _) ->
      let ctxt = Gas_helpers.set_limit ctxt in
      match ty with
      | Ex_ty ty ->
          let dummy_loc = 0 in
          let nodes = Script_ir_translator.type_size ty in
          Lwt.return (Script_ir_translator.ty_eq ctxt dummy_loc ty ty)
          >|= Environment.wrap_tzresult
          >>=? fun (_, ctxt') ->
          let consumed = Alpha_context.Gas.consumed ~since:ctxt ~until:ctxt' in
          let workload =
            Merge_types_workload
              {nodes; consumed = Z.to_int (Gas_helpers.fp_to_z consumed)}
          in
          let closure () = ignore (Script_ir_translator.ty_eq ctxt 0 ty ty) in
          return (Generator.Plain {workload; closure}) )
    |> function Ok closure -> closure | Error errs -> global_error name errs

  let make_bench sampler rng_state cfg () =
    let ty = sampler ~max_depth:cfg.max_depth in
    merge_type_benchmark rng_state ty

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat
      bench_num
      (make_bench
         (Michelson_generation.Samplers.Random_type.m_type rng_state)
         rng_state
         config)
end

let () = Registration_helpers.register (module Merge_types)

(* A dummy type generator, sampling linear terms of a given size. *)
let rec dummy_type_generator depth =
  let open Script_ir_translator in
  if depth = 0 then Ex_ty (Unit_t None)
  else
    match dummy_type_generator (depth - 1) with
    | Ex_ty something ->
        Ex_ty (List_t (something, None))

(* Generate combs; the size of a comb of depth d should be
   d * 2 + 1. *)
let rec dummy_comparable_type_generator depth =
  let open Script_ir_translator in
  let open Script_typed_ir in
  if depth = 0 then Ex_comparable_ty (Unit_key None)
  else
    match dummy_comparable_type_generator (depth - 1) with
    | Ex_comparable_ty r ->
        let l = Unit_key None in
        Ex_comparable_ty (Pair_key ((l, None), (r, None), None))

module Parse_type_shared = struct
  type config = {max_size : int}

  let default_config = {max_size = 1024}

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {max_size} -> max_size)
      (fun max_size -> {max_size})
      (obj1 (req "max_size" int31))

  type workload = Type_workload of {nodes : int; consumed : Size.t}

  let workload_encoding =
    let open Data_encoding in
    conv
      (function Type_workload {nodes; consumed} -> (nodes, consumed))
      (fun (nodes, consumed) -> Type_workload {nodes; consumed})
      (obj2 (req "nodes" int31) (req "consumed" int31))

  let workload_to_vector = function
    | Type_workload {nodes; consumed} ->
        Sparse_vec.String.of_list
          [("nodes", float_of_int nodes); ("consumed", float_of_int consumed)]

  let tags = [Tags.translator]
end

let parse_ty ctxt node =
  Script_ir_translator.parse_ty
    ctxt
    ~legacy:true
    ~allow_lazy_storage:true
    ~allow_operation:true
    ~allow_contract:true
    ~allow_ticket:true
    node

let unparse_ty ctxt ty = Script_ir_translator.unparse_ty ctxt ty

module Parse_type_benchmark : Benchmark.S = struct
  include Parse_type_shared

  let name = "PARSE_TYPE"

  let info = "Benchmarking parse_ty"

  let make_bench rng_state config () =
    let open Error_monad in
    Lwt_main.run (Execution_context.make ~rng_state)
    >>? (fun (ctxt, _) ->
          let ctxt = Gas_helpers.set_limit ctxt in
          let depth = Random.State.int rng_state config.max_size in
          let ty = dummy_type_generator depth in
          match ty with
          | Ex_ty ty ->
              Environment.wrap_tzresult @@ unparse_ty ctxt ty
              >>? fun (unparsed, _) ->
              Environment.wrap_tzresult @@ parse_ty ctxt unparsed
              >>? fun (_, ctxt') ->
              let consumed =
                Z.to_int
                  (Gas_helpers.fp_to_z
                     (Alpha_context.Gas.consumed ~since:ctxt ~until:ctxt'))
              in
              let workload = Type_workload {nodes = depth; consumed} in
              let closure () = ignore (parse_ty ctxt unparsed) in
              ok (Generator.Plain {workload; closure}))
    |> function Ok closure -> closure | Error errs -> global_error name errs

  let size_model =
    Model.make
      ~conv:(function Type_workload {nodes; consumed = _} -> (nodes, ()))
      ~model:
        (Model.affine
           ~intercept:
             (Free_variable.of_string (Format.asprintf "%s_const" name))
           ~coeff:(Free_variable.of_string (Format.asprintf "%s_coeff" name)))

  let models = [("size_translator_model", size_model)]

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Parse_type_benchmark)

module Unparse_type_benchmark : Benchmark.S = struct
  include Parse_type_shared

  let name = "UNPARSE_TYPE"

  let info = "Benchmarking unparse_ty"

  let make_bench rng_state config () =
    let open Error_monad in
    Lwt_main.run (Execution_context.make ~rng_state)
    >>? (fun (ctxt, _) ->
          let ctxt = Gas_helpers.set_limit ctxt in
          let depth = Random.State.int rng_state config.max_size in
          let ty = dummy_type_generator depth in
          match ty with
          | Ex_ty ty ->
              Environment.wrap_tzresult @@ unparse_ty ctxt ty
              >>? fun (_, ctxt') ->
              let consumed =
                Z.to_int
                  (Gas_helpers.fp_to_z
                     (Alpha_context.Gas.consumed ~since:ctxt ~until:ctxt'))
              in
              let workload = Type_workload {nodes = depth; consumed} in
              let closure () = ignore (unparse_ty ctxt ty) in
              ok (Generator.Plain {workload; closure}))
    |> function Ok closure -> closure | Error errs -> global_error name errs

  let size_model =
    Model.make
      ~conv:(function Type_workload {nodes; consumed = _} -> (nodes, ()))
      ~model:
        (Model.affine
           ~intercept:
             (Free_variable.of_string (Format.asprintf "%s_const" name))
           ~coeff:(Free_variable.of_string (Format.asprintf "%s_coeff" name)))

  let models = [("size_translator_model", size_model)]

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Unparse_type_benchmark)
