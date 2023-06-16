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

let ns = Namespace.make Registration_helpers.ns "script_typed_ir_size"

let fv s = Free_variable.of_namespace (ns s)

(** {2 [Script_typed_ir_size]-related benchmarks} *)

(** Benchmarking {!Script_typed_ir_size.value_size}. *)

let model_name = "ir_size_model"

let strict = Script_ir_translator_config.make ~legacy:false ()

module Size_benchmarks_shared_config = struct
  include Translator_benchmarks.Config

  type workload = {size : int}

  let workload_encoding : workload Data_encoding.t =
    let open Data_encoding in
    def "size_encoding"
    @@ conv (fun {size} -> size) (fun size -> {size}) (obj1 (req "size" int31))

  let workload_to_vector {size} =
    Sparse_vec.String.of_list [("size", float_of_int size)]

  let tags = [Tags.translator]

  let size_based_model name =
    let intercept_variable = fv (Format.asprintf "%s_const" name) in
    let coeff_variable = fv (Format.asprintf "%s_size_coeff" name) in
    Model.make
      ~conv:(function {size} -> (size, ()))
      ~model:
        (Model.affine
           ~name:(ns name)
           ~intercept:intercept_variable
           ~coeff:coeff_variable)
end

module Value_size_benchmark : sig
  include Tezos_benchmark.Benchmark.S

  val size_based_model : string -> workload Model.t
end = struct
  include Size_benchmarks_shared_config

  let name = ns "VALUE_SIZE"

  let models = [(model_name, size_based_model (Namespace.basename name))]

  let info = "Benchmarking Script_typed_ir_size.value_size"

  let module_filename = __FILE__

  let purpose = Benchmark.Other_purpose "No longer used to generate code"

  let value_size_benchmark rng_state (node : Protocol.Script_repr.expr)
      (michelson_type : Script_repr.expr) =
    (* FIXME: cleanup and factorize this code between translator benches and these ones. *)
    let open Translator_benchmarks in
    Lwt_main.run
      ( Execution_context.make ~rng_state >>=? fun (ctxt, _) ->
        let ex_ty = Type_helpers.michelson_type_to_ex_ty michelson_type ctxt in
        match ex_ty with
        | Script_typed_ir.Ex_ty ty -> (
            match
              Lwt_main.run
                (Script_ir_translator.parse_data
                   ctxt
                   ~elab_conf:strict
                   ~allow_forged:false
                   ty
                   (Micheline.root node))
            with
            | Error _ | (exception _) ->
                bad_data name node michelson_type In_protocol
            | Ok (value, _) ->
                let open Script_typed_ir_size in
                let open Cache_memory_helpers in
                let size = Nodes.(to_int (fst (value_size ty value))) in
                let workload = {size} in
                let closure () = ignore (value_size ty value) in
                return (Generator.Plain {workload; closure})) )
    |> function
    | Ok closure -> closure
    | Error errs -> global_error name errs

  let make_bench rng_state cfg () =
    let Michelson_mcmc_samplers.{term; typ} =
      Michelson_generation.make_data_sampler rng_state cfg.generator_config
    in
    value_size_benchmark rng_state term typ

  let create_benchmarks ~rng_state ~bench_num config =
    match config.michelson_terms_file with
    | Some file ->
        Format.eprintf "Loading terms from %s@." file ;
        let terms = Michelson_mcmc_samplers.load ~filename:file in
        List.filter_map
          (function
            | Michelson_mcmc_samplers.Data {term; typ} ->
                Some (fun () -> value_size_benchmark rng_state term typ)
            | _ -> None)
          terms
    | None ->
        Format.eprintf "No michelson_terms_file given, generating on-the-fly@." ;
        List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Value_size_benchmark)

(** Benchmarking {!Script_typed_ir_size.ty_size}. *)

module Type_size_benchmark : Tezos_benchmark.Benchmark.S = struct
  include Size_benchmarks_shared_config

  type config = unit

  let config_encoding = Data_encoding.unit

  let default_config = ()

  let name = ns "TYPE_SIZE"

  let info =
    "Benchmarking the time it takes to compute Script_typed_ir_size.ty_size"

  let module_filename = __FILE__

  let purpose = Benchmark.Other_purpose "No longer used to generate code"

  let models = [(model_name, size_based_model (Namespace.basename name))]

  let type_size_benchmark (Script_typed_ir.Ex_ty ty) =
    let open Script_typed_ir_size.Internal_for_tests in
    let open Cache_memory_helpers in
    let size = Nodes.(to_int (fst (ty_size ty))) in
    let workload = {size} in
    let closure () = ignore (ty_size ty) in
    Generator.Plain {workload; closure}

  let make_bench rng_state _cfg () =
    (* The [size] here is a parameter to the random sampler and does not
       match the [size] returned by [type_size]. *)
    let size =
      Base_samplers.sample_in_interval ~range:{min = 1; max = 1000} rng_state
    in
    let ex_ty =
      Michelson_generation.Samplers.Random_type.m_type ~size rng_state
    in
    type_size_benchmark ex_ty

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Type_size_benchmark)

(** Benchmarking {!Script_typed_ir_size.kinstr_size}. *)

module Kinstr_size_benchmark : sig
  include Tezos_benchmark.Benchmark.S

  val size_based_model : string -> workload Model.t
end = struct
  include Size_benchmarks_shared_config

  let name = ns "KINSTR_SIZE"

  let models = [(model_name, size_based_model (Namespace.basename name))]

  let info = "Benchmarking Script_typed_ir_size.kinstr_size"

  let module_filename = __FILE__

  let purpose = Benchmark.Other_purpose "No longer used to generate code"

  let kinstr_size_benchmark rng_state (expr : Protocol.Script_repr.expr)
      (stack : Script_repr.expr list) =
    (* FIXME: cleanup and factorize this code between translator benches and these ones. *)
    let open Translator_benchmarks in
    Lwt_main.run
      ( Execution_context.make ~rng_state >>=? fun (ctxt, _) ->
        let ex_stack_ty =
          Type_helpers.michelson_type_list_to_ex_stack_ty stack ctxt
        in
        let (Script_ir_translator.Ex_stack_ty bef) = ex_stack_ty in
        let node = Micheline.root expr in
        match
          Lwt_main.run
            (Script_ir_translator.parse_instr
               Script_tc_context.data
               ctxt
               ~elab_conf:strict
               node
               bef)
        with
        | Error _ | (exception _) -> bad_code name expr stack In_protocol
        | Ok (Failed {descr}, _) ->
            let kdescr = Script_ir_translator.close_descr (descr Bot_t) in
            let kinstr = kdescr.kinstr in
            let open Script_typed_ir_size.Internal_for_tests in
            let workload =
              let open Cache_memory_helpers in
              {size = Nodes.to_int @@ fst @@ kinstr_size kinstr}
            in
            let closure () = ignore (kinstr_size kinstr) in
            return (Generator.Plain {workload; closure})
        | Ok (Typed descr, _) ->
            let kdescr = Script_ir_translator.close_descr descr in
            let kinstr = kdescr.kinstr in
            let open Script_typed_ir_size.Internal_for_tests in
            let workload =
              let open Cache_memory_helpers in
              {size = Nodes.to_int @@ fst @@ kinstr_size kinstr}
            in
            let closure () = ignore (kinstr_size kinstr) in
            return (Generator.Plain {workload; closure}) )
    |> function
    | Ok closure -> closure
    | Error errs -> global_error name errs

  let make_bench rng_state cfg () =
    let Michelson_mcmc_samplers.{term; bef; aft = _} =
      Michelson_generation.make_code_sampler rng_state cfg.generator_config
    in
    kinstr_size_benchmark rng_state term bef

  let create_benchmarks ~rng_state ~bench_num config =
    match config.michelson_terms_file with
    | Some file ->
        Format.eprintf "Loading terms from %s@." file ;
        let terms = Michelson_mcmc_samplers.load ~filename:file in
        List.filter_map
          (function
            | Michelson_mcmc_samplers.Code {term; bef; aft = _} ->
                Some (fun () -> kinstr_size_benchmark rng_state term bef)
            | _ -> None)
          terms
    | None ->
        Format.eprintf "No michelson_terms_file given, generating on-the-fly@." ;
        List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Kinstr_size_benchmark)

module Node_size_benchmark : Benchmark.S = struct
  include Script_repr_benchmarks.Script_repr_shared_config

  let name = ns "NODE_SIZE"

  let info =
    "Benchmarking the time it takes to compute Script_typed_ir_size.node_size"

  let module_filename = __FILE__

  let purpose = Benchmark.Other_purpose "No longer used to generate code"

  let size_based_model =
    Model.make
      ~conv:(function {micheline_nodes} -> (micheline_nodes, ()))
      ~model:
        (Model.affine
           ~name
           ~intercept:
             (fv (Format.asprintf "%s_const" (Namespace.basename name)))
           ~coeff:
             (fv
                (Format.asprintf
                   "%s_ns_per_node_coeff"
                   (Namespace.basename name))))

  let () =
    Registration_helpers.register_for_codegen
      (Namespace.basename name)
      size_based_model

  let models = [(model_name, size_based_model)]

  let micheline_nodes_benchmark node =
    let open Cache_memory_helpers in
    let nodes = Nodes.to_int @@ fst @@ node_size node in
    let workload = {micheline_nodes = nodes} in
    let closure () = ignore (Script_typed_ir_size.node_size node) in
    Generator.Plain {workload; closure}

  let make_bench rng_state _cfg () =
    let term = Script_repr_benchmarks.Sampler.sample rng_state in
    micheline_nodes_benchmark term

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Node_size_benchmark)
