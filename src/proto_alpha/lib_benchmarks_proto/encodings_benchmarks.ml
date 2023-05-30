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

module Encodings =
Tezos_shell_benchmarks.Encoding_benchmarks_helpers.Make (struct
  let file = __FILE__

  let purpose =
    Benchmark.Generate_code
      "src/proto_alpha/lib_protocol/michelson_v1_gas_costs_generated.ml"
end)

module Size = Gas_input_size

let ns = Namespace.make Registration_helpers.ns "encoding"

let fv s = Free_variable.of_namespace (ns s)

module Micheline_common = struct
  let make_printable node =
    Micheline_printer.printable
      Michelson_v1_primitives.string_of_prim
      (Micheline.strip_locations node)

  type phase = Trace_production | In_protocol | Global

  type error =
    | Bad_micheline of {
        benchmark_name : Namespace.t;
        micheline : Alpha_context.Script.node;
        phase : phase;
      }

  exception Micheline_benchmark of error

  let pp_phase fmtr (phase : phase) =
    match phase with
    | Trace_production -> Format.fprintf fmtr "trace production"
    | In_protocol -> Format.fprintf fmtr "in protocol"
    | Global -> Format.fprintf fmtr "global"

  let pp_error fmtr = function
    | Bad_micheline {benchmark_name; micheline; phase} ->
        Format.open_vbox 1 ;
        Format.fprintf fmtr "Bad micheline:@," ;
        Format.fprintf fmtr "benchmark = %a@," Namespace.pp benchmark_name ;
        Format.fprintf
          fmtr
          "expression = @[<v 1>%a@]@,"
          Micheline_printer.print_expr
          (make_printable micheline) ;
        Format.fprintf fmtr "phase = %a@," pp_phase phase ;
        Format.close_box ()

  let bad_micheline benchmark_name micheline phase =
    raise
      (Micheline_benchmark (Bad_micheline {benchmark_name; micheline; phase}))

  type workload = {size : Size.micheline_size; bytes : int}

  let workload_encoding =
    let open Data_encoding in
    def "encoding_micheline_trace"
    @@ conv
         (fun {size; bytes} -> (size, bytes))
         (fun (size, bytes) -> {size; bytes})
         (obj2
            (req "micheline_size" Size.micheline_size_encoding)
            (req "micheline_bytes" Size.encoding))

  let workload_to_vector (workload : workload) =
    let keys =
      [
        ( "encoding_micheline_traversal",
          float_of_int (Size.to_int workload.size.traversal) );
        ( "encoding_micheline_int_bytes",
          float_of_int (Size.to_int workload.size.int_bytes) );
        ( "encoding_micheline_string_bytes",
          float_of_int (Size.to_int workload.size.string_bytes) );
        ("encoding_micheline_bytes", float_of_int (Size.to_int workload.bytes));
      ]
    in
    Sparse_vec.String.of_list keys

  let tags = [Tags.encoding]

  let model_size name =
    Model.make
      ~conv:(fun {size = {Size.traversal; int_bytes; string_bytes}; _} ->
        (traversal, (int_bytes, (string_bytes, ()))))
      ~model:
        (Model.trilinear
           ~name:(ns name)
           ~coeff1:(fv (Format.asprintf "%s_micheline_traversal" name))
           ~coeff2:(fv (Format.asprintf "%s_micheline_int_bytes" name))
           ~coeff3:(fv (Format.asprintf "%s_micheline_string_bytes" name)))

  let model_bytes name =
    Model.make
      ~conv:(fun {bytes; _} -> (bytes, ()))
      ~model:
        (Model.linear
           ~name:(ns (name ^ "_bytes"))
           ~coeff:(fv (Format.asprintf "%s_micheline_bytes" name)))

  let models name =
    [("micheline", model_size name); ("micheline_bytes", model_bytes name)]
end

module Encoding_micheline : Benchmark.S = struct
  include Translator_benchmarks.Config
  include Micheline_common

  let name = ns "ENCODING_MICHELINE"

  let info = "Benchmarking strip_location + encoding of Micheline to bytes"

  let module_filename = __FILE__

  let purpose =
    Benchmark.Generate_code
      "src/proto_alpha/lib_protocol/script_repr_costs_generated.ml"

  let micheline_serialization_trace (micheline_node : Alpha_context.Script.node)
      =
    match
      Data_encoding.Binary.to_string
        Protocol.Script_repr.expr_encoding
        (Micheline.strip_locations micheline_node)
    with
    | Error err ->
        Format.eprintf
          "micheline_serialization_trace: %a@."
          Data_encoding.Binary.pp_write_error
          err ;
        None
    | Ok str ->
        let micheline_size = Size.of_micheline micheline_node in
        Some {size = micheline_size; bytes = Size.string str}

  let encoding_micheline_benchmark (node : Protocol.Script_repr.expr) =
    let node = Micheline.root node in
    let workload =
      match micheline_serialization_trace node with
      | None -> Micheline_common.bad_micheline name node Trace_production
      | Some trace -> trace
    in
    let closure () =
      try
        ignore
          (Data_encoding.Binary.to_string_exn
             Protocol.Script_repr.expr_encoding
             (Micheline.strip_locations node))
      with _ -> Micheline_common.bad_micheline name node In_protocol
    in
    Generator.Plain {workload; closure}

  let make_bench rng_state cfg () =
    let Michelson_mcmc_samplers.{term; typ = _} =
      Michelson_generation.make_data_sampler rng_state cfg.generator_config
    in
    encoding_micheline_benchmark term

  let create_benchmarks ~rng_state ~bench_num config =
    match config.michelson_terms_file with
    | Some file ->
        Format.eprintf "Loading terms from %s@." file ;
        let terms = Michelson_mcmc_samplers.load ~filename:file in
        List.map
          (function
            | Michelson_mcmc_samplers.Data {term; typ = _}
            | Michelson_mcmc_samplers.Code {term; bef = _; aft = _} ->
                fun () -> encoding_micheline_benchmark term)
          terms
    | None -> List.repeat bench_num (make_bench rng_state config)

  let models = models (Namespace.basename name)
end

let () = Registration_helpers.register (module Encoding_micheline)

module Decoding_micheline : Benchmark.S = struct
  include Translator_benchmarks.Config
  include Micheline_common

  let name = ns "DECODING_MICHELINE"

  let info = "Decoding of bytes to Micheline"

  let module_filename = __FILE__

  let purpose =
    Benchmark.Generate_code
      "src/proto_alpha/lib_protocol/script_repr_costs_generated.ml"

  let micheline_deserialization_trace (micheline_str : string) =
    match
      Data_encoding.Binary.of_string
        Protocol.Script_repr.expr_encoding
        micheline_str
    with
    | Error err ->
        Format.eprintf
          "micheline_deserialization_trace: %a@."
          Data_encoding.Binary.pp_read_error
          err ;
        None
    | Ok micheline_node ->
        let micheline_size =
          Size.of_micheline (Micheline.root micheline_node)
        in
        Some {size = micheline_size; bytes = Size.string micheline_str}

  let decoding_micheline_benchmark (node : Protocol.Script_repr.expr) =
    let encoded =
      Data_encoding.Binary.to_string_exn Protocol.Script_repr.expr_encoding node
    in
    let node = Micheline.root node in
    let workload =
      match micheline_deserialization_trace encoded with
      | None -> bad_micheline name node Trace_production
      | Some trace -> trace
    in
    let closure () =
      try
        ignore
          (Data_encoding.Binary.of_string_exn
             Protocol.Script_repr.expr_encoding
             encoded)
      with _ -> bad_micheline name node In_protocol
    in
    Generator.Plain {workload; closure}

  let make_bench rng_state cfg () =
    let Michelson_mcmc_samplers.{term; typ = _} =
      Michelson_generation.make_data_sampler rng_state cfg.generator_config
    in
    decoding_micheline_benchmark term

  let create_benchmarks ~rng_state ~bench_num config =
    match config.michelson_terms_file with
    | Some file ->
        Format.eprintf "Loading terms from %s@." file ;
        let terms = Michelson_mcmc_samplers.load ~filename:file in
        List.map
          (function
            | Michelson_mcmc_samplers.Data {term; typ = _}
            | Michelson_mcmc_samplers.Code {term; bef = _; aft = _} ->
                fun () -> decoding_micheline_benchmark term)
          terms
    | None -> List.repeat bench_num (make_bench rng_state config)

  let models = models (Namespace.basename name)
end

let () = Registration_helpers.register (module Decoding_micheline)

module Timestamp = struct
  open Encodings

  let () =
    Registration_helpers.register
    @@ fixed_size_shared
         ~name:"TIMESTAMP_READABLE_ENCODING"
         ~generator:(fun rng_state ->
           let seconds_in_year = 30_000_000 in
           let offset = Random.State.int rng_state seconds_in_year in
           Script_timestamp.of_zint (Z.of_int (1597764116 + offset)))
         ~make_bench:(fun generator () ->
           let tstamp_string = generator () in
           let closure () =
             ignore (Script_timestamp.to_notation tstamp_string)
           in
           Generator.Plain {workload = (); closure})
         ()

  let () =
    let b, b_intercept =
      nsqrtn_shared_with_intercept
        ~name:"TIMESTAMP_READABLE_DECODING"
        ~generator:(fun rng_state ->
          let offset =
            Base_samplers.nat ~size:{min = 1; max = 100_000} rng_state
          in
          let tstamp =
            Script_timestamp.of_zint Z.(of_int 1597764116 + offset)
          in
          Script_timestamp.to_string tstamp)
        ~make_bench:(fun generator () ->
          let tstamp_string = generator () in
          let bytes = String.length tstamp_string in
          let closure () = ignore (Script_timestamp.of_string tstamp_string) in
          Generator.Plain {workload = {bytes}; closure})
        ~generator_intercept:(fun rng_state ->
          let seconds_in_year = 30_000_000 in
          let offset = Random.State.int rng_state seconds_in_year in
          let tstamp =
            Script_timestamp.of_zint (Z.of_int (1597764116 + offset))
          in
          Script_timestamp.to_string tstamp)
        ~make_bench_intercept:(fun generator () ->
          let tstamp_string = generator () in
          let closure () = ignore (Script_timestamp.of_string tstamp_string) in
          Generator.Plain {workload = {bytes = 0}; closure})
    in
    Registration_helpers.register b ;
    Registration_helpers.register b_intercept
end

(* when benchmarking, compile bls12-381 without ADX, see
   https://gitlab.com/dannywillems/ocaml-bls12-381/-/blob/71d0b4d467fbfaa6452d702fcc408d7a70916a80/README.md#install
*)
module BLS = struct
  open Encodings

  let check () =
    if not Bls12_381.built_with_blst_portable then (
      Format.eprintf
        "BLS must be built without ADX to run the BLS benchmarks. Try \
         compiling again after setting the environment variable BLST_PORTABLE. \
         Aborting.@." ;
      Stdlib.failwith "bls_not_built_with_blst_portable")

  let () =
    Registration_helpers.register
    @@ make_encode_fixed_size_to_bytes
         ~check
         ~name:"ENCODING_BLS_FR"
         ~to_bytes:Bls12_381.Fr.to_bytes
         ~generator:(fun rng_state -> Bls12_381.Fr.random ~state:rng_state ())
         ()

  let () =
    Registration_helpers.register
    @@ make_encode_fixed_size_to_bytes
         ~check
         ~name:"ENCODING_BLS_G1"
         ~to_bytes:Bls12_381.G1.to_bytes
         ~generator:(fun rng_state -> Bls12_381.G1.random ~state:rng_state ())
         ()

  let () =
    Registration_helpers.register
    @@ make_encode_fixed_size_to_bytes
         ~check
         ~name:"ENCODING_BLS_G2"
         ~to_bytes:Bls12_381.G2.to_bytes
         ~generator:(fun rng_state -> Bls12_381.G2.random ~state:rng_state ())
         ()

  let () =
    Registration_helpers.register
    @@ make_decode_fixed_size_from_bytes
         ~check
         ~name:"DECODING_BLS_FR"
         ~to_bytes:Bls12_381.Fr.to_bytes
         ~from_bytes:Bls12_381.Fr.of_bytes_exn
         ~generator:(fun rng_state -> Bls12_381.Fr.random ~state:rng_state ())
         ()

  let () =
    Registration_helpers.register
    @@ make_decode_fixed_size_from_bytes
         ~check
         ~name:"DECODING_BLS_G1"
         ~to_bytes:Bls12_381.G1.to_bytes
         ~from_bytes:Bls12_381.G1.of_bytes_exn
         ~generator:(fun rng_state -> Bls12_381.G1.random ~state:rng_state ())
         ()

  let () =
    Registration_helpers.register
    @@ make_decode_fixed_size_from_bytes
         ~check
         ~name:"DECODING_BLS_G2"
         ~to_bytes:Bls12_381.G2.to_bytes
         ~from_bytes:Bls12_381.G2.of_bytes_exn
         ~generator:(fun rng_state -> Bls12_381.G2.random ~state:rng_state ())
         ()

  let () =
    Registration_helpers.register
    @@ fixed_size_shared
         ~check
         ~name:"BLS_FR_FROM_Z"
         ~generator:(fun rng_state -> Bls12_381.Fr.random ~state:rng_state ())
         ~make_bench:(fun generator () ->
           let generated = generator () in
           let z = Bls12_381.Fr.to_z generated in
           let closure () = ignore (Bls12_381.Fr.of_z z) in
           Generator.Plain {workload = (); closure})
         ()

  let () =
    Registration_helpers.register
    @@ fixed_size_shared
         ~check
         ~name:"BLS_FR_TO_Z"
         ~generator:(fun rng_state -> Bls12_381.Fr.random ~state:rng_state ())
         ~make_bench:(fun generator () ->
           let generated = generator () in
           let closure () = ignore (Bls12_381.Fr.to_z generated) in
           Generator.Plain {workload = (); closure})
         ()
end

module Timelock = struct
  open Encodings

  let generator rng_state =
    let log_time =
      Base_samplers.sample_in_interval ~range:{min = 0; max = 29} rng_state
    in
    let time = Int.shift_left 1 log_time in
    let plaintext_size =
      Base_samplers.sample_in_interval ~range:{min = 1; max = 10000} rng_state
    in
    let chest, chest_key =
      Tezos_crypto.Timelock.chest_sampler ~plaintext_size ~time ~rng_state
    in
    ((chest, chest_key), plaintext_size)

  let () =
    Registration_helpers.register
    @@ make_encode_variable_size_to_string
         ~name:"ENCODING_Chest"
         ~to_string:
           (Data_encoding.Binary.to_string_exn
              Tezos_crypto.Timelock.chest_encoding)
         ~generator:(fun rng_state ->
           let (chest, _), plaintext_size = generator rng_state in
           (chest, {bytes = plaintext_size}))
         ()

  let () =
    Registration_helpers.register
    @@ make_encode_fixed_size_to_string
         ~name:"ENCODING_Chest_key"
         ~to_string:
           (Data_encoding.Binary.to_string_exn
              Tezos_crypto.Timelock.chest_key_encoding)
         ~generator:(fun rng_state ->
           let (_, chest_key), _w = generator rng_state in
           chest_key)
         ()

  let () =
    Registration_helpers.register
    @@ make_decode_variable_size_from_bytes
         ~name:"DECODING_Chest"
         ~to_bytes:
           (Data_encoding.Binary.to_bytes_exn
              Tezos_crypto.Timelock.chest_encoding)
         ~from_bytes:
           (Data_encoding.Binary.of_bytes_exn
              Tezos_crypto.Timelock.chest_encoding)
         ~generator:(fun rng_state ->
           let (chest, _), _ = generator rng_state in
           let b =
             Data_encoding.Binary.to_bytes_exn
               Tezos_crypto.Timelock.chest_encoding
               chest
           in
           (chest, {bytes = Bytes.length b}))
         ()

  let () =
    Registration_helpers.register
    @@ make_decode_fixed_size_from_bytes
         ~name:"DECODING_Chest_key"
         ~to_bytes:
           (Data_encoding.Binary.to_bytes_exn
              Tezos_crypto.Timelock.chest_key_encoding)
         ~from_bytes:
           (Data_encoding.Binary.of_bytes_exn
              Tezos_crypto.Timelock.chest_key_encoding)
         ~generator:(fun rng_state ->
           let (_, chest_key), _w = generator rng_state in
           chest_key)
         ()
end
