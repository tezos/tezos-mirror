(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Benchmark_base = Benchmark

module Benchmark = struct
  (** The module type of benchmarks, a simplification of {!Benchmark.S} used by
      [registration_simple] below. *)
  module type S = sig
    (** Name of the benchmark *)
    val name : Namespace.t

    (** Description of the benchmark *)
    val info : string

    (** filename of the benchmark module *)
    val module_location : string

    (** Tags of the benchmark *)
    val tags : string list

    (** Configuration of the benchmark (eg sampling parameters, paths, etc) *)
    type config

    (** Default configuration of the benchmark *)
    val default_config : config

    (** Configuration encoding *)
    val config_encoding : config Data_encoding.t

    (** Benchmark workload *)
    type workload

    (** Workload encoding *)
    val workload_encoding : workload Data_encoding.t

    (** Optional conversion to vector, for report generation purposes *)
    val workload_to_vector : workload -> Sparse_vec.String.t

    (** Cost model *)
    val model : workload Model.t

    (** Generated code file location, automatically prefix by
        "src/proto_alpha/lib_protocol/"
        and suffixed by
        "_costs_generated.ml" *)
    val generated_code_destination : string option

    (** Creates a  benchmark, ready to be run.
        The benchmarks are thunked to prevent evaluating the workload until
        needed. *)
    val create_benchmark :
      rng_state:Random.State.t -> config -> workload Generator.benchmark
  end

  type t = (module S)
end

(** Registers a benchmark with a model, model names are uniformely generated *)
let register_simple ((module Bench) : Benchmark.t) =
  let module B : Benchmark_base.S = struct
    include Bench

    let generated_code_destination =
      Option.map
        (fun destination ->
          Filename.concat
            "src/proto_alpha/lib_protocol"
            (destination ^ "_costs_generated.ml"))
        Bench.generated_code_destination

    (* The value will be used later in lib_benchmark/registration.ml for codegen
       file destination. *)
    let () = ignore generated_code_destination

    let models = [(Namespace.(cons name "model" |> to_string), Bench.model)]

    let create_benchmarks ~rng_state ~bench_num config =
      List.repeat bench_num (fun () -> Bench.create_benchmark ~rng_state config)
  end in
  Registration_helpers.register (module B)
