(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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
module Benchmark_base = Benchmark

module Benchmark = struct
  type group = Benchmark.group = Standalone | Group of string | Generic

  type purpose = Benchmark_base.purpose =
    | Other_purpose of string
    | Generate_code of string

  module type S = sig
    val name : Namespace.t

    val info : string

    val module_filename : string

    val purpose : Benchmark.purpose

    val group : group

    val tags : string list

    type config

    val default_config : config

    val config_encoding : config Data_encoding.t

    type workload

    val workload_encoding : workload Data_encoding.t

    val workload_to_vector : workload -> Sparse_vec.String.t

    val model : name:Namespace.t -> workload Model.t

    val create_benchmark :
      rng_state:Random.State.t -> config -> workload Generator.benchmark
  end

  type t = (module S)
end

module Registration = struct
  let register ((module Bench) : Benchmark.t) =
    let module B : Benchmark_base.S = struct
      include Bench

      let tags = "shell" :: Bench.tags

      let models =
        [
          ( (match Bench.group with
            | Generic -> "*"
            | Group g -> g
            | Standalone -> Namespace.(cons Bench.name "model" |> to_string)),
            Bench.model ~name );
        ]

      let create_benchmarks ~rng_state ~bench_num config =
        List.repeat bench_num (fun () ->
            Bench.create_benchmark ~rng_state config)
    end in
    Registration.register (module B)

  let register_base (module B : Benchmark_base.S) =
    let module Bench : Benchmark_base.S = struct
      include B

      let tags = "shell" :: B.tags
    end in
    Registration.register (module Bench)
end
