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
  module type S = sig
    val name : Namespace.t

    val info : string

    val module_filename : string

    val tags : string list

    type config

    val default_config : config

    val config_encoding : config Data_encoding.t

    type workload

    val workload_encoding : workload Data_encoding.t

    val workload_to_vector : workload -> Sparse_vec.String.t

    val model : name:Namespace.t -> workload Model.t

    val generated_code_destination : string option

    val create_benchmark :
      rng_state:Random.State.t -> config -> workload Generator.benchmark
  end

  type t = (module S)
end

module Registration = struct
  let ns = Namespace.root

  let register ((module Bench) : Benchmark.t) =
    let module B : Benchmark_base.S = struct
      include Bench

      let purpose =
        match generated_code_destination with
        | Some x -> Benchmark_base.Generate_code x
        | None -> Other_purpose "unused"

      let models =
        [(Namespace.(cons name "model" |> to_string), Bench.model ~name)]

      let create_benchmarks ~rng_state ~bench_num config =
        List.repeat bench_num (fun () ->
            Bench.create_benchmark ~rng_state config)
    end in
    Registration_helpers.register (module B : Benchmark_base.S)

  let register_simple_with_num ((module Bench) : Benchmark_base.simple_with_num)
      =
    let module B : Benchmark_base.Simple_with_num = struct
      include Bench

      let tags = Tags.common :: tags
    end in
    Registration.register_simple_with_num
      (module B : Benchmark_base.Simple_with_num)

  let register_as_simple_with_num (module B : Benchmark_base.S) =
    let modules =
      List.map
        (fun (model_name, model) : (module Benchmark_base.Simple_with_num) ->
          (module struct
            include B

            let name = Namespace.cons name model_name

            let group = Benchmark_base.Group model_name

            let model = model
          end))
        B.models
    in
    List.iter (fun x -> register_simple_with_num x) modules
end

module Model = struct
  include Model

  let make ~name ~conv model = make ~conv (model name)

  let affine ?intercept ?coeff name =
    let ns s = Free_variable.of_namespace (Namespace.cons name s) in
    let intercept = Option.value ~default:(ns "intercept") intercept in
    let coeff = Option.value ~default:(ns "coeff") coeff in
    affine ~name ~intercept ~coeff

  let logn ?coeff name =
    let ns s = Free_variable.of_namespace (Namespace.cons name s) in
    let coeff = Option.value ~default:(ns "coeff") coeff in
    logn ~name ~coeff
end
