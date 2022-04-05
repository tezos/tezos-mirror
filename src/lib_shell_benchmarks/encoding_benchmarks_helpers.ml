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

(* Helpers for encodings of fixed-size values (hence constant-time) *)
module Shared_constant_time = struct
  type config = unit

  let config_encoding = Data_encoding.unit

  let default_config = ()

  type workload = unit

  let workload_encoding = Data_encoding.unit

  let workload_to_vector () = Sparse_vec.String.of_list [("encoding", 1.)]
end

module Shared_linear = struct
  type config = unit

  let config_encoding = Data_encoding.unit

  let default_config = ()

  type workload = {bytes : int}

  let workload_encoding =
    let open Data_encoding in
    conv (fun {bytes} -> bytes) (fun bytes -> {bytes}) int31

  let workload_to_vector {bytes} =
    Sparse_vec.String.of_list [("bytes", float_of_int bytes)]
end

(* Generic function to cook benchmarks for fixed-size encodings *)
let fixed_size_shared :
    ?check:(unit -> unit) ->
    name:string ->
    generator:'a Base_samplers.sampler ->
    make_bench:
      ((unit -> 'a) -> unit -> unit Tezos_benchmark.Generator.benchmark) ->
    unit ->
    Tezos_benchmark.Benchmark.t =
 fun ?(check = fun () -> ()) ~name ~generator ~make_bench () ->
  let free_variable =
    Free_variable.of_string (Format.asprintf "%s_const" name)
  in
  let model =
    Model.make
      ~conv:(fun () -> ())
      ~model:
        (Model.unknown_const2
           ~const1:free_variable
           ~const2:Builtin_benchmarks.timer_variable)
  in
  let codegen =
    Model.make
      ~conv:(fun () -> ())
      ~model:(Model.unknown_const1 ~const:free_variable)
  in
  let module Bench : Benchmark.S = struct
    let name = name

    let info = Format.asprintf "Benchmarking %s" name

    let tags = ["encoding"]

    include Shared_constant_time

    let create_benchmarks ~rng_state ~bench_num () =
      check () ;
      let generator () = generator rng_state in
      List.repeat bench_num (make_bench generator)

    let () = Registration.register_for_codegen name (Model.For_codegen codegen)

    let models = [("encoding", model); ("codegen", codegen)]
  end in
  ((module Bench) : Benchmark.t)

(* Generic function to cook benchmarks for linear-time encodings *)
let linear_shared ?(check = fun () -> ()) ~name ~generator ~make_bench () =
  let const = Free_variable.of_string (Format.asprintf "%s_const" name) in
  let coeff = Free_variable.of_string (Format.asprintf "%s_coeff" name) in
  let model =
    Model.make
      ~conv:(fun {Shared_linear.bytes} -> (bytes, ()))
      ~model:
        (Model.affine_split_const
           ~intercept1:Builtin_benchmarks.timer_variable
           ~intercept2:const
           ~coeff)
  in
  let codegen =
    Model.make
      ~conv:(fun {Shared_linear.bytes} -> (bytes, ()))
      ~model:(Model.affine ~intercept:const ~coeff)
  in
  let module Bench : Benchmark.S = struct
    let name = name

    let info = Format.asprintf "Benchmarking %s" name

    let tags = ["encoding"]

    include Shared_linear

    let create_benchmarks ~rng_state ~bench_num () =
      check () ;
      let generator () = generator rng_state in
      List.repeat bench_num (make_bench generator)

    let () = Registration.register_for_codegen name (Model.For_codegen codegen)

    let models = [("encoding", model); ("codegen", codegen)]
  end in
  ((module Bench) : Benchmark.t)

let make_encode_fixed_size :
    type a.
    ?check:(unit -> unit) ->
    name:string ->
    encoding:a Data_encoding.t ->
    generator:(Random.State.t -> a) ->
    unit ->
    Benchmark.t =
 fun ?check ~name ~encoding ~generator () ->
  fixed_size_shared
    ?check
    ~name
    ~generator
    ~make_bench:(fun generator () ->
      let generated = generator () in
      let closure () =
        ignore (Data_encoding.Binary.to_bytes_exn encoding generated)
      in
      Generator.Plain {workload = (); closure})
    ()

let make_encode_variable_size :
    type a.
    ?check:(unit -> unit) ->
    name:string ->
    encoding:a Data_encoding.t ->
    generator:(Random.State.t -> a * Shared_linear.workload) ->
    unit ->
    Benchmark.t =
 fun ?check ~name ~encoding ~generator ->
  linear_shared ?check ~name ~generator ~make_bench:(fun generator () ->
      let (generated, workload) = generator () in
      let closure () =
        ignore (Data_encoding.Binary.to_bytes_exn encoding generated)
      in
      Generator.Plain {workload; closure})

let make_decode_fixed_size :
    type a.
    ?check:(unit -> unit) ->
    name:string ->
    encoding:a Data_encoding.t ->
    generator:(Random.State.t -> a) ->
    unit ->
    Benchmark.t =
 fun ?check ~name ~encoding ~generator ->
  fixed_size_shared ?check ~name ~generator ~make_bench:(fun generator () ->
      let generated = generator () in
      let encoded = Data_encoding.Binary.to_bytes_exn encoding generated in
      let closure () =
        ignore (Data_encoding.Binary.of_bytes_exn encoding encoded)
      in
      Generator.Plain {workload = (); closure})

let make_decode_variable_size :
    type a.
    ?check:(unit -> unit) ->
    name:string ->
    encoding:a Data_encoding.t ->
    generator:(Random.State.t -> a * Shared_linear.workload) ->
    unit ->
    Benchmark.t =
 fun ?check ~name ~encoding ~generator ->
  linear_shared ?check ~name ~generator ~make_bench:(fun generator () ->
      let (generated, workload) = generator () in
      let encoded = Data_encoding.Binary.to_bytes_exn encoding generated in
      let closure () =
        ignore (Data_encoding.Binary.of_bytes_exn encoding encoded)
      in
      Generator.Plain {workload; closure})

(* Generic functions to cook benchmarks for b58check conversions (used for
   typechecking in readable mode in the protocol...) and byte conversions. *)
let make_encode_fixed_size_to_string :
    type a.
    ?check:(unit -> unit) ->
    name:string ->
    to_string:(a -> string) ->
    generator:(Random.State.t -> a) ->
    unit ->
    Benchmark.t =
 fun ?check ~name ~to_string ~generator ->
  fixed_size_shared ?check ~name ~generator ~make_bench:(fun generator () ->
      let generated = generator () in
      let closure () = ignore (to_string generated) in
      Generator.Plain {workload = (); closure})

(* Exactly the sample implem' as above.*)
let make_encode_fixed_size_to_bytes :
    type a.
    ?check:(unit -> unit) ->
    name:string ->
    to_bytes:(a -> bytes) ->
    generator:(Random.State.t -> a) ->
    unit ->
    Benchmark.t =
 fun ?check ~name ~to_bytes ~generator ->
  fixed_size_shared ?check ~name ~generator ~make_bench:(fun generator () ->
      let generated = generator () in
      let closure () = ignore (to_bytes generated) in
      Generator.Plain {workload = (); closure})

let make_encode_variable_size_to_string :
    type a.
    ?check:(unit -> unit) ->
    name:string ->
    to_string:(a -> string) ->
    generator:(Random.State.t -> a * Shared_linear.workload) ->
    unit ->
    Benchmark.t =
 fun ?check ~name ~to_string ~generator ->
  linear_shared ?check ~name ~generator ~make_bench:(fun generator () ->
      let (generated, workload) = generator () in
      let closure () = ignore (to_string generated) in
      Generator.Plain {workload; closure})

let make_decode_fixed_size_from_string :
    type a.
    ?check:(unit -> unit) ->
    name:string ->
    to_string:(a -> string) ->
    from_string:(string -> a) ->
    generator:(Random.State.t -> a) ->
    unit ->
    Benchmark.t =
 fun ?check ~name ~to_string ~from_string ~generator ->
  fixed_size_shared ?check ~name ~generator ~make_bench:(fun generator () ->
      let generated = generator () in
      let string = to_string generated in
      let closure () = ignore (from_string string) in
      Generator.Plain {workload = (); closure})

let make_decode_fixed_size_from_bytes :
    type a.
    ?check:(unit -> unit) ->
    name:string ->
    to_bytes:(a -> bytes) ->
    from_bytes:(bytes -> a) ->
    generator:(Random.State.t -> a) ->
    unit ->
    Benchmark.t =
 fun ?check ~name ~to_bytes ~from_bytes ~generator ->
  fixed_size_shared ?check ~name ~generator ~make_bench:(fun generator () ->
      let generated = generator () in
      let bytes = to_bytes generated in
      let closure () = ignore (from_bytes bytes) in
      Generator.Plain {workload = (); closure})

let make_decode_variable_size_from_string :
    type a.
    ?check:(unit -> unit) ->
    name:string ->
    to_string:(a -> string) ->
    from_string:(string -> a) ->
    generator:(Random.State.t -> a * Shared_linear.workload) ->
    unit ->
    Benchmark.t =
 fun ?check ~name ~to_string ~from_string ~generator ->
  linear_shared ?check ~name ~generator ~make_bench:(fun generator () ->
      let (generated, workload) = generator () in
      let string = to_string generated in
      let closure () = ignore (from_string string) in
      Generator.Plain {workload; closure})

let make_decode_variable_size_from_bytes :
    type a.
    ?check:(unit -> unit) ->
    name:string ->
    to_bytes:(a -> bytes) ->
    from_bytes:(bytes -> a) ->
    generator:(Random.State.t -> a * Shared_linear.workload) ->
    unit ->
    Benchmark.t =
 fun ?check ~name ~to_bytes ~from_bytes ~generator ->
  linear_shared ?check ~name ~generator ~make_bench:(fun generator () ->
      let (generated, workload) = generator () in
      let string = to_bytes generated in
      let closure () = ignore (from_bytes string) in
      Generator.Plain {workload; closure})
