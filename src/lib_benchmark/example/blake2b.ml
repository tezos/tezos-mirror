(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Tezos_benchmark

(* A full example: benchmarking blake2b hashes *)

(*
  First step: defining the benchmark.
*)

let ns = Namespace.make Namespace.root "example"

let fv s = Free_variable.of_namespace (ns s)

let name = ns "Blake2b_example"

let model_blake2b =
  Model.affine
    ~name
    ~intercept:(fv "blake2b_const")
    ~coeff:(fv "blake2b_ns_p_byte")

module Blake2b_bench : Benchmark.S = struct
  let name = name

  let info = "Illustrating tezos-benchmark by benchmarking blake2b"

  let module_filename = __FILE__

  let generated_code_destination = None

  let tags = ["example"]

  (* We will measure hashing time on random bytes with length
     sampled uniformly between 1 and [max_bytes] length. *)
  type config = {max_bytes : int}

  let default_config = {max_bytes = 1 lsl 16}

  (* The encoding is used by `tezos-snoop` to load the config from json
     files. *)
  let config_encoding =
    let open Data_encoding in
    conv
      (fun {max_bytes} -> max_bytes)
      (fun max_bytes -> {max_bytes})
      (obj1 (req "max_bytes" int31))

  (* The only relevant piece of information is the nbytes of the bytes being
     benchmarked. *)
  type workload = {nbytes : int}

  (* The workload encoding is used to save and load workloads in binary format. *)
  let workload_encoding =
    let open Data_encoding in
    conv
      (fun {nbytes} -> nbytes)
      (fun nbytes -> {nbytes})
      (obj1 (req "nbytes" int31))

  (* How to interpret a workload as a vector; for automatic scatter-plotting. *)
  let workload_to_vector {nbytes} =
    Sparse_vec.String.of_list [("nbytes", float_of_int nbytes)]

  (* We posit that the execution time of blake2b is proportional to the
     length of the bytes. *)
  let models =
    [
      ( "blake2b",
        Model.make ~conv:(fun {nbytes} -> (nbytes, ())) ~model:model_blake2b );
    ]

  let blake2b_benchmark rng_state config () =
    let nbytes =
      Base_samplers.sample_in_interval
        rng_state
        ~range:{min = 1; max = config.max_bytes}
    in
    let bytes = Base_samplers.uniform_bytes rng_state ~nbytes in
    let workload = {nbytes} in
    (* The closure here is the piece of code to be benchmarked. *)
    let closure () = ignore (Tezos_crypto.Blake2B.hash_bytes [bytes]) in
    Generator.Plain {workload; closure}

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (blake2b_benchmark rng_state config)
end

let () = Registration.register (module Blake2b_bench)
